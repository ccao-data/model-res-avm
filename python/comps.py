import typing

import numpy as np
import numba as nb
import pandas as pd
import taichi as ti

# Initialize taichi
ti.init(arch=ti.gpu, default_ip=ti.i32, default_fp=ti.f32)


def get_comps(
    observation_df,
    comparison_df,
    weights,
    num_comps=20,
    num_price_bins=10,
):
    """Fast algorithm to get the top `num_comps` comps from a dataframe of lightgbm
    leaf node assignments (`observation_df`) compared to a second dataframe of
    assignments (`comparison_df`). Leaf nodes are weighted according to a tree
    importance matrix `weights` and used to generate a similarity score and
    return two dataframes, one a set of indices and the other a set of scores
    for the `n` most similar comparables. More details on the underlying
    algorithm here: https://ccao-data.github.io/lightsnip/articles/finding-comps.html

    The function expects that `observation_df` and `comparison_df` will both
    have a column called `predicted_value`. These two columns represent the
    (integer) predicted value for the observations and the comparisons.
    These columns are then used along with the `num_price_bins` parameter to bin
    the comparison data and only compare observations to comparisons that are in
    the three closest bins to the observation.
    """
    # Convert the weights to a numpy array so that we can take advantage of
    # numba acceleration later on
    weights_matrix = np.asarray(weights, dtype=np.float32)

    # Add ID columns so that we can keep track of the initial position of
    # each row as we sort them. This is necessary to allow the caller to
    # translate results back to parcels based on row index
    observation_df["id"] = list(range(len(observation_df)))
    comparison_df["id"] = list(range(len(comparison_df)))

    # Sort the comparison data and extract the indexes of rows that represent
    # boundaries between price bins. We'll use these bin indexes to
    # reduce the number of comparison parcels that we need to search for each
    # observation
    sorted_comparison_df = comparison_df.sort_values(
        ["predicted_value"]
    ).reset_index(
        drop=True
    )
    sorted_comparison_df["price_bin"] = pd.qcut(
        sorted_comparison_df["predicted_value"],
        num_price_bins,
        labels=np.arange(0, num_price_bins),
    )

    # Extract price bin metadata from the binned comparison data
    price_bin_indices = sorted_comparison_df.groupby(
        "price_bin", observed=False  # Silence deprecated `observed` warning
    ).apply(
        lambda x: x.index.tolist()
    ).apply(
        lambda x: [x[0], x[-1]]
    )
    bin_argmin = [idx[0] for idx in price_bin_indices.values]
    bin_argmax = [idx[1] for idx in price_bin_indices.values]
    price_bin_indices = pd.DataFrame(
        {
            "id": price_bin_indices.index.astype(int),
            "argmin": bin_argmin,
            "argmax": bin_argmax,
            "min": sorted_comparison_df["predicted_value"][bin_argmin].values,
            "max": sorted_comparison_df["predicted_value"][bin_argmax].values
        },
    )

    # Fudge the minimum/maximum values in the price bin range so that they
    # approximate infinity. This is necessary because observation data may
    # have observed values that are lower than the minimum value in the
    # comparison data, or higher than the maximum value
    price_bin_indices.at[0, "min"] = np.iinfo(np.int32).min + 1
    price_bin_indices.at[num_price_bins - 1, "max"] = np.iinfo(np.int32).max - 1

    # Update bins to ensure they have no gaps. This enables us to put values
    # from the observation dataframe into these bins, since otherwise
    # values in the observation dataframe might be in between the maximum
    # and minimum values of two adjacent bins
    for bin_idx in range(len(price_bin_indices)):
        # Skip the first bin, since its min val should aways be the smallest
        # possible int
        if bin_idx > 0:
            price_bin_indices.at[bin_idx, "min"] = (
                price_bin_indices["max"][bin_idx - 1] + 1
            )

    # Place observations in bins. Do this in a numba-accelerated function so
    # that we can make use of fast loops
    observation_matrix = observation_df[["id", "predicted_value"]].values
    taichi_obs_ndarray = ti.ndarray(dtype=int, shape=observation_matrix.shape)
    taichi_obs_ndarray.from_numpy(observation_matrix),

    price_bin_matrix = price_bin_indices.values
    taichi_bin_ndarray = ti.ndarray(dtype=int, shape=price_bin_matrix.shape)
    taichi_bin_ndarray.from_numpy(price_bin_matrix),

    num_observations = observation_matrix.shape[0]
    num_price_bins = price_bin_matrix.shape[0]

    # Output vector
    binned_vector = ti.ndarray(dtype=int, shape=(num_observations, 1))

    _bin_by_price(
        taichi_obs_ndarray,
        taichi_bin_ndarray,
        binned_vector,
        num_observations,
        num_price_bins
    )

    observation_df["price_bin"] = binned_vector.to_numpy()

    total_num_observations = len(observation_df)
    total_num_possible_comps = len(sorted_comparison_df)
    binned_ids, binned_scores = [], []
    for bin_idx, bin in price_bin_indices.iterrows():
        observations = observation_df[observation_df["price_bin"] == bin["id"]]

        if observations.empty:
            print(
                f"No observations in bin {bin['id'] + 1}; skipping",
                # Flush statement to stdout so that reticulate will print it
                # in real time
                flush=True
            )
            continue

        observation_matrix = observations.drop(
          columns=["id", "predicted_value", "price_bin"]
        ).values

        # Add a 1-bin buffer on either side in case an observation is close to
        # a bin edge. In addition, make sure that argmax is inclusive by
        # expanding the range by 1
        argmin, argmax = bin["argmin"], bin["argmax"] + 1
        price_min, price_max = bin["min"], bin["max"]
        if bin_idx > 0:
            prev_bin = price_bin_indices.iloc[bin_idx - 1]
            argmin, price_min = prev_bin["argmin"], prev_bin["min"]
        if bin_idx < len(price_bin_indices) - 1:
            next_bin = price_bin_indices.iloc[bin_idx + 1]
            argmax, price_max = next_bin["argmax"] + 1, next_bin["max"]

        possible_comps = sorted_comparison_df[argmin:argmax]
        comp_idx_to_id = dict(
            zip(
                possible_comps.reset_index(drop=True).index,
                possible_comps['id']
            )
        )
        # Handle -1, which is an indicator of no match
        comp_idx_to_id[-1] = -1
        possible_comp_matrix = possible_comps.drop(
          columns=["id", "predicted_value", "price_bin"]
        ).values

        print(
            (
                f"Getting top {num_comps} comps for price bin {bin['id'] + 1}/"
                f"{len(price_bin_indices)} (${price_min:,} to ${price_max:,}) - "
                f"{len(observations)}/{total_num_observations} observations, "
                f"{len(possible_comps)}/{total_num_possible_comps} possible comps"
            ),
            flush=True
        )

        num_observations = len(observation_matrix)
        num_possible_comparisons = len(possible_comp_matrix)

        # Store scores and indexes in two separate arrays rather than a 3d matrix
        # for simplicity (array of tuples does not convert to pandas properly).
        comp_ids =  ti.ndarray(dtype=int, shape=(num_observations, num_comps))
        comp_scores =  ti.ndarray(dtype=float, shape=(num_observations, num_comps))

        # Indexes default to -1, which is an impossible index and so is a signal
        # that no comp was found
        comp_ids.fill(-1)

        num_trees = observation_matrix.shape[1]

        _get_top_n_comps(
            observation_matrix,
            possible_comp_matrix,
            weights_matrix,
            comp_ids,
            comp_scores,
            num_comps,
            num_observations,
            num_possible_comparisons,
            num_trees,
        )

        # Match comp and observation IDs back to the original dataframes since
        # we have since rearranged them
        comp_ids = comp_ids.to_numpy()
        comp_scores = comp_scores.to_numpy()

        matched_comp_ids = np.vectorize(comp_idx_to_id.get)(comp_ids)
        observation_ids = observations["id"].values

        for obs_idx, comp_idx, comp_score in zip(
            observation_ids, matched_comp_ids, comp_scores
        ):
            binned_ids.append((obs_idx, comp_idx))
            binned_scores.append((obs_idx, comp_score))

    # Sort the IDs and comps according to the original order of the input
    # data so that the output data has the same order
    sorted_binned_ids = [
        binned_idx[1]
        for binned_idx in sorted(
            binned_ids, key=lambda binned_idx: binned_idx[0]
        )
    ]
    sorted_binned_scores = [
        binned_score[1]
        for binned_score in sorted(
            binned_scores, key=lambda binned_score: binned_score[0]
        )
    ]

    # Turn the comps matrices into pandas dataframes to match the type of
    # the input data
    indexes_df = pd.DataFrame(
        np.asarray(sorted_binned_ids),
        columns=[f"comp_idx_{idx}" for idx in range(1, num_comps + 1)]
    )
    scores_df = pd.DataFrame(
        np.asarray(sorted_binned_scores),
        columns=[f"comp_score_{idx}" for idx in range(1, num_comps + 1)]
    )

    return indexes_df, scores_df


@ti.kernel
def _bin_by_price(
    observation_matrix: ti.types.ndarray(),
    price_bin_matrix: ti.types.ndarray(),
    output_vector: ti.types.ndarray(),
    num_observations: int,
    num_price_bins: int
):
    """Given a matrix of observations and a matrix of price bins, place the
    observations in the closest price bin and return an array of bin IDs
    with the same length as the observation matrix."""
    price_bin_idx, price_bin_min_idx, price_bin_max_idx = 0, 3, 4
    observation_price_idx = 1

    for obs_idx in range(num_observations):
        observation_price = observation_matrix[obs_idx, observation_price_idx]
        bin_found = False
        for bin_idx in range(num_price_bins):
            bin_min = price_bin_matrix[bin_idx, price_bin_min_idx]
            bin_max = price_bin_matrix[bin_idx, price_bin_max_idx]
            bin_id = price_bin_matrix[bin_idx, price_bin_idx]

            if (
                # Since we expect the price bins to be non-overlapping with
                # no gaps and an integer difference of 1 between ranges, the
                # ranges can be treated as inclusive on both ends
                observation_price >= bin_min and observation_price <= bin_max
            ):
                output_vector[obs_idx, 0] = bin_id
                bin_found = True
                break

        if not bin_found:
            # Set a special value to indicate an error, since taichi doesn't
            # support runtime errors
            output_vector[obs_idx, 0] = -1


@ti.kernel
def _get_top_n_comps(
    leaf_node_matrix: ti.types.ndarray(),
    comparison_leaf_node_matrix: ti.types.ndarray(),
    weights_matrix: ti.types.ndarray(),
    all_top_n_idxs: ti.types.ndarray(),
    all_top_n_scores: ti.types.ndarray(),
    num_comps: int,
    num_observations: int,
    num_possible_comparisons: int,
    num_trees: int
):
    """Helper function that takes matrices of leaf node assignments for
    observations in a tree model, a matrix of weights for each obs/tree, and an
    integer `num_comps`, and returns a matrix where each observation is scored
    by similarity to observations in the comparison matrix and the top N scores
    are returned along with the indexes of the comparison observations."""
    for x_i in range(num_observations):
        for y_i in range(num_possible_comparisons):
            similarity_score = 0.0
            for tree_idx in range(num_trees):
                similarity_score += (
                    weights_matrix[x_i, tree_idx] * (
                        leaf_node_matrix[x_i, tree_idx] ==
                        comparison_leaf_node_matrix[y_i, tree_idx]
                    )
                )

            # See if the score is higher than any of the top N
            # comps, and store it in the sorted comps array if it is.
            # First check if the score is higher than the lowest score,
            # since otherwise we don't need to bother iterating the scores
            if similarity_score > all_top_n_scores[x_i, num_comps - 1]:
              for idx in range(num_comps):
                if similarity_score > all_top_n_scores[x_i, idx]:
                    # Shift scores and indices to make room for the new one.
                    # This requires iterating the indices backwards; since
                    # taichi doesn't support the `step` parameter in `range()`
                    # calls the way that Python does, we need to recreate
                    # it with other primitives
                    for i in range(num_comps - 1, idx):
                        all_top_n_scores[x_i, i] = all_top_n_scores[x_i, i - 1]
                        all_top_n_idxs[x_i, i] = all_top_n_idxs[x_i, i - 1]

                    # Insert the new score and index at the correct position
                    all_top_n_scores[x_i, idx] = similarity_score
                    all_top_n_idxs[x_i, idx] = y_i
                    break


@ti.func
def _insert_at_coord_and_shift(ndarr, x, y, elem, max_len):
    """Helper function to insert an element `elem` into a sorted numpy array `arr`
    at a given (x, y) coordinate and shift the subsequent elements down one
    index, with a maximum of `max_len` elements."""
    for i in range(max_len - 1, y, -1):
        ndarr[x, i] = ndarr[x, i - 1]
    ndarr[x, y] = elem


if __name__ == "__main__":
    # When this module is run as a script, the following code will test
    # performance of the script and print the run time to the console
    import time

    num_trees = 500
    num_obs = 20001
    num_comparisons = 10000
    mean_sale_price = 350000
    std_deviation = 110000

    leaf_nodes = pd.DataFrame(
        np.random.randint(0, num_obs, size=[num_obs, num_trees])
    )
    leaf_nodes["predicted_value"] = np.random.normal(
        mean_sale_price, std_deviation, size=num_obs
    ).astype(int)

    training_leaf_nodes = pd.DataFrame(
        np.random.randint(0, num_comparisons, size=[num_comparisons, num_trees])
    )
    training_leaf_nodes["predicted_value"] = np.random.normal(
        mean_sale_price, std_deviation, size=num_comparisons
    ).astype(int)
    tree_weights = np.asarray([
      np.random.dirichlet(np.ones(num_trees))
      for _ in range(num_comparisons)
    ])

    start = time.time()
    get_comps(leaf_nodes, training_leaf_nodes, tree_weights)
    end = time.time()
    print(f"get_comps runtime: {end - start}s")

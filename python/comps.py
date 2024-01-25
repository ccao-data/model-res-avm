import numpy as np
import numba as nb
import pandas as pd

# The number of bins to create for when binning data by sale price; e.g. if
# this value is 10, the data will be binned into deciles
NUM_PRICE_BINS = 30


def get_comps(
    observation_df,
    comparison_df,
    weights,
    n=20,
    num_price_bins=NUM_PRICE_BINS,
):
    """Fast algorithm to get the top `n` comps from a dataframe of lightgbm
    leaf node assignments (`observation_df`) compared to a second dataframe of
    assignments (`comparison_df`). Leaf nodes are weighted according to a tree
    importance vector `weights` and used to generate a similarity score and
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
    weights_arr = np.asarray(weights, dtype=np.float64)

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
        ["meta_sale_price"]
    ).reset_index(
        drop=True
    )
    sorted_comparison_df["price_bin"] = pd.qcut(
        sorted_comparison_df["meta_sale_price"],
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
            "min": sorted_comparison_df["meta_sale_price"][bin_argmin].values,
            "max": sorted_comparison_df["meta_sale_price"][bin_argmax].values
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
    observation_df["price_bin"] = _bin_by_price(
        observation_df[["id", "pred_pin_final_fmv"]].values,
        price_bin_indices.values
    )

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
          columns=["id", "pred_pin_final_fmv", "price_bin"]
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
          columns=["id", "meta_sale_price", "price_bin"]
        ).values

        print(
            (
                f"Getting top {n} comps for price bin {bin['id'] + 1}/"
                f"{len(price_bin_indices)} (${price_min:,} to ${price_max:,}) - "
                f"{len(observations)}/{total_num_observations} observations, "
                f"{len(possible_comps)}/{total_num_possible_comps} possible comps"
            ),
            flush=True
        )

        comp_ids, comp_scores = _get_top_n_comps(
            observation_matrix, possible_comp_matrix, weights_arr, n
        )

        # Match comp and observation IDs back to the original dataframes since
        # we have since rearranged them
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
        columns=[f"comp_idx_{idx}" for idx in range(1, n+1)]
    )
    scores_df = pd.DataFrame(
        np.asarray(sorted_binned_scores),
        columns=[f"comp_score_{idx}" for idx in range(1, n+1)]
    )

    return indexes_df, scores_df


@nb.njit(fastmath=True, parallel=True)
def _bin_by_price(observation_matrix, price_bin_matrix):
    """Given a matrix of observations and a matrix of price bins, place the
    observations in the closest price bin and return an array of bin IDs
    with the same length as the observation matrix."""
    num_observations = len(observation_matrix)
    price_bin_idx, price_bin_min_idx, price_bin_max_idx = 0, 3, 4
    observation_price_idx = 1
    output_matrix = np.zeros(num_observations, dtype=np.int64)

    for obs_idx in nb.prange(num_observations):
        observation = observation_matrix[obs_idx]
        observation_price = observation[observation_price_idx]
        for bin in price_bin_matrix:
            if (
                # Since we expect the price bins to be non-overlapping with
                # no gaps and an integer difference of 1 between ranges, the
                # ranges can be treated as inclusive on both ends
                observation_price >= bin[price_bin_min_idx] and
                observation_price <= bin[price_bin_max_idx]
            ):
                output_matrix[obs_idx] = bin[price_bin_idx]
                break
        else:
            raise ValueError(
                f"Observation {obs_idx} did not match any price bins"
            )

    return output_matrix


@nb.njit(fastmath=True, parallel=True)
def _get_top_n_comps(leaf_node_matrix, comparison_leaf_node_matrix, weights, n):
    """Helper function that takes matrices of leaf node assignments for
    observations in a tree model, an array of weights for each tree, and an
    integer N, and returns a matrix where each observation is scored by
    similarity to observations in the comparison matrix and the top N scores
    are returned along with the indexes of the comparison observations."""
    num_observations = len(leaf_node_matrix)
    num_comparisons = len(comparison_leaf_node_matrix)
    weights = weights.T
    idx_dtype = np.int64
    score_dtype = np.float64

    # Store scores and indexes in two separate arrays rather than a 3d matrix
    # for simplicity (array of tuples does not convert to pandas properly).
    # Indexes default to -1, which is an impossible index and so is a signal
    # that no comp was found
    all_top_n_idxs = np.full((num_observations, n), -1, dtype=idx_dtype)
    all_top_n_scores = np.zeros((num_observations, n), dtype=score_dtype)

    for x_i in nb.prange(num_observations):
        top_n_idxs = np.full(n, -1, dtype=idx_dtype)
        top_n_scores = np.zeros(n, dtype=score_dtype)

        # TODO: We could probably speed this up by skipping comparisons we've
        # already made; we just need to do it in a way that will have a
        # low memory footprint
        for y_i in range(num_comparisons):
            similarity_score = 0.0
            for tree_idx in range(len(leaf_node_matrix[x_i])):
                similarity_score += (
                    weights[tree_idx] * (
                        leaf_node_matrix[x_i][tree_idx] ==
                        comparison_leaf_node_matrix[y_i][tree_idx]
                    )
                )

            # See if the score is higher than any of the top N
            # comps, and store it in the sorted comps array if it is.
            # First check if the score is higher than the lowest score,
            # since otherwise we don't need to bother iterating the scores
            if similarity_score > top_n_scores[-1]:
              for idx, score in enumerate(top_n_scores):
                if similarity_score > score:
                  top_n_idxs = _insert_at_idx_and_shift(
                    top_n_idxs, y_i, idx
                  )
                  top_n_scores = _insert_at_idx_and_shift(
                    top_n_scores, similarity_score, idx
                  )
                  break

        all_top_n_idxs[x_i] = top_n_idxs
        all_top_n_scores[x_i] = top_n_scores

    return all_top_n_idxs, all_top_n_scores


@nb.njit(fastmath=True)
def _insert_at_idx_and_shift(arr, elem, idx):
  """Helper function to insert an element `elem` into a sorted numpy array `arr`
  at a given index `idx` and shift the subsequent elements down one index."""
  return np.concatenate((
    arr[:idx], np.array([(elem)], dtype=arr.dtype), arr[idx:-1]
  ))


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
    leaf_nodes["pred_pin_final_fmv"] = np.random.normal(
        mean_sale_price, std_deviation, size=num_obs
    ).astype(int)

    training_leaf_nodes = pd.DataFrame(
        np.random.randint(0, num_comparisons, size=[num_comparisons, num_trees])
    )
    training_leaf_nodes["meta_sale_price"] = np.random.normal(
        mean_sale_price, std_deviation, size=num_comparisons
    ).astype(int)
    tree_weights = np.random.dirichlet(np.ones(num_trees))

    start = time.time()
    get_comps(leaf_nodes, training_leaf_nodes, tree_weights)
    end = time.time()
    print(f"get_comps runtime: {end - start}s")

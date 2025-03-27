import typing

import numba as nb
import numpy as np
import pandas as pd


def get_comps(
    observation_df: pd.DataFrame,
    comparison_df: pd.DataFrame,
    weights: np.ndarray,
    num_comps: int = 5,
    num_chunks: int = 10,
) -> typing.Tuple[pd.DataFrame, pd.DataFrame]:
    """
    Fast algorithm to get the top `num_comps` comps from a dataframe of
    lightgbm leaf node assignments (`observation_df`) compared to a second
    dataframe of leaf node assignments (`comparison_df`).

    Leaf nodes are weighted according to a tree importance matrix `weights`
    and used to generate a similarity score. The function returns two
    dataframes: One containing the indices of the most similar compararables
    and the other containing their corresponding similarity scores.

    More details on the underlying algorithm can be found here:
    https://ccao-data.github.io/lightsnip/articles/finding-comps.html

    Args:
        observation_df (pandas.DataFrame):
            DataFrame containing leaf node assignments for observations.
        comparison_df (pandas.DataFrame):
            DataFrame containing leaf node assignments for potential
            comparables.
        weights (numpy.ndarray):
            Importance weights for leaf nodes, used to compute similarity
            scores.
        num_comps (int, optional):
            Number of top comparables to return for each observation.
            Default is 5.
        num_chunks (int, optional):
            Number of chunks to split observations for progress reporting.
            Default is 10.

    Returns:
        tuple:
            - pd.DataFrame:
                DataFrame containing the indices of the `num_comps`
                most similar comparables in `comparison_df`. The order of
                rows will match the order of rows in `observation_df`.
            - pd.DataFrame:
                DataFrame containing similarity scores for the `num_comps`
                most similar comparables. The order of rows will match the
                order of rows in `observation_df`.
    """
    # Check to make sure the shape of the input matrices is correct
    if observation_df.shape[1] != comparison_df.shape[1]:
        raise ValueError(
            "Number of columns in `observation_df` "
            f"({observation_df.shape[1]}) "
            f"must match `comparison_df` ({comparison_df.shape[1]})"
        )
    if comparison_df.shape != weights.shape:
        raise ValueError(
            f"`comparison_df.shape` {comparison_df.shape} must match "
            f"`weights.shape` {weights.shape}"
        )

    # Convert the weights to a numpy array so that we can take advantage of
    # numba acceleration later on
    weights_matrix = np.asarray(weights, dtype=np.float32)

    # Chunk the observations so that the script can periodically report progress
    observation_df["chunk"] = pd.cut(
        observation_df.index, bins=num_chunks, labels=False
    )

    total_num_observations = len(observation_df)
    total_num_possible_comps = len(comparison_df)
    chunked_ids, chunked_scores = [], []
    for chunk_num in observation_df["chunk"].unique():
        observations = observation_df[observation_df["chunk"] == chunk_num]
        # Drop chunk column to produce a matrix that we can accelerate
        # with numba
        observation_matrix = observations.drop(columns=["chunk"]).values

        # Produce a numba-compatible matrix for the comparisons
        possible_comp_matrix = comparison_df.values

        print(
            (
                f"Getting top {num_comps} comps for chunk {chunk_num}/"
                f"{num_chunks} - "
                f"{len(observations)}/{total_num_observations} observations, "
                f"{total_num_possible_comps} possible comps"
            ),
            # Flush so that we can print to the console in realtime when
            # reticulate runs this function in an R context
            flush=True,
        )

        # Compute comps for each observation
        comp_ids, comp_scores = _get_top_n_comps(
            observation_matrix, possible_comp_matrix, weights_matrix, num_comps
        )

        observation_ids = observations.index.values
        for obs_idx, comp_idx, comp_score in zip(
            observation_ids, comp_ids, comp_scores
        ):
            chunked_ids.append((obs_idx, comp_idx))
            chunked_scores.append((obs_idx, comp_score))

    # Turn the comps matrices into pandas dataframes to match the type of
    # the input data
    indexes_df = pd.DataFrame(
        # We don't need the observation ID, since the output should be in the
        # same order as the input
        np.asarray([chunked_id[1] for chunked_id in chunked_ids]),
        columns=[f"comp_idx_{idx}" for idx in range(1, num_comps + 1)],
    )
    scores_df = pd.DataFrame(
        np.asarray([chunked_score[1] for chunked_score in chunked_scores]),
        columns=[f"comp_score_{idx}" for idx in range(1, num_comps + 1)],
    )

    return indexes_df, scores_df


@nb.njit(fastmath=True, parallel=True)
def _get_top_n_comps(
    leaf_node_matrix: np.ndarray,
    comparison_leaf_node_matrix: np.ndarray,
    weights_matrix: np.ndarray,
    num_comps: int,
) -> typing.Tuple[np.ndarray, np.ndarray]:
    """Helper function that takes matrices of leaf node assignments for
    observations in a tree model, a matrix of weights for each obs/tree, and an
    integer `num_comps`, and returns a matrix where each observation is scored
    by similarity to observations in the comparison matrix and the top N scores
    are returned along with the indexes of the comparison observations."""
    num_observations = len(leaf_node_matrix)
    num_possible_comparisons = len(comparison_leaf_node_matrix)
    idx_dtype = np.int32
    score_dtype = np.float32

    # Store scores and indexes in two separate arrays rather than a 3d matrix
    # for simplicity (array of tuples does not convert to pandas properly).
    # Indexes default to -1, which is an impossible index and so is a signal
    # that no comp was found
    all_top_n_idxs = np.full((num_observations, num_comps), -1, dtype=idx_dtype)
    all_top_n_scores = np.zeros((num_observations, num_comps), dtype=score_dtype)

    for x_i in nb.prange(num_observations):
        # TODO: We could probably speed this up by skipping comparisons we've
        # already made; we just need to do it in a way that will have a
        # low memory footprint
        for y_i in range(num_possible_comparisons):
            similarity_score = 0.0
            for tree_idx in range(len(leaf_node_matrix[x_i])):
                if (
                    leaf_node_matrix[x_i][tree_idx]
                    == comparison_leaf_node_matrix[y_i][tree_idx]
                ):
                    similarity_score += weights_matrix[y_i][tree_idx]

            # See if the score is higher than any of the top N
            # comps, and store it in the sorted comps array if it is.
            # First check if the score is higher than the lowest score,
            # since otherwise we don't need to bother iterating the scores
            if similarity_score > all_top_n_scores[x_i][-1]:
                for idx, score in enumerate(all_top_n_scores[x_i]):
                    if similarity_score > score:
                        insert_at_idx_and_shift(all_top_n_idxs[x_i], y_i, idx)
                        insert_at_idx_and_shift(
                            all_top_n_scores[x_i], similarity_score, idx
                        )
                        break

    return all_top_n_idxs, all_top_n_scores


@nb.njit(fastmath=True)
def insert_at_idx_and_shift(
    arr: np.ndarray, elem: typing.Union[int, float], idx: int
) -> np.ndarray:
    """Helper function to insert an element `elem` into a sorted numpy array `arr`
    at a given index `idx` and shift the subsequent elements down one index."""
    arr[idx + 1 :] = arr[idx:-1]
    arr[idx] = elem
    return arr


if __name__ == "__main__":
    # When this module is run as a script, the following code will test
    # performance of the script and print the run time to the console
    import time

    num_trees = 500
    num_obs = 20001
    num_comparisons = 10000
    mean_sale_price = 350000
    std_deviation = 110000

    leaf_nodes = pd.DataFrame(np.random.randint(0, num_obs, size=[num_obs, num_trees]))
    training_leaf_nodes = pd.DataFrame(
        np.random.randint(0, num_comparisons, size=[num_comparisons, num_trees])
    )
    tree_weights = np.asarray(
        [np.random.dirichlet(np.ones(num_trees)) for _ in range(num_comparisons)]
    )

    start = time.time()
    get_comps(leaf_nodes, training_leaf_nodes, tree_weights)
    end = time.time()
    print(f"get_comps runtime: {end - start}s")

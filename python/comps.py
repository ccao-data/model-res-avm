import numpy as np
import numba as nb
import pandas as pd


def get_comps(leaf_node_df, comparison_leaf_node_df, weights, n=20):
    """Fast algorithm to get the top `n` comps from a dataframe of lightgbm
    leaf node assignments (`leaf_node_df`), weighted according to a tree
    importance vector `weights`. More details on the underlying algorithm here:
    https://ccao-data.github.io/lightsnip/articles/finding-comps.html
    """
    # Convert the input dataframes and lists to numpy arrays
    # so that we can take advantage of numba acceleration
    leaf_node_matrix = leaf_node_df.values
    comparison_leaf_node_matrix = comparison_leaf_node_df.values
    weights_arr = np.asarray(weights, dtype=np.float64)

    # Chunk the input matrix so that we can report on progress between
    # processing each chunk. This is necessary to do outside of the main
    # computation loop because compiled numba code does not support flushing
    # print statements to stdout, which is necessary in order for reticulate
    # (which runs this code) to log print statements in real time
    num_leaf_node_chunks = 100
    leaf_node_chunks = np.array_split(leaf_node_matrix, num_leaf_node_chunks)
    index_chunks, score_chunks = [], []

    # Get the indexes and scores of the top N comps
    for chunk_idx, leaf_node_chunk in enumerate(leaf_node_chunks, 1):
        print(
            (
                f"Getting top {n} comps for leaf node chunk {chunk_idx} of "
                f"{num_leaf_node_chunks} "
                f"({leaf_node_chunk.shape[0]} observations)"
            ),
            # Flush statement to stdout so that reticulate will print it
            # in real time
            flush=True
        )
        index_chunk, score_chunk = _get_top_n_comps(
            leaf_node_chunk, comparison_leaf_node_matrix, weights_arr, n
        )
        index_chunks.append(index_chunk)
        score_chunks.append(score_chunk)

    # Turn the comps matrices into pandas dataframes to match the input
    indexes_df = pd.DataFrame(
        np.concatenate(index_chunks),
        columns=[f"comp_idx_{idx}" for idx in range(1, n+1)]
    )
    scores_df = pd.DataFrame(
        np.concatenate(score_chunks),
        columns=[f"comp_score_{idx}" for idx in range(1, n+1)]
    )

    # Replace 0s with NaNs if they represent null comps (i.e. they are 0 in
    # both the indexes and scores dataframes)
    indexes_df.iloc[
        # Scores are 64-bit floating points, so check if they are close to 0
        np.where((np.isclose(scores_df, 0, atol=1e-12)) & (indexes_df == 0))
    ] = pd.NA
    scores_df.iloc[np.where(pd.isna(indexes_df))] = pd.NA

    return indexes_df, scores_df


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
    # for simplicity (array of tuples does not convert to pandas properly)
    all_top_n_idxs = np.zeros((num_observations, n), dtype=idx_dtype)
    all_top_n_scores = np.zeros((num_observations, n), dtype=score_dtype)

    for x_i in nb.prange(num_observations):
        top_n_idxs = np.zeros(n, dtype=idx_dtype)
        top_n_scores = np.zeros(n, dtype=score_dtype)

        # TODO: We could probably speed this up by skipping comparisons we've
        # already made; we just need to do it in a way that will have a
        # low memory footprint
        for y_i in range(num_comparisons):
            similarity_score = 0.0
            for tree_idx in range(len(leaf_node_matrix[x_i])):
                similarity_score += (
                    weights[0, tree_idx] * (
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

    leaf_nodes = pd.DataFrame(
        np.random.randint(0, num_obs, size=[num_obs, num_trees])
    )
    training_leaf_nodes = pd.DataFrame(
        np.random.randint(0, num_comparisons, size=[num_comparisons, num_trees])
    )
    tree_weights = np.random.dirichlet(np.ones(num_trees), size=1).T

    start = time.time()
    get_comps(leaf_nodes, training_leaf_nodes, tree_weights)
    end = time.time()
    print(f"get_comps runtime: {end - start}s")

import numpy as np
import numba as nb
import pandas as pd


def get_comps(leaf_node_df, weights, n=20):
    """Fast algorithm to get the top `n` comps from a dataframe of lightgbm
    leaf node assignments (`leaf_node_df`), weighted according to a tree
    importance vector `weights`. More details on the underlying algorithm here:
    https://ccao-data.github.io/lightsnip/articles/finding-comps.html
    """
    # Convert the input dataframe to a matrix and the weights list to an array
    # so that we can take advantage of numba acceleration
    leaf_node_matrix = leaf_node_df.values
    weights_arr = np.asarray(weights, dtype=np.float64)

    # Get the indexes and scores of the top N comps
    indexes, scores = _get_top_n_comps(leaf_node_matrix, weights_arr, n)

    # Turn the comps matrices into pandas dataframes to match the input
    indexes_df = pd.DataFrame(
        indexes,
        columns=[f"comp_idx_{idx}" for idx in range(1, n+1)]
    )
    scores_df = pd.DataFrame(
        scores,
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


@nb.njit(fastmath=True)
def _get_top_n_comps(leaf_node_matrix, weights, n):
    """Helper function that takes a matrix of leaf node assignments for each
    observation in a tree model, an array of weights for each tree, and an
    integer N, and returns a matrix where each observation is scored by
    similarity to every other observation and the top N scores are returned
    in a tuple along with the index of the observation."""
    num_observations = len(leaf_node_matrix)
    idx_dtype = np.int64
    score_dtype = np.float64

    # Store scores and indexes in two separate arrays rather than a 3d matrix
    # for simplicity (array of tuples does not convert to pandas properly)
    all_top_n_idxs = np.zeros((num_observations, n), dtype=idx_dtype)
    all_top_n_scores = np.zeros((num_observations, n), dtype=score_dtype)

    for x_i in range(num_observations):
        top_n_idxs = np.zeros(n, dtype=idx_dtype)
        top_n_scores = np.zeros(n, dtype=score_dtype)

        # Compare x_i to every other observation and save the top N highest
        # scores and observation indexes for those scores.
        # We can safely skip comparing observations with indexes including and
        # before x_i, since we don't want to compare x_i to itself and any
        # observations before that will have already been compared to x_i
        # in a previous iteration of the outer loop
        for y_i in range(x_i+1, num_observations):
            leaf_node_match_arr = (
              leaf_node_matrix[x_i] == leaf_node_matrix[y_i]
            ).astype(np.int64)
            similarity_score = np.sum(leaf_node_match_arr * weights)
            # See if the score is higher than any of the top N
            # comps, and store it in the sorted comps array if it is.
            # First check if the score is higher than the lowest score,
            # since otherwise we don't need to bother iterating the scores
            if similarity_score > top_n_scores[-1]:
              for idx, score in zip(top_n_idxs, top_n_scores):
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

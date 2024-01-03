import numpy as np
import numba as nb
import pandas as pd


def get_comps(leaf_node_df, weights, n=20):
    """Fast algorithm to get the top `n` comps from a dataframe of lightgbm
    leaf node assignments (`leaf_node_df`), weighted according to a tree
    importance vector `weights`. More details on the underlying algorithm here:
    https://ccao-data.github.io/lightsnip/articles/finding-comps.html
    """
    # Convert the input dataframe to a matrix so that we can take advantage
    # of numba acceleration
    leaf_node_matrix = leaf_node_df.values
    weights_arr = np.asarray(weights, dtype=np.float64)
    top_n_comps = _get_top_n_comps(leaf_node_matrix, weights_arr, n)
    # Turn the comps back into a pandas dataframe to match the input
    df = pd.DataFrame(
        # Unpacking is necessary here or else pandas will overwrite all elements
        # with NAs
        [*top_n_comps],
        columns=[f"comp_{idx}" for idx in range(1, n+1)]
    )
    return df


def _get_top_n_comps(leaf_node_matrix, weights, n):
    """Helper function that takes a matrix of leaf node assignments for each
    observation in a tree model, an array of weights for each tree, and an
    integer N, and returns a matrix where each observation is scored by
    similarity to every other observation and the top N scores are returned
    in a tuple along with the index of the observation."""
    num_observations = len(leaf_node_matrix)
    comp_dtype = [("score", np.float64), ("idx", np.int64)]
    similarity_matrix = np.zeros((num_observations, n), dtype=comp_dtype)

    for x_i in range(num_observations):
        top_n_comps = np.zeros(n, dtype=comp_dtype)

        for y_i in range(num_observations):
            if x_i == y_i:
                # Observations should never be similar to themselves, so skip
                # the comparison
                continue
            # TODO: As a future improvement, we can skip comparisons if they
            # have already been made, we just need an efficient way of checking
            # if a comparison has been made
            else:
                leaf_node_match_arr = (
                  leaf_node_matrix[x_i] == leaf_node_matrix[y_i]
                ).astype(np.int64)
                # TODO: Why are the scores all 1.0?
                similarity_score = np.sum(leaf_node_match_arr * weights)
                # See if the score is higher than any of the top N
                # comps, and store it in the sorted comps array if it is.
                # First check if the score is higher than the lowest score,
                # since otherwise we don't need to bother iterating the scores
                if similarity_score > top_n_comps["score"][-1]:
                  for idx, comp in enumerate(top_n_comps):
                    if similarity_score > comp["score"]:
                      top_n_comps = np.concatenate((
                        top_n_comps[:idx],
                        np.array([(similarity_score, y_i)], dtype=comp_dtype),
                        top_n_comps[idx:-1]
                      ))
                      break

        similarity_matrix[x_i] = top_n_comps

    return similarity_matrix

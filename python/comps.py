import numpy as np
import numba as nb
import pandas as pd


def get_comps(leaf_node_df, weights, n=20):
    """Fast algorithm to get the top `n` comps from a dataframe of lightgbm
    leaf node assignments (`leaf_node_df`), weighted according to a feature
    importance vector `weights`. More details on the underlying algorithm here:
    https://sno.ws/big-data-table
    """
    # Convert the input dataframe to a matrix so that we can take advantage
    # of numba acceleration
    leaf_node_matrix = leaf_node_df.values
    weights_arr = np.asarray(weights, dtype=np.float64)
    comps = _get_similarity_matrix(leaf_node_matrix, weights_arr)
    # Extract the top n matches from each row of the comp matrix
    # by sorting the columns for each row, selecting the last n indices, and
    # reversing the order
    top_n_comps = np.argsort(comps)[:, -n:][:, ::-1]
    # Turn the comps back into a pandas dataframe to match the input
    return pd.DataFrame(
        top_n_comps,
        columns=[f"comp_{idx}" for idx in range(1, n+1)]
    )


@nb.njit(fastmath=True)
def _get_similarity_matrix(leaf_node_matrix, weights):
    """Helper function that takes a matrix of leaf node assignments for each
    observation in a tree model and an array of weights for each tree, and
    returns a matrix scoring the similarity every observation to every other
    observation."""
    num_observations = len(leaf_node_matrix)
    similarity_matrix = np.zeros(
        (num_observations, num_observations),
        dtype=np.float64
    )
    for x_i in range(num_observations):
        for y_i in range(num_observations):
            if x_i == y_i:
                # Observations should never be similar to themselves, so zero
                # out the comparison
                similarity_matrix[x_i][y_i] = 0
            else:
                leaf_node_match_arr = (
                  leaf_node_matrix[x_i] == leaf_node_matrix[y_i]
                ).astype(np.int64)
                similarity_matrix[x_i][y_i] = np.sum(
                    leaf_node_match_arr * weights
                )

    return similarity_matrix

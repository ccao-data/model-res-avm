import numpy as np
import numba as nb
import pandas as pd


def get_comps(leaf_node_df, weights, n=5):
    """Fast algorithm to get the top `n` comps from a dataframe of lightgbm
    leaf node assignments (`leaf_node_df`), weighted according to a feature
    importance vector `weights`. More details on the underlying algorithm here:
    https://sno.ws/big-data-table
    """
    # Convert the input dataframe to a matrix so that we can take advantage
    # of numba acceleration
    leaf_node_matrix = leaf_node_df.values
    top_n_comps = _get_comps(leaf_node_matrix, weights, n)
    # Turn the comps back into a pandas dataframe to match the input
    return pd.DataFrame(
        top_n_comps,
        columns=[f"comp_{idx}" for idx in range(1, n+1)]
    )


@nb.njit(parallel=True, fastmath=True)
def _get_comps(leaf_node_matrix, weights, n=5):
    num_observations = len(leaf_node_matrix)
    comp_matrix = np.zeros(
        (num_observations, num_observations),
        dtype=np.float64
    )
    for x_i in range(num_observations):
        for y_i in range(num_observations):
            if x_i == y_i:
                # Properties should never be comps of themselves, so zero out
                # the comparison
                comp_matrix[x_i][y_i] = 0
            else:
                comp_matrix[x_i][y_i] = np.sum(
                    (leaf_node_matrix[x_i] == leaf_node_matrix[y_i]) * weights
                )

    # Extract the top num_comps matches from each row of the comp matrix
    # by sorting the columns for each row, selecting the last N indices, and
    # reversing the order
    return np.argsort(comp_matrix, axis=1)[:, -n:][:, ::-1]

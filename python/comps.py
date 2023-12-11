import numpy as np
import numba as nb
import pandas as pd


@nb.njit(parallel=True, fastmath=True)
def get_comps(leaf_node_df, weights, n=5):
    """Get the top `n` comps from a dataframe of leaf node assignments
    (`leaf_node_df`), weighted according to a vector `weights`. More details
    on the algorithm here:
    https://sno.ws/big-data-table
    """
    num_observations = len(leaf_node_df)
    leaf_node_matrix = leaf_node_df.values

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
    top_n_comps = np.argsort(comp_matrix, axis=1)[:, -n:][:, ::-1]

    return pd.DataFrame(
        top_n_comps,
        columns=[f"comp_{idx}" for idx in range(1, n+1)]
    )

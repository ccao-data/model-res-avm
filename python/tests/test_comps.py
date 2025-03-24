import numpy as np
import pandas as pd
import pytest as pt

from ..comps import get_comps


@pt.mark.parametrize(
    "leaf_nodes,training_leaf_nodes,tree_weights,expected_comps,expected_scores",
    [
        pt.param(
            pd.DataFrame([[1, 1, 1]]),
            pd.DataFrame([[1, 1, 0], [1, 0, 0], [0, 0, 0]]),
            np.array([0.333] * 9).reshape(3, 3),
            pd.DataFrame(
                {"comp_idx_1": [0], "comp_idx_2": [1], "comp_idx_3": [-1]},
                dtype=np.int32,
            ),
            pd.DataFrame(
                {
                    "comp_score_1": [0.333 * 2],
                    "comp_score_2": [0.333 * 1],
                    "comp_score_3": [0.333 * 0]
                },
                dtype=np.float32,
            ),
            id="simple_example"
        ),
        pt.param(
            pd.DataFrame([[1, 1, 1]]),
            pd.DataFrame([[1, 1, 0], [1, 1, 0], [1, 1, 0]]),
            np.array([[0.5, 0.3, 0.2], [0.3, 0.2, 0.5], [0.2, 0.1, 0.7]]),
            pd.DataFrame(
                {"comp_idx_1": [0], "comp_idx_2": [1], "comp_idx_3": [2]},
                dtype=np.int32,
            ),
            pd.DataFrame(
                {
                    "comp_score_1": [0.5 + 0.3],
                    "comp_score_2": [0.3 + 0.2],
                    "comp_score_3": [0.2 + 0.1]
                },
                dtype=np.float32,
            ),
            id="weights_tiebreak"
        )
    ],
)
def test_get_comps(
    leaf_nodes,
    training_leaf_nodes,
    tree_weights,
    expected_comps,
    expected_scores
):
    comps, scores = get_comps(
        leaf_nodes,
        training_leaf_nodes,
        tree_weights,
        num_comps=3
    )
    assert comps.equals(expected_comps)
    assert scores.equals(expected_scores)

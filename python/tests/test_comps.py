import numpy as np
import pandas as pd
import pytest as pt

import comps as comps_module


@pt.mark.parametrize(
    "leaf_nodes,training_leaf_nodes,tree_weights,num_comps,expected_comps,expected_scores",
    [
        # Simple example that tests one input observation with uniform weights
        # to make sure that the algorithm correctly prioritizes comparison
        # observations with the highest number of leaf node matches
        pt.param(
            pd.DataFrame([[1, 1, 1]]),
            pd.DataFrame([[1, 1, 0], [1, 0, 0], [0, 0, 0]]),
            np.array([0.333] * 9).reshape(3, 3),
            3,
            pd.DataFrame(
                {
                    "comp_idx_1": [0],
                    "comp_idx_2": [1],
                    # Only two comparison observations have any leaf nodes that
                    # match the input observation, so make sure that the third
                    # comp is empty
                    "comp_idx_3": [-1],
                },
                dtype=np.int32,
            ),
            pd.DataFrame(
                {
                    # The weights are identical for all trees, so the score
                    # should just be the global weight multiplied by the
                    # number of leaf nodes in the comparison observation
                    # that match the input observation
                    "comp_score_1": [0.333 * 2],
                    "comp_score_2": [0.333 * 1],
                    "comp_score_3": [0.333 * 0],
                },
                dtype=np.float32,
            ),
            id="simple_one_observation",
        ),
        # Extend the simple test to work with two observations to ensure
        # that the algorithm correctly prioritizes larger counts of matches
        # even when there are multiple input observations
        pt.param(
            pd.DataFrame([[1, 1, 1], [2, 2, 2]]),
            pd.DataFrame([[1, 1, 1], [1, 1, 2], [1, 2, 2], [2, 2, 2]]),
            np.array([0.333] * 12).reshape(4, 3),
            4,
            pd.DataFrame(
                {
                    # The first element of each column is the comp for the
                    # first input observation, and the second element is the
                    # comp for the second input observation
                    "comp_idx_1": [0, 3],
                    "comp_idx_2": [1, 2],
                    "comp_idx_3": [2, 1],
                    "comp_idx_4": [-1, -1],
                },
                dtype=np.int32,
            ),
            pd.DataFrame(
                {
                    # The number of leaf nodes that match each comp are
                    # identical between the two input observations, and all
                    # trees have the same weights, so all we need to do to
                    # compute the expected score is to multiply the global
                    # weight by the number of leaf node matches and then copy
                    # that to the second element in the score column to account
                    # for both input observations
                    "comp_score_1": [0.333 * 3] * 2,
                    "comp_score_2": [0.333 * 2] * 2,
                    "comp_score_3": [0.333 * 1] * 2,
                    "comp_score_4": [0.333 * 0] * 2,
                },
                dtype=np.float32,
            ),
            id="simple_two_observations",
        ),
        # Test to make sure that the tree weight vector is correctly applied
        # to break ties in situations where comparison observations have the
        # same number of matching leaf nodes but different weights for those
        # leaf nodes
        pt.param(
            pd.DataFrame([[1, 1, 1]]),
            pd.DataFrame([[1, 1, 0], [1, 1, 0], [1, 1, 0]]),
            np.array([[0.5, 0.3, 0.2], [0.3, 0.2, 0.5], [0.2, 0.1, 0.7]]),
            3,
            pd.DataFrame(
                {"comp_idx_1": [0], "comp_idx_2": [1], "comp_idx_3": [2]},
                dtype=np.int32,
            ),
            pd.DataFrame(
                {
                    "comp_score_1": [0.5 + 0.3],
                    "comp_score_2": [0.3 + 0.2],
                    "comp_score_3": [0.2 + 0.1],
                },
                dtype=np.float32,
            ),
            id="weights_tiebreak",
        ),
        # TODO: Test to make sure that chunking produces the correct values
    ],
)
def test_get_comps(
    leaf_nodes,
    training_leaf_nodes,
    tree_weights,
    num_comps,
    expected_comps,
    expected_scores,
):
    """Test all of our common parametrized test cases"""
    comps, scores = comps_module.get_comps(
        leaf_nodes,
        training_leaf_nodes,
        tree_weights,
        num_comps,
    )
    assert comps.equals(expected_comps)
    assert scores.equals(expected_scores)

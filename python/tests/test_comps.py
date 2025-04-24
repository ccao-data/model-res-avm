import numpy as np
import pandas as pd
import pytest as pt

import comps as comps_module


@pt.mark.parametrize(
    "leaf_nodes,training_leaf_nodes,tree_weights,num_comps,num_chunks,expected_comps,expected_scores",
    [
        # Simple example that tests one input observation with uniform weights
        # to make sure that the algorithm correctly prioritizes comparison
        # observations with the highest number of leaf node matches
        pt.param(
            pd.DataFrame([[1, 1, 1]]),
            pd.DataFrame([[1, 1, 0], [1, 0, 0], [0, 0, 0]]),
            np.array([0.333] * 9).reshape(3, 3),
            3,
            1,
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
            id="one_observation",
        ),
        # Extend the simple test to work with two observations to ensure
        # that the algorithm correctly prioritizes larger counts of matches
        # even when there are multiple input observations
        pt.param(
            pd.DataFrame([[1, 1, 1], [2, 2, 2]]),
            pd.DataFrame([[1, 1, 1], [1, 1, 2], [1, 2, 2], [2, 2, 2]]),
            np.array([0.333] * 12).reshape(4, 3),
            4,
            1,
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
            id="two_observations",
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
            1,
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
        # Test that score tiebreaks always prioritize the first observation
        # in the comparison array
        pt.param(
            pd.DataFrame([[1, 1, 1]]),
            pd.DataFrame([[1, 1, 1], [1, 1, 1], [1, 1, 1]]),
            np.array([0.333] * 9).reshape(3, 3),
            3,
            1,
            pd.DataFrame(
                {"comp_idx_1": [0], "comp_idx_2": [1], "comp_idx_3": [2]},
                dtype=np.int32,
            ),
            pd.DataFrame(
                {
                    "comp_score_1": [0.333 * 3],
                    "comp_score_2": [0.333 * 3],
                    "comp_score_3": [0.333 * 3],
                },
                dtype=np.float32,
            ),
            id="score_tiebreak",
        ),
        # Test to make sure that chunking produces the correct values when
        # there are _more_ chunks than observations
        pt.param(
            pd.DataFrame([[1, 1, 1], [2, 2, 2], [3, 3, 3]]),
            pd.DataFrame([[1, 1, 1], [2, 2, 2], [3, 3, 3]]),
            np.array([0.333] * 9).reshape(3, 3),
            3,
            10,
            pd.DataFrame(
                {
                    "comp_idx_1": [0, 1, 2],
                    "comp_idx_2": [-1, -1, -1],
                    "comp_idx_3": [-1, -1, -1],
                },
                dtype=np.int32,
            ),
            pd.DataFrame(
                {
                    "comp_score_1": [0.333 * 3] * 3,
                    "comp_score_2": [0.333 * 0] * 3,
                    "comp_score_3": [0.333 * 0] * 3,
                },
                dtype=np.float32,
            ),
            id="more_chunks_than_observations",
        ),
        # Test to make sure that chunking produces the correct values when
        # there are _fewer_ chunks than observations
        pt.param(
            pd.DataFrame([[1, 1, 1], [2, 2, 2], [3, 3, 3]]),
            pd.DataFrame([[1, 1, 1], [2, 2, 2], [3, 3, 3]]),
            np.array([0.333] * 9).reshape(3, 3),
            3,
            2,
            pd.DataFrame(
                {
                    "comp_idx_1": [0, 1, 2],
                    "comp_idx_2": [-1, -1, -1],
                    "comp_idx_3": [-1, -1, -1],
                },
                dtype=np.int32,
            ),
            pd.DataFrame(
                {
                    "comp_score_1": [0.333 * 3] * 3,
                    "comp_score_2": [0.333 * 0] * 3,
                    "comp_score_3": [0.333 * 0] * 3,
                },
                dtype=np.float32,
            ),
            id="fewer_chunks_than_observations",
        ),
        # Test that comps will not make it into the output if we have more
        # comparison observations with positive scores than `num_comps`
        pt.param(
            pd.DataFrame([[1, 1, 1]]),
            pd.DataFrame([[1, 1, 1], [1, 1, 1], [1, 1, 1]]),
            np.array([0.333] * 9).reshape(3, 3),
            2,
            1,
            pd.DataFrame(
                {
                    "comp_idx_1": [0],
                    "comp_idx_2": [1],
                },
                dtype=np.int32,
            ),
            pd.DataFrame(
                {
                    "comp_score_1": [0.333 * 3],
                    "comp_score_2": [0.333 * 3],
                },
                dtype=np.float32,
            ),
            id="more_possible_comps_than_num_comps",
        ),
    ],
)
def test_get_comps(
    leaf_nodes,
    training_leaf_nodes,
    tree_weights,
    num_comps,
    num_chunks,
    expected_comps,
    expected_scores,
):
    comps, scores = comps_module.get_comps(
        leaf_nodes,
        training_leaf_nodes,
        tree_weights,
        num_comps,
        num_chunks,
    )
    assert comps.equals(expected_comps)
    assert scores.equals(expected_scores)


@pt.mark.parametrize(
    "leaf_nodes,training_leaf_nodes,tree_weights,expected_exception,expected_msg",
    [
        pt.param(
            pd.DataFrame([[1, 1]]),
            pd.DataFrame([[1, 1, 1], [2, 2, 2], [3, 3, 3]]),
            np.array([0.333] * 9).reshape(3, 3),
            ValueError,
            "Number of columns in `observation_df` (2) must match `comparison_df` (3)",
            id="observation_comparison_shapes_match",
        ),
        pt.param(
            pd.DataFrame([[1, 1, 1]]),
            pd.DataFrame([[1, 1, 1], [2, 2, 2], [3, 3, 3]]),
            np.array([0.333] * 4).reshape(2, 2),
            ValueError,
            "`comparison_df.shape` (3, 3) must match `weights.shape` (2, 2)",
            id="comparison_weights_shapes_match",
        ),
    ],
)
def test_get_comps_raises_on_invalid_inputs(
    leaf_nodes,
    training_leaf_nodes,
    tree_weights,
    expected_exception,
    expected_msg,
):
    with pt.raises(expected_exception) as exc_info:
        comps_module.get_comps(leaf_nodes, training_leaf_nodes, tree_weights)
    assert str(exc_info.value) == expected_msg


@pt.mark.parametrize(
    "arr, elem, idx, expected",
    [
        pt.param([1, 2, 4, 5, 0], 3, 2, [1, 2, 3, 4, 5], id="insert_middle"),
        pt.param([2, 3, 4, 5, 0], 1, 0, [1, 2, 3, 4, 5], id="insert_start"),
        pt.param([1, 2, 3, 4, 0], 5, 4, [1, 2, 3, 4, 5], id="insert_end"),
        pt.param([0], 1, 0, [1], id="insert_single_element"),
        pt.param([], 1, 0, [], id="insert_empty_array"),
        pt.param([1, 2, 3, 4, 5], 6, 10, [1, 2, 3, 4, 5], id="insert_out_of_bounds"),
    ],
)
def test_insert_at_idx_and_shift(arr, elem, idx, expected):
    result = comps_module.insert_at_idx_and_shift(np.array(arr), elem, idx)
    np.testing.assert_array_equal(result, np.array(expected))

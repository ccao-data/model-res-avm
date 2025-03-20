import decimal
import pandas as pd
import numpy as np

# Define constants for column indexes in the extracted LightGBM model structure
TREE_INDEX = 0
DEPTH = 1
SPLIT_INDEX = 2
SPLIT_FEATURE = 3
NODE_PARENT = 4
LEAF_INDEX = 5
LEAF_PARENT = 6
THRESHOLD = 8
DECISION_TYPE = 9
DEFAULT_LEFT = 10

ZERO_LEAF_IDX = 0.1

# Define a dictionary to map decision types to comparison functions
decision_functions = {
    "<": lambda feature_value, threshold: feature_value < threshold,
    "<=": lambda feature_value, threshold: feature_value <= threshold,
    ">": lambda feature_value, threshold: feature_value > threshold,
    ">=": lambda feature_value, threshold: feature_value >= threshold,
    "==": lambda feature_value, threshold: feature_value == threshold,
    "!=": lambda feature_value, threshold: feature_value != threshold
}

def _parse_tree_structure(tree_df: np.ndarray, num_trees: int) -> list[dict]:
    """
    Parse the structure of a tree dataframe and return a dataframe that is
    optimized for estimating node assignments for observations
    """
    tree_indices = np.unique(tree_df[:, TREE_INDEX])[:num_trees]
    parsed_trees = []
    for tree_index in tree_indices:
        tree = tree_df[tree_df[:, TREE_INDEX] == tree_index]
        parsed_nodes = []

        # Consolidate split and leaf node metadata
        for node in tree:
            is_leaf = np.isnan(node[SPLIT_INDEX])
            if is_leaf:
                # Handle collisions between split and leaf node IDs by
                # converting leaf node IDs to negative. This doesn't work for
                # 0, so handle that as a special case
                node_key = ZERO_LEAF_IDX if int(node[LEAF_INDEX]) == 0 else -int(node[LEAF_INDEX])
                raw_parent = node[LEAF_PARENT]
            else:
                node_key = int(node[SPLIT_INDEX])
                raw_parent = node[NODE_PARENT]

            parsed_nodes.append(
                {
                    "is_leaf": is_leaf,
                    "key": node_key,
                    "depth": int(node[DEPTH]),
                    "split_feature": node[SPLIT_FEATURE] if not is_leaf else None,
                    "threshold": decimal.Decimal(node[THRESHOLD]) if not is_leaf else None,
                    "decision_type": node[DECISION_TYPE] if not is_leaf else None,
                    "default_to_pass": node[DEFAULT_LEFT] == "TRUE" if not is_leaf else None,
                    "parent": int(raw_parent) if not np.isnan(raw_parent) else None,
                    "child_pass": None,  # We'll fill this out later
                    "child_fail": None,
                }
            )

        # Assign child IDs to each split node
        map_parent_to_children = {}
        for node in parsed_nodes:
            if node["parent"] is None:
                continue

            if not map_parent_to_children.get(node["parent"]):
                map_parent_to_children[node["parent"]] = {"pass": node["key"]}
            else:
                # There should only be 2, so the second occurrence must be the
                # failure branch
                map_parent_to_children[node["parent"]]["fail"] = node["key"]

        # Complete the structure of the tree by assigning child IDs to each
        # split node
        parsed_tree = {}
        for node in parsed_nodes:
            if not node["is_leaf"]:
                node["child_pass"] = map_parent_to_children[node["key"]]["pass"]
                node["child_fail"] = map_parent_to_children[node["key"]]["fail"]
            node_without_key = node.copy()
            del(node_without_key["key"])
            parsed_tree[node["key"]] = node_without_key

        # Validate structure of the parsed tree before assigning it to output
        for node in parsed_nodes:
            # Every split node should have a key in the parsed tree
            assert node["is_leaf"] or node["key"] in parsed_tree, (
                f"Split node {node['key']} is missing from parsed tree {tree_index}"
            )

        for key, node in parsed_tree.items():
            if node["is_leaf"]:
                # Leaf nodes should have no children
                assert node["child_pass"] is None, (
                    f"Leaf node {key} should not have a `child_pass` branch"
                )
                assert node["child_fail"] is None, (
                    f"Leaf node {key} should not have a `child_fail` branch"
                )
            else:
                # Split nodes should have exactly two children
                assert node["child_pass"] is not None, (
                    f"Split node {key} is missing required `child_pass` branch"
                )
                assert node["child_fail"] is not None, (
                    f"Split node {key} is missing required `child_fail` branch"
                )

        # First node should always be a split node
        assert parsed_nodes[0]["key"] == 0
        assert not parsed_tree[0]["is_leaf"]

        parsed_trees.append(parsed_tree)

    return parsed_trees


def _get_split_nodes(
    data_points: np.ndarray,
    tree_df: np.ndarray,
    feature_map: dict[str, int],
    num_trees: int | None,
    max_depth: int
) -> np.ndarray:
    num_trees = num_trees or np.unique(tree_df[:, TREE_INDEX]).max() + 1
    all_split_nodes = np.full((data_points.shape[0], num_trees, max_depth), np.nan)
    parsed_trees = _parse_tree_structure(tree_df, num_trees)
    for obs_idx in range(data_points.shape[0]):
        data_point = data_points[obs_idx]
        for tree_idx, tree in enumerate(parsed_trees):
            # The first node should always have an index of 0 (we validate this
            # in the _parse_tree_structure function). This is the base case
            # for the recursion that we use to traverse the tree
            parent_node_key = 0
            while True:
                node = tree[parent_node_key]
                if node["is_leaf"]:
                    # We've reached the leaf node, which is the terminal
                    # case for the tree traversal, so exit the loop
                    break

                decision_type = node["decision_type"]
                split_feature = node["split_feature"]
                threshold = node["threshold"]
                child_pass, child_fail = node["child_pass"], node["child_fail"]
                default_to_pass = node["default_to_pass"]
                depth = node["depth"]

                feature_value = data_point[feature_map[split_feature]]

                # Use decision type to determine the next node
                if decision_type in decision_functions:
                    if np.isnan(feature_value):
                        child_node_key = child_pass if default_to_pass else child_fail
                    else:
                        child_node_key = child_pass if decision_functions[decision_type](feature_value, threshold) else child_fail
                else:
                    raise ValueError(f"Unsupported decision type: {decision_type}")

                # Since the base case split node index (0) is shared between
                # all observations, we don't count it as part of the split
                # node path. This means that we don't need a special indicator
                # for the leaf node that happens to share that same index, and
                # we can revert it back to its original index for ease of
                # comparing to other types of model output that record leaf
                # node assignments
                child_node_assign_key = 0 if child_node_key == ZERO_LEAF_IDX else child_node_key
                all_split_nodes[obs_idx, tree_idx, depth] = child_node_assign_key

                # Restart the loop and recurse the next branch
                parent_node_key = child_node_key

    return all_split_nodes


def get_split_nodes(data_points, tree_df, num_trees: int | None = None) -> np.ndarray:
    # Parse some metadata about the input data that we need to preserve before
    # we transform the data for more efficient processing
    max_depth = max(tree_df.groupby("tree_index")["depth"].max().values)
    feature_map: dict[str, int] = {
        colname: idx for idx, colname in enumerate(data_points.columns)
    }
    # Transform the input to numpy arrays so that we can process them more
    # efficiently in the inner function
    data_points_arr = np.array(data_points)
    tree_arr = np.array(tree_df)
    # Pass off to a parallelized helper function
    return _get_split_nodes(
        data_points_arr,
        tree_arr,
        feature_map,
        num_trees,
        max_depth
    )


if __name__ == "__main__":
    data_points = pd.read_parquet("data_points.parquet")
    tree_df = pd.read_parquet("tree_df.parquet")
    splits = get_split_nodes(data_points, tree_df)
    print(splits)

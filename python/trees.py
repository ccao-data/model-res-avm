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

# Define a dictionary to map decision types to comparison functions
decision_functions = {
    "<": lambda feature_value, split_value: feature_value < split_value,
    "<=": lambda feature_value, split_value: feature_value <= split_value,
    ">": lambda feature_value, split_value: feature_value > split_value,
    ">=": lambda feature_value, split_value: feature_value >= split_value,
    "==": lambda feature_value, split_value: feature_value == split_value,
    "!=": lambda feature_value, split_value: feature_value != split_value
}


def _get_split_nodes(
    data_points: np.ndarray,
    tree_df: np.ndarray,
    feature_map: dict[str, int],
    num_trees: int,
    max_depth: int
) -> np.ndarray:
    all_split_nodes = np.full((data_points.shape[0], num_trees, max_depth), np.nan)
    tree_indices = np.unique(tree_df[:, TREE_INDEX])[:num_trees]
    for i in range(data_points.shape[0]):
        data_point = data_points[i]
        for tree_index in tree_indices:
            tree = tree_df[tree_df[:, TREE_INDEX] == tree_index]
            current_node = 0
            while True:
                # There should only ever be one node with the current idx, so
                # pull it
                node = tree[tree[:, SPLIT_INDEX] == current_node]
                if node.shape[0] != 1:
                    raise ValueError(f"Expected exactly one node, but got {node.shape[0]}")
                node = node[0]

                # Extract data from the tree structure about this node
                split_feature = node[SPLIT_FEATURE]
                split_value = decimal.Decimal(node[THRESHOLD])
                depth = node[DEPTH]
                decision_type = node[DECISION_TYPE]
                default_left = node[DEFAULT_LEFT] == "TRUE" if not pd.isna(node[DEFAULT_LEFT]) else False
                feature_value = data_point[feature_map[split_feature]]

                # Determine the left and right child nodes
                children = tree[(tree[:, NODE_PARENT] == current_node) | (tree[:, LEAF_PARENT] == current_node)]
                if children.shape[0] != 2:
                    raise ValueError(f"Expected exactly two children, but got {children.shape[0]}")
                child_data = [
                    {
                        "index": int(c[SPLIT_INDEX]) if not np.isnan(c[SPLIT_INDEX]) else int(c[LEAF_INDEX]),
                        "depth": c[DEPTH],
                        "is_leaf": np.isnan(c[SPLIT_INDEX])
                    }
                    for c in children
                ]
                # Lefthand side should prefer splits and lower indices
                # TODO: Is this correct? It doesn't seem to always produce
                # the right assignment
                child_data.sort(key=lambda x: (x["is_leaf"], x["index"]))
                left_child, right_child = child_data[0], child_data[1]

                # Use decision type to determine the next node
                if decision_type in decision_functions:
                    if np.isnan(feature_value):
                        next_node = left_child if default_left else right_child
                    else:
                        next_node = left_child if decision_functions[decision_type](feature_value, split_value) else right_child
                else:
                    raise ValueError(f"Unsupported decision type: {decision_type}")

                # Distinguish splits from leaves in the output array by marking
                # leaves as negative. This works even though some leaf nodes
                # can have an index of 0, because the first split node always
                # has an index of 0 but never should be included in the output
                # as all observations share the first split
                next_node_fmt = -next_node['index'] if next_node["is_leaf"] else next_node["index"]
                all_split_nodes[i, tree_index, depth] = next_node_fmt

                if next_node["is_leaf"]:
                    # We've reached the leaf node, so exit the loop
                    break

                # If the next node is not a leaf node (i.e. it's a split) start
                # the loop over again at the next node
                current_node = next_node["index"]

    return all_split_nodes


def get_split_nodes(data_points, tree_df, num_trees: int) -> np.ndarray:
    data_points_np = np.array(data_points)
    tree_df_np = np.array(tree_df)
    max_depth = max(tree_df.groupby("tree_index")["depth"].max().values)
    feature_map: dict[str, int] = {
        colname: idx for idx, colname in enumerate(data_points.columns)
    }
    return _get_split_nodes(
        data_points_np,
        tree_df_np,
        feature_map,
        num_trees,
        max_depth
    )


if __name__ == "__main__":
    data_points = pd.read_parquet("data_points.parquet")
    tree_df = pd.read_parquet("tree_df.parquet")
    splits = get_split_nodes(data_points, tree_df, num_trees = 3)
    print(splits)

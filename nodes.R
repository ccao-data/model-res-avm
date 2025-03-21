# Load required libraries
library(lightgbm)
library(dplyr)
library(data.table)
library(reticulate)

source("R/helpers.R")

# Generate some example data
# TODO: Refactor for regression problem
data(iris)
iris <- as.data.table(iris)

# Add a categorical feature
iris[, CategoricalFeature := sample(letters[1:3], .N, replace = TRUE)]
iris[, CategoricalFeature := as.integer(as.factor(CategoricalFeature))]

# Prepare data for LightGBM
# Convert the data table to a matrix, excluding the label column
train_data <- as.matrix(iris[, -c("Species"), with = FALSE])
# Convert the label column to numeric
train_label <- as.numeric(iris$Species) - 1

# Create LightGBM dataset with the categorical feature specified
dtrain <- lgb.Dataset(
  data = train_data,
  label = train_label,
  categorical_feature = c("CategoricalFeature")
)

# Set parameters
params <- list(
  objective = "multiclass",
  num_class = 3,
  metric = "multi_logloss",
  max_depth = 3
)
num_trees <- 100

# Train the model
bst <- lgb.train(params, dtrain, num_trees)

# Predict leaf indices for each data point
leaf_indices <- predict(bst, train_data, type = "leaf")

tree_weights <- extract_tree_weights(
  model = bst,
  init_score = mean(train_label),
  training_data = train_data,
  outcome = train_label,
  num_iterations = num_trees
)

# Extract tree structure
tree_df <- lgb.model.dt.tree(bst)

# Use reticulate to source the Python script
trees_module <- import("python.trees")

# Example usage
data_points <- train_data[1:2, ]
split <- trees_module$get_splits(
  as.data.frame(data_points),
  tree_df %>% mutate(split_index = coalesce(split_index, NaN)),
  num_trees = 3L
)
print(split)

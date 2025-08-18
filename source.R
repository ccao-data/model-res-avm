library(tictoc)
library(dplyr)
library(arrow)
library(lightgbm)
library(recipes)
library(tibble)
library(here)
library(microbenchmark)

source("R/helpers.R")
paths <- model_file_dict()

training_data <- read_parquet(paths$input$training$local) %>%
  filter(!ind_pin_is_multicard, !sv_is_outlier) %>%
  as_tibble()

lgbm_final_full_fit <- lightsnip::lgbm_load(paths$output$workflow_fit$local)
booster <- lgbm_final_full_fit$fit
tree_dt <- lgb.model.dt.tree(booster)
recipe <- readRDS(paths$output$workflow_recipe$local)
lgbm_final_full_recipe <- readRDS(paths$output$workflow_recipe$local)

training_data_prepped_filtered <- recipes::bake(
  object = lgbm_final_full_recipe,
  new_data = training_data,
  recipes::all_predictors()
)

extract_tree_weights <- function(model,
                                 init_score,
                                 training_data,
                                 outcome,
                                 num_iterations) {
  # Coerce and validate
  num_iterations <- as.integer(num_iterations)
  stopifnot(is.numeric(outcome), length(outcome) == nrow(training_data))

  # Convert predictors to numeric matrix
  X <- training_data |>
    as.data.frame() |>
    data.matrix()

  # ---- Leaf index extraction ----
  leaf_idx <- predict(model, X, type = "leaf")
  total_trees <- ncol(leaf_idx)
  if (num_iterations > total_trees) {
    warning(sprintf(
      "num_iterations (%d) > model trees (%d); using %d.",
      num_iterations, total_trees, total_trees
    ))
    num_iterations <- total_trees
  }
  leaf_idx <- leaf_idx[, seq_len(num_iterations), drop = FALSE]

  # ---- Map leaf indices to values ----
  tree_dt <- lgb.model.dt.tree(model)
  leaf_lookup <- subset(tree_dt, !is.na(leaf_index),
    select = c("tree_index", "leaf_index", "leaf_value")
  )

  leaf_values <- matrix(0.0, nrow = nrow(leaf_idx), ncol = ncol(leaf_idx))
  for (t in seq_len(ncol(leaf_idx))) {
    this_tree <- leaf_lookup[leaf_lookup$tree_index == (t - 1L), , drop = FALSE]
    m <- match(leaf_idx[, t], this_tree$leaf_index)
    v <- this_tree$leaf_value[m]
    v[is.na(v)] <- 0.0
    leaf_values[, t] <- v
  }

  # ---- Row-wise cumulative sums ----
  if (ncol(leaf_values) >= 2L) {
    for (j in 2:ncol(leaf_values)) {
      leaf_values[, j] <- leaf_values[, j] + leaf_values[, j - 1L]
    }
  }

  # ---- Predictions from init score baseline ----
  tree_predictions <- matrix(init_score,
    nrow = nrow(X),
    ncol = num_iterations + 1L
  )
  if (num_iterations >= 1L) {
    tree_predictions[, 2:(num_iterations + 1L)] <- init_score + leaf_values
  }

  # ---- Error differences & weights ----
  outcome_mat <- matrix(outcome,
    nrow = length(outcome),
    ncol = ncol(tree_predictions)
  )
  tree_errors <- abs(outcome_mat - tree_predictions)

  diff_in_errors <- t(apply(tree_errors, 1L, diff)) * -1
  diff_in_errors[diff_in_errors < 0] <- 0

  denom <- rowSums(diff_in_errors)
  weights <- diff_in_errors / ifelse(denom == 0, 1, denom)
  weights[is.na(weights)] <- 0

  return(weights)
}

weights <- extract_tree_weights(
  model = booster,
  training_data = training_data_prepped_filtered,
  outcome = training_data$meta_sale_price,
  num_iterations = lgbm_final_full_fit$fit$params$num_iterations,
  init_score = mean(training_data$meta_sale_price, na.rm = TRUE)
)

write.csv(weights, "weights.csv")


extract_tree_weights_old <- function(model,
                                     init_score,
                                     training_data,
                                     outcome,
                                     num_iterations) {
  tree_predictions <- matrix(
    nrow = nrow(training_data),
    ncol = num_iterations + 1
  )
  tree_predictions[, 1] <- init_score
  for (num_iteration in seq_len(num_iterations)) {
    if (num_iteration %% 100 == 0) {
      message(
        glue::glue("Extracting weights for tree {num_iteration}")
      )
    }
    tree_predictions[, num_iteration + 1] <- predict(
      object = model,
      newdata = as.matrix(training_data),
      num_iteration = num_iteration
    )
  }
  tree_errors <- abs(outcome - tree_predictions)
  # Get a lagged diff of the error matrix by row. Since diff() operates on
  # matrix columns, we need to transpose the input
  diff_in_errors <- tree_errors %>%
    t() %>%
    diff(1, 1) %>%
    t() * -1

  diff_in_errors <- diff_in_errors %>%
    apply(2, function(x) ifelse(x < 0, 0, x))

  # Take proportion of diff in errors over total diff in
  # errors from all trees
  weights <- diff_in_errors / rowSums(diff_in_errors)
  weights[is.nan(weights)] <- 0

  return(weights)
}

weights_old <- extract_tree_weights_old(
  model = booster,
  training_data = training_data_prepped_filtered,
  outcome = training_data$meta_sale_price,
  num_iterations = lgbm_final_full_fit$fit$params$num_iterations,
  init_score = mean(training_data$meta_sale_price, na.rm = TRUE)
)

write.csv(weights_old, "weights_old.csv")

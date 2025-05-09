# Shared utilities for working with comps

# Extract weights for all trees in a model, where weight corresponds to the
# relative reduction in error that a given tree contributes to the overall
# error reduction of the ensemble. These weights are useful for calculating
# comps using leaf node assignments, since tree weights allow the comps
# algorithm to weight each matching leaf node according to the importance of
# the tree. See the `get_comps()` function in the `python.comps` module for
# details
extract_tree_weights <- function(model,
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


# Main R interface for calculating comps for a set of observations in an
# `assessment_data` dataframe. This function is primarily a wrapper around
# The `extract_tree_weights()` R function and the `get_comps()` Python
# function. See those two functions for details on how the algorithm works
generate_comps <- function(assessment_data,
                           training_data,
                           model,
                           recipe,
                           triad,
                           num_comps = 5) {
  # Filter target properties for only the current triad, to speed up the comps
  # algorithm
  comp_assessment_data_preprocess <- assessment_data %>%
    filter(
      meta_township_code %in% (
        ccao::town_dict %>%
          filter(triad_name == tools::toTitleCase(triad)) %>%
          pull(township_code)
      )
    )

  # Multi-card handling. For multi-card pins with 2-3 cards, we predict by
  # aggregating the bldg_sf to a single card, and using that card to predict
  # the value for the multi-card PIN as a whole. Since we don't predict on the
  # other cards, we set them aside for comp generation, to re-attach them later
  small_multicards <- comp_assessment_data_preprocess %>%
    filter(meta_pin_num_cards %in% c(2, 3))

  frankencards <- small_multicards %>%
    group_by(meta_pin) %>%
    arrange(sqft_card_num_sort) %>%
    slice(1) %>%
    ungroup()

  single_cards_and_large_multicards <- comp_assessment_data_preprocess %>%
    filter(!meta_pin %in% frankencards$meta_pin)

  comp_assessment_data <-
    bind_rows(single_cards_and_large_multicards, frankencards)

  comp_assessment_data_prepped <- recipes::bake(
    object = recipe,
    new_data = comp_assessment_data,
    all_predictors()
  )

  # Calculate the leaf node assignments for every predicted value.
  # Due to integer overflow problems with leaf node assignment, we need to
  # chunk our data such that they are strictly less than the limit of 1073742
  # rows. More detail here: https://github.com/microsoft/LightGBM/issues/1884
  chunk_size <- 500000
  chunks <- split(
    comp_assessment_data_prepped,
    ceiling(seq_along(comp_assessment_data_prepped[[1]]) / chunk_size)
  )
  chunked_leaf_nodes <- chunks %>%
    map(\(chunk) {
      predict(
        object = model,
        newdata = as.matrix(chunk),
        type = "leaf",
      )
    })
  # Prefer do.call(rbind, ...) over bind_rows() because the chunks are
  # not guaranteed to have the same number of rows, and bind_rows() will raise
  # an error in that case
  leaf_nodes <- do.call(rbind, chunked_leaf_nodes) %>% as_tibble()

  # Calculate weights representing feature importance, so that we can weight
  # leaf node assignments based on the most important features.
  # To do this, we need the training data so that we can compute the mean sale
  # price and use it as the base model error
  message("Extracting weights from training data")
  training_data <- training_data %>%
    filter(!ind_pin_is_multicard, !sv_is_outlier) %>%
    as_tibble()
  training_data_prepped <- recipes::bake(
    object = recipe,
    new_data = training_data,
    all_predictors()
  )

  tree_weights <- extract_tree_weights(
    model = model,
    init_score = mean(training_data$meta_sale_price, na.rm = TRUE),
    training_data = training_data_prepped,
    outcome = training_data$meta_sale_price,
    num_iterations = model$params$num_iterations
  )

  if (length(tree_weights) == 0) {
    message("Warning: tree_weights are empty")
  }
  if (all(rowSums(tree_weights) %in% c(0, 1))) {
    message("Warning: tree_weights do not sum to 1 or 0 for each row")
    message("First 5 weights:")
    print(head(tree_weights, 5))
  }

  message("Getting leaf node assignments for the training data")

  # Get predicted values and leaf node assignments for the training data
  training_leaf_nodes <- predict(
    object = model,
    newdata = as.matrix(training_data_prepped),
    type = "leaf"
  ) %>%
    as_tibble()

  # Make sure that the leaf node tibbles are all integers, which is what
  # the comps algorithm expects
  leaf_nodes <- leaf_nodes %>%
    mutate(across(everything(), ~ as.integer(.x)))
  training_leaf_nodes <- training_leaf_nodes %>%
    mutate(across(everything(), ~ as.integer(.x)))

  # Do the comps calculation in Python because the code is simpler and faster
  message("Calling out to python/comps.py to perform comps calculation")
  comps_module <- import("python.comps")
  tryCatch(
    {
      comps <- comps_module$get_comps(
        leaf_nodes, training_leaf_nodes, tree_weights,
        num_comps = num_comps
      )
    },
    error = function(e) {
      # Log the full Python traceback in case of an error
      print(py_last_error())
      stop("Encountered error in python/comps.py")
    }
  )

  # Translate comp indexes to PINs and document numbers
  comps[[1]] <- comps[[1]] %>%
    mutate(
      # Correct for the fact that Python is 0-indexed by incrementing the
      # comp indexes by 1, and cast null indicators (-1) to null
      across(everything(), ~ ifelse(. == -1, NA, . + 1)),
      # Map comp index to PIN
      across(
        starts_with("comp_idx_"),
        \(idx_row) {
          ifelse(is.na(idx_row), NA, training_data[idx_row, ]$meta_pin)
        },
        .names = "comp_pin_{str_remove(col, 'comp_idx_')}"
      ),
      # Map comp index to sale doc number
      across(
        starts_with("comp_idx_"),
        \(idx_row) {
          ifelse(
            is.na(idx_row),
            NA,
            training_data[idx_row, ]$meta_sale_document_num
          )
        },
        .names = "comp_document_num_{str_remove(col, 'comp_idx_')}"
      )
    ) %>%
    select(-starts_with("comp_idx_")) %>%
    cbind(
      pin = comp_assessment_data$meta_pin,
      card = comp_assessment_data$meta_card_num
    ) %>%
    relocate(pin, card)

  comp_idxs_and_scores <- cbind(comps[[1]], comps[[2]])

  # Grab removed small multi-cards, re-add them, and assign them the comps data
  # that we calculated for the frankencard
  removed_cards <- small_multicards %>%
    anti_join(frankencards, by = c("meta_pin", "meta_card_num")) %>%
    select(meta_pin, meta_card_num)

  removed_cards_comps <- removed_cards %>%
    rename(pin = meta_pin, card = meta_card_num) %>%
    left_join(comp_idxs_and_scores %>% select(-card), by = "pin")

  # Return final combined comps dataframe
  bind_rows(comp_idxs_and_scores, removed_cards_comps) %>%
    arrange(pin, card)
}

# Helper function for uploading a comps dataframe to a location on S3,
# partitioned by year and run ID
# TODO: Handle versioning and staging runs
upload_comps <- function(comps, s3_uri, run_id, year) {
  comps %>%
    mutate(run_id = !!run_id, year = !!year) %>%
    group_by(year, run_id) %>%
    arrow::write_dataset(
      path = s3_path,
      format = "parquet",
      hive_style = TRUE,
      compression = "snappy"
    )
}

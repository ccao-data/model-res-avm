#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Setup ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# NOTE: See DESCRIPTION for library dependencies and R/setup.R for
# variables used in each pipeline stage

# Start the stage timer and clear logs from prior stage
tictoc::tic.clearlog()
tictoc::tic("Interpret")

# Load libraries, helpers, and recipes from files
purrr::walk(list.files("R/", "\\.R$", full.names = TRUE), source)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. Load Data -----------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Loading model fit and recipe")

# Load the final lightgbm model object and recipe from file
lgbm_final_full_fit <- lightsnip::lgbm_load(paths$output$workflow_fit$local)
lgbm_final_full_recipe <- readRDS(paths$output$workflow_recipe$local)

if (shap_enable || comp_enable) {
  message("Loading assessment data for SHAP and comp calculation")

  # Load the input data used for assessment. This is the universe of CARDs (not
  # PINs) that need values. Will use the the trained model to calc SHAP values
  assessment_data <- as_tibble(read_parquet(paths$input$assessment$local))

  # Run the saved recipe on the assessment data to format it for prediction
  assessment_data_prepped <- recipes::bake(
    object = lgbm_final_full_recipe,
    new_data = assessment_data,
    all_predictors()
  )
}

if (comp_enable) {
  message("Loading predicted values for comp calculation")

  assessment_card <- read_parquet(paths$output$assessment_card$local) %>%
    as_tibble()
}




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Calculate SHAP Values -----------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

if (shap_enable) {
  message("Calculating SHAP values")

  # Calculate a SHAP value for each observation for each feature in the
  # assessment data. Uses lightgbm's built-in method (predcontrib = TRUE)
  shap_values <- predict(
    object = lgbm_final_full_fit$fit,
    newdata = as.matrix(assessment_data_prepped),
    type = "contrib"
  )

  # Convert the SHAP value output from a matrix to a tibble and add column names
  shap_values_tbl <- shap_values %>%
    as_tibble(.name_repair = "unique") %>%
    purrr::set_names(c(
      colnames(assessment_data_prepped),
      "pred_card_shap_baseline_fmv"
    ))

  # Keep only the SHAP value columns from predictors + any ID and partition
  # columns, then add run ID and write to file
  shap_values_final <- assessment_data %>%
    select(
      meta_year, meta_pin, meta_card_num,
      township_code = meta_township_code
    ) %>%
    bind_cols(shap_values_tbl) %>%
    select(
      meta_year, meta_pin, meta_card_num, pred_card_shap_baseline_fmv,
      all_of(params$model$predictor$all), township_code
    ) %>%
    write_parquet(paths$output$shap$local)
} else {
  # If SHAP creation is disabled, we still need to write an empty stub file
  # so DVC doesn't complain
  arrow::write_parquet(data.frame(), paths$output$shap$local)
}




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4. Calculate Feature Importance ----------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Calculating feature importance metrics")

# Calculate feature importance using LightGBM's built-in method
lightgbm::lgb.importance(lgbm_final_full_fit$fit) %>%
  as_tibble() %>%
  rename(model_predictor_all_name = Feature) %>%
  rename_with(tolower, Gain:Frequency) %>%
  mutate(across(
    gain:frequency,
    ~ order(order(.x, decreasing = TRUE)),
    .names = "{.col}_rank"
  )) %>%
  rename_with(~ paste0(.x, "_value"), gain:frequency) %>%
  write_parquet(paths$output$feature_importance$local)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4. Calculate comps -----------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

if (comp_enable) {
  message("Calculating comps")

  # Calculate the leaf node assignments for every predicted value.
  # Due to integer overflow problems with leaf node assignment, we need to
  # chunk our data such that they are strictly less than the limit of 1073742
  # rows. More detail here: https://github.com/microsoft/LightGBM/issues/1884
  chunk_size <- 500000
  chunks <- split(
    assessment_data_prepped,
    ceiling(seq_along(assessment_data_prepped[[1]]) / chunk_size)
  )
  chunked_leaf_nodes <- chunks %>%
    map(\(chunk) {
      predict(
        object = lgbm_final_full_fit$fit,
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
  training_data <- read_parquet(paths$input$training$local) %>%
    filter(!ind_pin_is_multicard, !sv_is_outlier) %>%
    as_tibble()

  tree_weights <- extract_weights(
    model = lgbm_final_full_fit$fit,
    mean_sale_price = mean(training_data[["meta_sale_price"]]),
    metric = params$model$objective
  )

  # Get predicted values and leaf node assignments for the training data
  training_data_prepped <- recipes::bake(
    object = lgbm_final_full_recipe,
    new_data = training_data,
    all_predictors()
  )
  training_leaf_nodes <- predict(
    object = lgbm_final_full_fit$fit,
    newdata = as.matrix(training_data_prepped),
    type = "leaf"
  ) %>%
    as_tibble()
  training_leaf_nodes$predicted_value <- predict(
    object = lgbm_final_full_fit$fit,
    newdata = as.matrix(training_data_prepped)
  ) %>%
    # Round predicted values down for binning
    floor()

  # Get predicted values for the assessment set, which we already have in
  # the assessment card set
  leaf_nodes$predicted_value <- assessment_data %>%
    left_join(assessment_card, by = c("meta_pin", "meta_card_num")) %>%
    # Round predicted values down for binning
    mutate(pred_card_initial_fmv = floor(pred_card_initial_fmv)) %>%
    dplyr::pull("pred_card_initial_fmv")

  # Make sure that the leaf node tibbles are all integers, which is what
  # the comps algorithm expects
  leaf_nodes <- leaf_nodes %>% mutate_all(as.integer)
  training_leaf_nodes <- training_leaf_nodes %>% mutate_all(as.integer)

  # Do the comps calculation in Python because the code is simpler and faster
  message("Calling out to python/comps.py to perform comps calculation")
  comps_module <- import("python.comps")
  tryCatch(
    {
      comps <- comps_module$get_comps(
        leaf_nodes, training_leaf_nodes, tree_weights,
        num_comps = num_comps,
        num_price_bins = num_comp_price_bins
      )
    },
    error = function(e) {
      # Log the full Python traceback in case of an error
      print(py_last_error())
      stop("Encountered error in python/comps.py")
    }
  )
  # Correct for the fact that Python is 0-indexed by incrementing the
  # comp indexes by 1
  comps[[1]] <- comps[[1]] + 1

  # Translate comp indexes to PINs
  comps[[1]] <- comps[[1]] %>%
    mutate_all(\(idx_row) {
      training_data[idx_row, ]$meta_pin
    }) %>%
    cbind(
      pin = assessment_data$meta_pin,
      card = assessment_data$meta_card_num
    ) %>%
    relocate(pin, card) %>%
    rename_with(\(colname) gsub("comp_idx_", "comp_pin_", colname))

  # Combine the comp indexes and scores into one dataframe and write to a file
  cbind(comps[[1]], comps[[2]]) %>%
    write_parquet(paths$output$comp$local)
} else {
  # If comp creation is disabled, we still need to write an empty stub file
  # so DVC doesn't complain
  arrow::write_parquet(data.frame(), paths$output$comp$local)
}

# End the stage timer and write the time elapsed to a temporary file
tictoc::toc(log = TRUE)
bind_rows(tictoc::tic.log(format = FALSE)) %>%
  arrow::write_parquet(gsub("//*", "/", file.path(
    paths$intermediate$timing$local,
    "model_timing_interpret.parquet"
  )))

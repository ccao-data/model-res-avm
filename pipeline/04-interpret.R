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
  assessment_data <- as_tibble(read_parquet(paths$input$assessment$local)) %>%
    head(1000)

  # Run the saved recipe on the assessment data to format it for prediction
  assessment_data_prepped <- recipes::bake(
    object = lgbm_final_full_recipe,
    new_data = assessment_data,
    all_predictors()
  )
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
  # To do this, we need the training data so that we can compute base model
  # error
  training_data <- read_parquet(paths$input$training$local) %>%
    filter(!ind_pin_is_multicard, !sv_is_outlier) %>%
    as_tibble() %>%
    head(1000)

  tree_weights <- extract_weights(
    model = lgbm_final_full_fit$fit,
    train = training_data,
    outcome_col = "meta_sale_price",
  )

  # Do the comps calculation in Python because the code is simpler and faster
  comps_module <- import("python.comps")
  tryCatch(
    {
      comps <- comps_module$get_comps(
        leaf_nodes, tree_weights,
        n = as.integer(20)
      )
    },
    error = function(e) {
      # Log the full Python traceback in case of an error
      py_last_error()
    }
  )
  # Correct for the fact that Python is 0-indexed
  comps <- comps + 1

  # Translate comps to PINs before writing them out to a file
  comps %>%
    mutate_all(\(idx_row) assessment_data$meta_pin[idx_row]) %>%
    cbind(pin = assessment_data$meta_pin) %>%
    relocate(pin) %>%
    rename_with(\(colname) gsub("comp_", "comp_pin_", colname)) %>%
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

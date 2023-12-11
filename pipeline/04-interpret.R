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

if (shap_enable) {
  message("Loading assessment data for SHAP calculation")

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




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Calculate SHAP Values -----------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

if (shap_enable) {
  message("Calculating SHAP values")

  # Calculate a SHAP value for each observation for each feature in the
  # assessment data. Uses lightgbm's built-in method (predcontrib = TRUE)
  shap_values <- predict(
    object = lgbm_final_full_fit$fit,
    data = as.matrix(assessment_data_prepped),
    predcontrib = TRUE
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
# 4. Save leaf node assignments  -----------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

if (comp_enable) {
  message("Saving leaf node assignments")

  # Calculate the leaf node assignments for every predicted value
  leaf_nodes <- predict(
    object = lgbm_final_full_fit$fit,
    data = as.matrix(assessment_data_prepped),
    predleaf = TRUE
  ) %>%
    as_tibble() %>%
    mutate(id = row_number()) %>%
    write_parquet(paths$output$leaf_node$local)
} else {
  # If comp creation is disabled, we still need to write an empty stub file
  # so DVC doesn't complain
  arrow::write_parquet(data.frame(), paths$output$leaf_node$local)
}

# End the stage timer and write the time elapsed to a temporary file
tictoc::toc(log = TRUE)
bind_rows(tictoc::tic.log(format = FALSE)) %>%
  arrow::write_parquet(gsub("//*", "/", file.path(
    paths$intermediate$timing$local,
    "model_timing_interpret.parquet"
  )))

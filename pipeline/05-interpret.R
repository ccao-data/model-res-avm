# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Setup #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Start the script timer and clear logs from prior script
tictoc::tic.clearlog()
tictoc::tic("Interpret")

# Load the necessary libraries
options(scipen = 99)
library(arrow)
library(dplyr)
library(here)
library(recipes)
library(stringr)
library(tidyr)
library(tictoc)
source(here("R", "helpers.R"))

# Initialize a dictionary of file paths and URIs. See R/file_dict.csv
paths <- model_file_dict()

# Load the run type from the metadata file, if it exists
if (file.exists(paths$output$metadata$local)) {
  model_run_type <- read_parquet(paths$output$metadata$local, "run_type") %>%
    dplyr::pull(run_type)
} else {
  model_run_type <- "automated"
}




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Load Data #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Generate SHAP values only for candidate and final runs
if (interactive() && model_run_type %in% c("candidate", "final")) {

  # Load the final lightgbm model object and recipe from file
  lgbm_final_full_fit <- lightsnip::lgbm_load(paths$output$workflow_fit$local)
  lgbm_final_full_recipe <- readRDS(paths$output$workflow_recipe$local)
  
  # Load the input data used for assessment. This is the universe of
  # IMPROVEMENTs (not PINs) that need values. Use the the trained model
  # to get SHAP values
  assessment_data <- as_tibble(read_parquet(paths$input$assessment$local))
  
  # Run the saved recipe on the assessment data to format it for prediction
  assessment_data_prepped <- recipes::bake(
    object = lgbm_final_full_recipe,
    new_data = assessment_data,
    all_predictors()
  )
  
  # Load the improvement-level predictions from the previous (assess) stage
  assessment_data_pred <- as_tibble(read_parquet(
    file = paths$output$assessment$local,
    col_select = all_of(c(
      "meta_year", "meta_pin", "meta_class", "meta_card_num",
      "township_code", "initial_pred_fmv"
    ))
  ))
}




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Calculate SHAP Values #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Only calculate SHAP values for candidate and final runs
if (interactive() && model_run_type %in% c("candidate", "final")) {

  # Calculate a SHAP value for each observation for each feature in the
  # assessment data. Uses LightGBM's built-in methods 
  shap_values <- predict(
    object = lgbm_final_full_fit$fit,
    data = as.matrix(assessment_data_prepped),
    predcontrib = TRUE
  )
  
  # Convert the SHAP vals from a matrix to a tibble and add column names
  shap_values_tbl <- shap_values %>%
    as_tibble() %>%
    purrr::set_names(c(
      colnames(assessment_data_prepped),
      "initial_pred_baseline"
    ))
  
  # Combine the identify information from the original prediction data frame
  # with the SHAP value output, then save to file
  shap_values_final <- assessment_data_pred %>%
    bind_cols(shap_values_tbl) %>%
    relocate(initial_pred_baseline, .after = "initial_pred_fmv") %>%
    write_parquet(paths$output$shap$local)
}

# End the script timer and write the time elapsed to file
tictoc::toc(log = TRUE)
arrow::read_parquet(paths$intermediate$timing$local) %>%
  bind_rows(., tictoc::tic.log(format = FALSE)) %>%
  arrow::write_parquet(paths$intermediate$timing$local)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Setup ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Start the stage timer and clear logs from prior stage
tictoc::tic.clearlog()
tictoc::tic("Interpret")

# Load libraries and scripts
options(scipen = 99)
library(arrow)
library(dplyr)
library(here)
library(recipes)
library(stringr)
library(tidyr)
library(tictoc)
library(yaml)
source(here("R", "helpers.R"))

# Initialize a dictionary of file paths. See R/file_dict.csv for details
paths <- model_file_dict()

# Load the parameters file containing the run settings
params <- read_yaml("params.yaml")




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. Load Data -----------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load the final lightgbm model object and recipe from file
lgbm_final_full_fit <- lightsnip::lgbm_load(paths$output$workflow_fit$local)
lgbm_final_full_recipe <- readRDS(paths$output$workflow_recipe$local)

# Load the input data used for assessment. This is the universe of CARDs (not
# PINs) that need values. Will use the the trained model to retrieve SHAP values
assessment_data <- as_tibble(read_parquet(paths$input$assessment$local))

# Run the saved recipe on the assessment data to format it for prediction
assessment_data_prepped <- recipes::bake(
  object = lgbm_final_full_recipe,
  new_data = assessment_data,
  all_predictors()
)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Calculate SHAP Values -----------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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
    meta_year, meta_pin, meta_card_num, township_code = meta_township_code
  ) %>%
  bind_cols(shap_values_tbl) %>%
  select(
    meta_year, meta_pin, meta_card_num, pred_card_shap_baseline_fmv,
    all_of(params$model$predictor$all), township_code
  ) %>%
  write_parquet(paths$output$shap$local)

# End the stage timer and write the time elapsed to a temporary file
tictoc::toc(log = TRUE)
bind_rows(tictoc::tic.log(format = FALSE)) %>%
  arrow::write_parquet(gsub("//*", "/", file.path(
    paths$intermediate$timing$local,
    "model_timing_interpret.parquet"
  )))

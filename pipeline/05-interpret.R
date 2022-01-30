# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Setup ####
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

# Initialize a dictionary of file paths and URIs. See R/helpers.R
paths <- model_file_dict()

# Load the final lightgbm model object and recipe from file
lgbm_final_full_fit <- lightsnip::lgbm_load(paths$output$workflow_fit$local)
lgbm_final_full_recipe <- readRDS(paths$output$workflow_recipe$local)

# Load the input data used for assessment. This is the universe of IMPROVEMENTs
# (not PINs) that need values. Use the the trained model to get SHAP values
assessment_data <- read_parquet(paths$input$assessment$local) %>%
  as_tibble()

# Run the saved recipe on the assessment data to format it for prediction
assessment_data_prepped <- recipes::bake(
  object = lgbm_final_full_recipe,
  new_data = assessment_data,
  all_predictors()
)

# Load the improvement-level predictions from the previous (assess) stage
assessment_data_pred <- read_parquet(paths$output$assessment$local) %>%
  as_tibble()




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Calculate SHAP Values ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Calculate a SHAP value for each observation for each feature in the assessment
# data. Uses LightGBM's built-in methods 
shap_values <- predict(
  object = lgbm_final_full_fit$fit,
  data = as.matrix(assessment_data_prepped),
  predcontrib = TRUE
)

colnames(shap_values) <- c(
  colnames(assessment_data_prepped),
  "initial_pred_baseline"
)



assessment_data_pred %>%
  bind_cols(as_tibble(shap_values), shap_total = rowSums(shap_values)) %>%
  mutate(
    avg_plus_shap_equals_pred = round(shap_total, 1) == round(initial_pred_value, 1)
  ) %>%
  relocate(any_of(
    c("avg_initial_pred_value", "shap_total", "avg_plus_shap_equals_pred")
  ), .after = "initial_pred_value")


# End the script timer and write the time elapsed to file
tictoc::toc(log = TRUE)
arrow::read_parquet(paths$output$timing$local) %>%
  bind_rows(., tictoc::tic.log(format = FALSE)) %>%
  arrow::write_parquet(paths$output$timing$local)
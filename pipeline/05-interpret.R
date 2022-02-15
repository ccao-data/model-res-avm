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
source(here("R", "helpers.R"))

# Initialize a dictionary of file paths and S3 URIs. See R/file_dict.csv
paths <- model_file_dict()

# Load the metadata file containing the run settings
metadata <- read_parquet(paths$output$metadata$local)




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

# Combine the SHAP value for each feature with the actual feature value and name
# Then convert the data from wide (column per feature) to long (row per feature)
# so we can visualize it more easily
shap_values_final <- assessment_data %>%
  select(meta_pin, meta_card_num, township_code = meta_township_code) %>%
  bind_cols(
    shap_values_tbl %>%
      select(-pred_card_shap_baseline_fmv)
  ) %>%
  tidyr::pivot_longer(
    cols = all_of(metadata$model_predictor_all_name[[1]]),
    names_to = "model_predictor_name",
    values_to = "model_shap_value"
  ) %>%
  bind_cols(
    assessment_data %>%
      select(all_of(metadata$model_predictor_all_name[[1]])) %>%
      ccao::vars_recode(
        starts_with("char_"),
        type = "long",
        as_factor = FALSE
      ) %>%
      mutate(across(
        all_of(metadata$model_predictor_all_name[[1]]),
        as.character
      )) %>%
      tidyr::pivot_longer(
        cols = all_of(metadata$model_predictor_all_name[[1]]),
        names_to = "model_predictor_name",
        values_to = "model_predictor_value"
      ) %>%
      select(-model_predictor_name)
  ) %>%
  relocate(model_predictor_value, .after = "model_predictor_name") %>%
  mutate(run_id = metadata$run_id, year = metadata$model_assessment_year) %>%
  write_parquet(paths$output$shap$local)

# Extract the SHAP baseline (average of predictions on the training data) and
# append it to the card-level assessment output
read_parquet(paths$output$assessment_card$local) %>%
  mutate(
    pred_card_shap_baseline_fmv = shap_values_tbl$pred_card_shap_baseline_fmv[1]
  ) %>%
  write_parquet(paths$output$assessment_card$local)

# End the stage timer and append the time elapsed to a temporary file
tictoc::toc(log = TRUE)
arrow::read_parquet(paths$intermediate$timing$local) %>%
  bind_rows(., tictoc::tic.log(format = FALSE)) %>%
  arrow::write_parquet(paths$intermediate$timing$local)

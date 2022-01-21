# This script will upload all the locally stored objects created by a pipeline
# run. Uploaded objects will be renamed by their run_id and/or have the run_id
# inserted into their data in the left-most position.
#
# NOTE: This script requires CCAO employee access. See wiki for S3 credentials
# setup

# Load the necessary libraries
library(arrow)
library(aws.s3)
library(dplyr)
library(here)
library(tidyr)
library(tune)
source(here("R", "helpers.R"))

# Initialize a dictionary of file paths and URIs. See R/helpers.R
if (all(sapply(c("model_run_id", "model_run_id", "model_assessment_year"), exists))) {
  paths <- model_file_dict(
    model_s3_bucket,
    model_run_id,
    model_assessment_year
  )
} else {
  stop("This script should only be run from run.R!")
}


### 01-setup.R

# Upload run metadata
aws.s3::put_object(
  paths$output$metadata$local,
  paths$output$metadata$s3
)


### 02-train.R

# Upload objects with no need for alteration first
# Upload LGBM fit
aws.s3::put_object(
  paths$output$workflow$fit$local,
  paths$output$workflow$fit$s3
)

# Upload Tidymodels recipe
aws.s3::put_object(
  paths$output$workflow$recipe$local,
  paths$output$workflow$recipe$s3
)

# Upload the parameters object. Requires some cleaning since the Tidymodels
# output is stored as a nested data frame
# Get the best/chosen parameter run from cross-validation
model_best_parameter <- read_parquet(paths$output$parameter$local) %>%
  select_best(metric = "rmse") %>%
  ccao::model_lgbm_cap_num_leaves() %>%
  mutate(chosen_parameters = TRUE)

# Write the raw parameters object to S3 in case we need to use it later
aws.s3::put_object(
  paths$output$parameter$local,
  paths$output$parameter$s3_raw
)

# Clean and unnest the raw parameters data, then write directly to S3
model_parameter <- read_parquet(paths$output$parameter$local) %>%
  tidyr::unnest(cols = .metrics) %>%
  mutate(
    run_id = model_run_id,
    num_leaves_capped = num_leaves > (2 ^ tree_depth) - 1
  ) %>%
  ccao::model_lgbm_cap_num_leaves() %>%
  left_join(
    rename(., notes = .notes) %>%
      tidyr::unnest(cols = notes) %>%
      rename(notes = .notes)
  ) %>%
  left_join(model_best_parameter) %>%
  select(-.notes) %>%
  rename_with(~ gsub("^\\.", "", .x)) %>%
  tidyr::pivot_wider(names_from = "metric", values_from = "estimate") %>%
  select(
    run_id, iteration = iter, configuration = config, fold_id = id,
    chosen_parameters, rmse:mape,
    mtry:num_leaves, num_leaves_capped, notes
  ) %>%
  mutate(chosen_parameters = tidyr::replace_na(chosen_parameters, FALSE)) %>%
  write_parquet(paths$output$parameter$s3)


### 03-evaluate.R

# Upload test set peformance
aws.s3::put_object(
  paths$output$performance$test$local,
  paths$output$performance$test$s3
)


### 05-timing.R

# Upload run timings 
aws.s3::put_object(
  paths$output$timing$local,
  paths$output$timing$s3
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Setup ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# This script will upload all the locally stored objects created by a pipeline
# run. Uploaded objects will be renamed by their run_id and/or have the run_id
# inserted into their data in the left-most position.
#
# NOTE: This script requires CCAO employee access. See wiki for S3 credentials
# setup

# Load the necessary libraries
library(arrow)
library(aws.s3)
library(ccao)
library(dplyr)
library(here)
library(lubridate)
library(paws.application.integration)
library(tidyr)
library(tune)
source(here("R", "helpers.R"))

# Whether or not to upload model artifacts (objects, results, parameters) to S3
# Only available to CCAO employees via interactive sessions (unless overridden)
if (interactive() | as.logical(Sys.getenv("MODEL_UPLOAD_TO_S3_OVERRIDE", FALSE))) {
  model_upload_to_s3 <- as.logical(Sys.getenv("MODEL_UPLOAD_TO_S3", TRUE))
} else {
  model_upload_to_s3 <- FALSE
}

# Disable CV for non-interactive sessions (GitLab CI) unless overridden
if (interactive() | as.logical(Sys.getenv("MODEL_CV_ENABLE_OVERRIDE", FALSE))) {
  model_cv_enable <- as.logical(Sys.getenv("MODEL_CV_ENABLE", TRUE))
} else {
  model_cv_enable <- FALSE
}

# Location of files uploaded to S3. AWS_S3_WAREHOUSE_BUCKET should be set in
# your root .Renviron file (if you are a CCAO employee)
model_s3_bucket <- file.path(Sys.getenv("AWS_S3_WAREHOUSE_BUCKET"), "model")

# Initialize a dictionary of file paths and URIs. See R/helpers.R
paths <- model_file_dict()

# Get a list of all pipeline outputs that should exist for upload
output_paths <- unlist(paths)[
  grepl("local", names(unlist(paths)), fixed = TRUE) &
  grepl("output", names(unlist(paths)), fixed = TRUE) &
  !grepl("parameter", names(unlist(paths)), fixed = TRUE)
]

# Check whether all conditions are met for upload:
#   1. All output files must exist, except...
#   2. The parameter file, which may not exists if CV is disabled, so check that
#   3. S3 upload is enabled
upload_bool <- all(sapply(output_paths, file.exists)) & (
  (!file.exists(paths$output$parameter$local) & !model_cv_enable) |
  (file.exists(paths$output$parameter$local) & model_cv_enable) &
  model_upload_to_s3
)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Upload ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Only run upload if above conditions are met
if (upload_bool) {
  
  # Load run info from the saved metadata file. This will let us rename the
  # uploaded files by run ID and determine what to upload
  metadata <- read_parquet(paths$output$metadata$local)
  model_run_id <- metadata$run_id[1]
  model_assessment_year <- metadata$model_assessment_year[1]
  
  # Initialize a new dictionary of file paths AND S3 URIs. See R/helpers.R
  paths <- model_file_dict(
    model_s3_bucket = model_s3_bucket,
    model_run_id = model_run_id,
    model_assessment_year = model_assessment_year
  )


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
  
  # Upload the parameters objects if CV was enabled. Requires some cleaning
  # since the Tidymodels output is stored as a nested data frame
  if (model_cv_enable) {
  
    # Get the best/chosen parameter run from cross-validation
    model_best_parameter <- read_parquet(paths$output$parameter$local) %>%
      select_best(metric = "rmse") %>%
      mutate(chosen_parameters = TRUE)
    
    # Write the raw parameters object to S3 in case we need to use it later
    aws.s3::put_object(
      paths$output$parameter$local,
      paths$output$parameter$s3_raw
    )
  
    # Clean and unnest the raw parameters data, then write directly to S3
    read_parquet(paths$output$parameter$local) %>%
      tidyr::unnest(cols = .metrics) %>%
      mutate(run_id = model_run_id) %>%
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
        chosen_parameters, rmse:mape, mtry:cat_smooth, notes
      ) %>%
      mutate(chosen_parameters = tidyr::replace_na(chosen_parameters, FALSE)) %>%
      write_parquet(paths$output$parameter$s3)
  }
  
  ### 03-evaluate.R
  
  # Upload test set performance
  read_parquet(paths$output$performance$test$local) %>%
    mutate(run_id = model_run_id) %>%
    relocate(run_id, .before = everything()) %>%
    write_parquet(paths$output$performance$test$s3)
  
  # Upload assessment set performance
  read_parquet(paths$output$performance$assessment$local) %>%
    mutate(run_id = model_run_id) %>%
    relocate(run_id, .before = everything()) %>%
    write_parquet(paths$output$performance$assessment$s3)
  
  
  ### 05-timing.R
  
  # Upload run timings 
  aws.s3::put_object(
    paths$output$timing$local,
    paths$output$timing$s3
  )
  
  
  ### SNS Notification
  
  # If SNS ARN is available, notify subscribers via email upon run completion
  if (!is.na(Sys.getenv("AWS_SNS_ARN_MODEL_STATUS", unset = NA))) {
    pipeline_sns <- paws.application.integration::sns()
    
    # Get pipeline total run time from file
    pipeline_sns_total_time <- arrow::read_parquet(paths$output$timing$local) %>%
      mutate(dur = lubridate::seconds_to_period(round(overall_sec_elapsed))) %>%
      pull(dur)
    
    # Get overall stats by township for the triad of interest, collapsed into
    # a plaintext table
    pipeline_sns_results <- arrow::read_parquet(
      paths$output$performance$assessment$local,
      col_select = c("geography_type", "geography_id", "by_class", "cod")
    ) %>%
      filter(
        town_get_triad(geography_id, name = TRUE) == Sys.getenv("MODEL_TRIAD"),
        !by_class, geography_type == "township_code"
      ) %>%
      mutate(township_name = town_convert(geography_id)) %>%
      select(cod, township_name) %>%
      mutate(across(where(is.numeric), round, 2)) %>%
      arrange(cod) %>%
      knitr::kable(format = "rst") %>%
      as.character() %>%
      .[!grepl("=", .)] %>%
      paste0(collapse = "\n")
    
    # Publish to SNS
    pipeline_sns$publish(
      Subject = paste("Model Run Complete:", model_run_id),
      Message = paste0(
        "Model run: ", model_run_id, " complete\n",
        "Finished in: ", pipeline_sns_total_time, "\n\n",
        pipeline_sns_results
      ),
      TopicArn = Sys.getenv("AWS_SNS_ARN_MODEL_STATUS")
    )
  }
}

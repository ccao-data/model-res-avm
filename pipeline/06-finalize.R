# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Setup ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# This script will upload all the locally stored objects created by a pipeline
# run. Uploaded objects will be renamed by their run_id and/or have the run_id
# inserted into their data in the left-most position.

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
library(stringr)
library(tidyr)
library(tune)
source(here("R", "helpers.R"))

# Initialize a dictionary of file paths and URIs. See R/helpers.R
paths <- model_file_dict()




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Save Timings ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Convert input timing logs to data frame, then save to file
if (file.exists(paths$output$metadata$local) &
    file.exists(paths$intermediate$timing$local)) {
  
  # Load info from the saved metadata file to append run ID and start time
  metadata <- read_parquet(paths$output$metadata$local)
  
  # Load the built timing file and munge it into a more useful format
  read_parquet(paths$intermediate$timing$local) %>%
    mutate(
      run_id = metadata$run_id[1],
      run_start_timestamp = metadata$run_start_timestamp[1],
      elapsed = round(toc - tic, 2),
      stage = paste0(tolower(stringr::word(msg, 1)), "_sec_elapsed")
    ) %>%
    select(-c(tic:toc, msg)) %>%
    tidyr::pivot_wider(
      id_cols = c(run_id, run_start_timestamp),
      names_from = stage,
      values_from = elapsed
    ) %>%
    mutate(overall_sec_elapsed = rowSums(across(ends_with("_sec_elapsed")))) %>%
    write_parquet(paths$output$timing$local)
  
  # Clear any remaining logs from tictoc
  tictoc::tic.clearlog()
}




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Upload Prep ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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

# Get a list of all pipeline outputs that should exist for upload
output_paths <- unlist(paths)[
  grepl("local", names(unlist(paths)), fixed = TRUE) &
  grepl("output", names(unlist(paths)), fixed = TRUE) &
  !grepl("parameter", names(unlist(paths)), fixed = TRUE) &
  !grepl("assessment", names(unlist(paths)), fixed = TRUE) &
  !grepl("shap", names(unlist(paths)), fixed = TRUE) 
]

# Check whether all conditions are met for upload:
#   1. All output files must exist, except...
#   2. The parameter file, which may not exist if CV is disabled
#   3. The assessment performance data exists OR this is an automated run
#   4. The SHAP values exist OR this is an automated run
#   5. S3 upload is enabled
upload_all_files <- all(sapply(output_paths, file.exists))
upload_search <- (
  (!file.exists(paths$output$parameter_search$local) & !model_cv_enable) |
  (file.exists(paths$output$parameter_search$local) & model_cv_enable)
)
upload_assessment <- (
  (!file.exists(paths$output$performance_assessment$local) & !interactive()) |
  (file.exists(paths$output$performance_assessment$local) & interactive())
)
upload_interpret <- (
  (!file.exists(paths$output$shap$local) & !interactive()) |
  (file.exists(paths$output$shap$local) & interactive())
)
upload_bool <- upload_all_files &
  upload_search &
  upload_assessment &
  upload_interpret &
  model_upload_to_s3

# Retrieve hard-coded model hyperparameters from .Renviron
model_param_objective <- as.character(
  Sys.getenv("MODEL_PARAM_OBJECTIVE", "regression")
)
model_param_num_iterations <- as.integer(
  Sys.getenv("MODEL_PARAM_NUM_ITERATIONS", 500)
)
model_param_learning_rate <- as.numeric(
  Sys.getenv("MODEL_PARAM_LEARNING_RATE", 0.1)
)
model_param_validation_prop <- as.numeric(
  Sys.getenv("MODEL_PARAM_VALIDATION_PROP", 0.1)
)
model_param_validation_metric <- as.character(
  Sys.getenv("MODEL_PARAM_VALIDATION_METRIC", "rmse")
)
model_param_link_max_depth <- as.logical(
  Sys.getenv("MODEL_PARAM_LINK_MAX_DEPTH", TRUE)
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
  
  # Initialize a dictionary of file paths and URIs. See R/file_dict.csv
  paths <- model_file_dict(
    run_id = model_run_id,
    year = model_assessment_year
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
    paths$output$workflow_fit$local,
    paths$output$workflow_fit$s3
  )
  
  # Upload Tidymodels recipe
  aws.s3::put_object(
    paths$output$workflow_recipe$local,
    paths$output$workflow_recipe$s3
  )
  
  # Always write the chosen/final parameters to S3. Get parameters not tuned in
  # CV from the environment
  arrow::read_parquet(paths$output$parameter_final$local) %>%
    rename(any_of(c("configuration" = ".config"))) %>%
    mutate(
      run_id = model_run_id,
      objective_func = model_param_objective,
      num_iterations = model_param_num_iterations,
      learning_rate = model_param_learning_rate,
      validation_prop = model_param_validation_prop,
      validation_metric = model_param_validation_metric,
      link_max_depth = model_param_link_max_depth,
      max_depth = ifelse(
        link_max_depth,
        as.integer(floor(log2(num_leaves)) + add_to_linked_depth),
        max_depth
      )
    ) %>%
    relocate(any_of(c("run_id", "configuration")), .before = everything()) %>%
    arrow::write_parquet(paths$output$parameter_final$s3)
  
  # Upload the parameter search objects if CV was enabled. Requires some 
  # cleaning since the Tidymodels output is stored as a nested data frame
  if (model_cv_enable) {
  
    # Write the raw parameters object to S3 in case we need to use it later
    aws.s3::put_object(
      paths$output$parameter_raw$local,
      paths$output$parameter_raw$s3
    )
  
    # Clean and unnest the raw parameters data, then write directly to S3
    read_parquet(paths$output$parameter_raw$local) %>%
      tidyr::unnest(cols = .metrics) %>%
      mutate(run_id = model_run_id) %>%
      left_join(
        rename(., notes = .notes) %>%
          tidyr::unnest(cols = notes) %>%
          rename(notes = .notes)
      ) %>%
      select(-.notes) %>%
      rename_with(~ gsub("^\\.", "", .x)) %>%
      tidyr::pivot_wider(names_from = "metric", values_from = "estimate") %>%
      relocate(
        all_of(c(
          "run_id", "iteration" = "iter",
          "configuration" = "config", "fold_id" = "id"
        )),
        .before = everything()
      ) %>%
      relocate(notes, .after = everything()) %>%
      dplyr::select(-any_of(c("estimator"))) %>%
      write_parquet(paths$output$parameter_search$s3)
    
    # Write the parameter range with run ID to a separate table
    read_parquet(paths$output$parameter_range$local) %>%
      mutate(run_id = model_run_id) %>%
      relocate(run_id, .before = everything()) %>%
      write_parquet(paths$output$parameter_range$s3)
  }
  
  ### 03-assess.R
  
  # Upload assessment values if running locally. Assessed values are per
  # improvement, so the output is very large. Therefore, we use arrow to
  # partition the data by year, run, and township
  if (interactive()) {
    read_parquet(paths$output$assessment$local) %>%
      mutate(run_id = model_run_id, year = model_assessment_year) %>%
      group_by(year, run_id, township_code) %>%
      write_partitions_to_s3(paths$output$assessment$s3, overwrite = TRUE)
  }
  
  
  ### 04-evaluate.R
  
  # Upload test set performance
  read_parquet(paths$output$performance_test$local) %>%
    mutate(run_id = model_run_id) %>%
    relocate(run_id, .before = everything()) %>%
    write_parquet(paths$output$performance_test$s3)
  read_parquet(paths$output$performance_quantile_test$local) %>%
    mutate(run_id = model_run_id) %>%
    relocate(run_id, .before = everything()) %>%
    write_parquet(paths$output$performance_quantile_test$s3)
  
  # Upload assessment set performance if running locally
  if (interactive()) {
    read_parquet(paths$output$performance_assessment$local) %>%
      mutate(run_id = model_run_id) %>%
      relocate(run_id, .before = everything()) %>%
      write_parquet(paths$output$performance_assessment$s3)
    read_parquet(paths$output$performance_quantile_assessment$local) %>%
      mutate(run_id = model_run_id) %>%
      relocate(run_id, .before = everything()) %>%
      write_parquet(paths$output$performance_quantile_assessment$s3)
  }
  
  
  ### 05-interpret.R
  
  # Upload SHAP values if running locally. SHAP values are per improvement, so
  # the output is very large. Therefore, we use arrow to partition the data by
  # year, run, and township
  if (interactive()) {
    read_parquet(paths$output$shap$local) %>%
      mutate(run_id = model_run_id, year = model_assessment_year) %>%
      group_by(year, run_id, township_code) %>%
      write_partitions_to_s3(paths$output$shap$s3, overwrite = TRUE)
  }
  
  # Upload finalized timings
  aws.s3::put_object(
    paths$output$timing$local,
    paths$output$timing$s3
  )
  
  
  
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ##### Wrap Up ####
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  # If assessments and SHAP values were uploaded, trigger a Glue crawler to find
  # new partitions
  if (interactive()) {
    glue_srv <- paws.analytics::glue()
    glue_srv$start_crawler("ccao-data-warehouse-model-crawler")
  }
  
  # If SNS ARN is available, notify subscribers via email upon run completion
  if (!is.na(Sys.getenv("AWS_SNS_ARN_MODEL_STATUS", unset = NA))) {
    pipeline_sns <- paws.application.integration::sns(
      config = list(region = Sys.getenv("AWS_REGION"))
    )
    
    # Get pipeline total run time from file
    pipeline_sns_total_time <- arrow::read_parquet(paths$output$timing$local) %>%
      mutate(dur = lubridate::seconds_to_period(round(overall_sec_elapsed))) %>%
      dplyr::pull(dur)
    
    # Get overall stats by township for the triad of interest, collapsed into
    # a plaintext table
    pipeline_sns_results <- arrow::read_parquet(
      paths$output$performance_test$local,
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

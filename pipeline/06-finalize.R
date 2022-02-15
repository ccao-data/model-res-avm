#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Setup ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# NOTE: This script requires CCAO employee access. See wiki for S3 credentials
# setup and multi-factor auth help

# Load libraries and scripts
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

# Initialize a dictionary of file paths and S3 URIs. See R/file_dict.csv
paths <- model_file_dict()

# Load the metadata file containing the run settings
metadata <- read_parquet(paths$output$metadata$local)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. Save Timings --------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Convert the intermediate timing logs to a wide data frame, then save to file
if (file.exists(paths$output$metadata$local) &
  file.exists(paths$intermediate$timing$local)) {

  # Load the timing file and reshape it from long to wide
  read_parquet(paths$intermediate$timing$local) %>%
    mutate(
      run_id = metadata$run_id,
      run_start_timestamp = metadata$run_start_timestamp,
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




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Prepare to Upload ---------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Whether or not to upload model artifacts (objects, results, parameters) to S3
model_upload_to_s3 <- as.logical(Sys.getenv("MODEL_UPLOAD_TO_S3", FALSE))

# Get a list of all pipeline outputs that should exist for any run
output_paths <- unlist(paths)[
  grepl("local", names(unlist(paths))) &
    grepl("output", names(unlist(paths))) &
    !grepl(
      "assessment|shap|parameter_range|parameter_search|parameter_raw",
      names(unlist(paths))
    )
]

# Check whether all conditions are met for upload:
#   1. All output files must exist, except...
#   2. Some parameter files, which may not exist if CV is disabled
#   3. The assessment values (candidate and final runs only)
#   4. The SHAP values (candidate and final runs only)
#   5. S3 upload is enabled

# See R/file_dict.csv for a breakdown of what files are created under certain
# run types and circumstances
upload_all_files <- all(sapply(output_paths, file.exists))
upload_bool <- upload_all_files & model_upload_to_s3




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4. Upload --------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Only run upload if above conditions are met
if (upload_bool) {

  # Initialize a dictionary of file paths and S3 URIs, this time with paths
  # specific to the run ID and year
  paths <- model_file_dict(
    run_id = metadata$run_id,
    year = metadata$model_assessment_year
  )


  ## 4.1. Setup ----------------------------------------------------------------

  # Upload run metadata
  aws.s3::put_object(
    paths$output$metadata$local,
    paths$output$metadata$s3
  )


  # 4.2. Train -----------------------------------------------------------------

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

  # Upload the final run hyperparameters
  aws.s3::put_object(
    paths$output$parameter_final$local,
    paths$output$parameter_final$s3
  )

  # Upload the parameter search objects if CV was enabled. Requires some
  # cleaning since the Tidymodels output is stored as a nested data frame
  if (metadata$model_cv_enable) {

    # Upload the raw parameters object to S3 in case we need to use it later
    aws.s3::put_object(
      paths$output$parameter_raw$local,
      paths$output$parameter_raw$s3
    )
    
    # Upload the parameter ranges used for CV
    aws.s3::put_object(
      paths$output$parameter_range$local,
      paths$output$parameter_range$s3
    )
    
    # Clean and unnest the raw parameters data, then write the results to S3
    read_parquet(paths$output$parameter_raw$local) %>%
      tidyr::unnest(cols = .metrics) %>%
      mutate(run_id = metadata$run_id) %>%
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
          "run_id",
          "iteration" = "iter",
          "configuration" = "config", "fold_id" = "id"
        )),
        .before = everything()
      ) %>%
      relocate(notes, .after = everything()) %>%
      dplyr::select(-any_of(c("estimator"))) %>%
      write_parquet(paths$output$parameter_search$s3)
  }


  # 4.3. Assess ----------------------------------------------------------------

  # Upload PIN and card-level values for candidate or final runs. These outputs
  # are very large, so to help reduce file size and improve query performance we
  # partition them by year, run ID, and township
  if (model_run_type %in% c("candidate", "final")) {
    read_parquet(paths$output$assessment_card$local) %>%
      group_by(year, run_id, township_code) %>%
      write_partitions_to_s3(paths$output$assessment_card$s3, overwrite = TRUE)
    read_parquet(paths$output$assessment_pin$local) %>%
      group_by(year, run_id, township_code) %>%
      write_partitions_to_s3(paths$output$assessment_pin$s3, overwrite = TRUE)
  }


  # 4.4. Evaluate --------------------------------------------------------------

  # Upload test set performance
  aws.s3::put_object(
    paths$output$performance_test$local,
    paths$output$performance_test$s3
  )
  aws.s3::put_object(
    paths$output$performance_quantile_test$local,
    paths$output$performance_quantile_test$s3
  )

  # Upload assessment set performance if a candidate or final run
  if (model_run_type %in% c("candidate", "final")) {
    aws.s3::put_object(
      paths$output$performance_assessment$local,
      paths$output$performance_assessment$s3
    )
    aws.s3::put_object(
      paths$output$performance_quantile_assessment$local,
      paths$output$performance_quantile_assessment$s3
    )
  }


  # 4.5. Interpret -------------------------------------------------------------

  # Upload SHAP values if a candidate or final run. SHAP values are 1 row per
  # card per feature, so the output is very large (100M+ rows). Therefore, we
  # partition the data by year, run, and township
  if (model_run_type %in% c("candidate", "final")) {
    read_parquet(paths$output$shap$local) %>%
      group_by(year, run_id, township_code) %>%
      write_partitions_to_s3(paths$output$shap$s3, overwrite = TRUE)
  }


  # 4.6. Miscellaneous ---------------------------------------------------------

  # Upload finalized timings
  aws.s3::put_object(
    paths$output$timing$local,
    paths$output$timing$s3
  )
}




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 5. Wrap-Up -------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Only run AWS stuff when actually uploading. This will run a Glue crawler to
# update schemas and send an email to any SNS subscribers
if (upload_bool) {

  # If assessments and SHAP values were uploaded, trigger a Glue crawler to find
  # any new partitions
  if (metadata$run_type %in% c("candidate", "final")) {
    glue_srv <- paws.analytics::glue()
    glue_srv$start_crawler("ccao-model-results-crawler")
  }

  # If SNS ARN is available, notify subscribers via email upon run completion
  if (!is.na(Sys.getenv("AWS_SNS_ARN_MODEL_STATUS", unset = NA))) {
    pipeline_sns <- paws.application.integration::sns(
      config = list(region = Sys.getenv("AWS_REGION"))
    )

    # Get pipeline total run time from file
    pipeline_sns_total_time <- read_parquet(paths$output$timing$local) %>%
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
      Subject = paste("Model Run Complete:", metadata$run_id),
      Message = paste0(
        "Model run: ", metadata$run_id, " complete\n",
        "Finished in: ", pipeline_sns_total_time, "\n\n",
        pipeline_sns_results
      ),
      TopicArn = Sys.getenv("AWS_SNS_ARN_MODEL_STATUS")
    )
  }
}

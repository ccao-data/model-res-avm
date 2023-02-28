#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Setup ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# NOTE: This script requires CCAO employee access. See wiki for S3 credentials
# setup and multi-factor authentication help

# Load libraries and scripts
suppressPackageStartupMessages({
  library(arrow)
  library(aws.s3)
  library(ccao)
  library(dplyr)
  library(here)
  library(lubridate)
  library(paws.application.integration)
  library(purrr)
  library(tidyr)
  library(tune)
  library(yaml)
})
source(here("R", "helpers.R"))

# Initialize a dictionary of file paths. See misc/file_dict.csv for details
paths <- model_file_dict()

# Load the parameters file containing the run settings
params <- read_yaml("params.yaml")

# Override CV toggle, SHAP toggle, and run_type, used for CI or limited runs
cv_enable <- as.logical(
  Sys.getenv("CV_ENABLE_OVERRIDE", unset = params$toggle$cv_enable)
)
shap_enable <- as.logical(
  Sys.getenv("SHAP_ENABLE_OVERRIDE", unset = params$toggle$shap_enable)
)
run_type <- as.character(
  Sys.getenv("RUN_TYPE_OVERRIDE", unset = params$run_type)
)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. Save Metadata -------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Saving run metadata")

## 2.1. Run Info ---------------------------------------------------------------

# Generate a random identifier for this run. This will serve as the primary key/
# identifier for in perpetuity. See ?ccao_generate_id for details
run_id <- ccao::ccao_generate_id()

# Get the current timestamp for when the run ended
run_end_timestamp <- lubridate::now()

# Get the commit of the current reference
git_commit <- git2r::revparse_single(git2r::repository(), "HEAD")

# For full runs, use the run note included in params.yaml, otherwise use the
# commit message
if (run_type == "full") {
  run_note <- params$run_note
} else {
  run_note <- gsub("\n", "", git_commit$message)
}


## 2.2. DVC Hashes -------------------------------------------------------------

# Read the MD5 hash of each input dataset. These are created by DVC and used to
# version and share the input data
dvc_md5_df <- bind_rows(read_yaml("dvc.lock")$stages$ingest$outs) %>%
  mutate(path = paste0("dvc_md5_", gsub("input/|.parquet", "", path))) %>%
  select(path, md5) %>%
  pivot_wider(names_from = path, values_from = md5)


## 2.3. Parameters -------------------------------------------------------------

# Save most parameters from params.yaml to a metadata file, along with
# run info, git stuff, etc.
metadata <- tibble::tibble(
  run_id = run_id,
  run_end_timestamp = run_end_timestamp,
  run_type = run_type,
  run_note = run_note,
  git_sha_short = substr(git_commit$sha, 1, 8),
  git_sha_long = git_commit$sha,
  git_message = gsub("\n", "", git_commit$message),
  git_author = git_commit$author$name,
  git_email = git_commit$author$email,
  assessment_year = params$assessment$year,
  assessment_date = params$assessment$date,
  assessment_triad = params$assessment$triad,
  assessment_group = params$assessment$group,
  assessment_data_year = params$assessment$data_year,
  input_min_sale_year = params$input$min_sale_year,
  input_max_sale_year = params$input$max_sale_year,
  input_complex_match_exact = list(params$input$complex$match_exact),
  input_complex_match_fuzzy_name = list(
    names(params$input$complex$match_fuzzy)
  ),
  input_complex_match_fuzzy_value = list(
    as.numeric(params$input$complex$match_fuzzy)
  ),
  ratio_study_far_year = params$ratio_study$far_year,
  ratio_study_far_stage = params$ratio_study$far_stage,
  ratio_study_far_column = params$ratio_study$far_column,
  ratio_study_near_year = params$ratio_study$near_year,
  ratio_study_near_stage = params$ratio_study$near_stage,
  ratio_study_near_column = params$ratio_study$near_column,
  ratio_study_num_quantile = list(params$ratio_study$num_quantile),
  cv_enable = cv_enable,
  cv_num_folds = params$cv$num_folds,
  cv_initial_set = params$cv$initial_set,
  cv_max_iterations = params$cv$max_iterations,
  cv_no_improve = params$cv$no_improve,
  cv_split_prop = params$cv$split_prop,
  cv_best_metric = params$cv$best_metric,
  pv_multicard_yoy_cap = params$pv$multicard_yoy_cap,
  pv_land_pct_of_total_cap = params$pv$land_pct_of_total_cap,
  pv_round_break = list(params$pv$round_break),
  pv_round_to_nearest = list(params$pv$round_to_nearest),
  pv_round_type = params$pv$round_type,
  model_predictor_id_count = length(params$model$predictor$id),
  model_predictor_id_name = list(params$model$predictor$id),
  model_predictor_all_count = length(params$model$predictor$all),
  model_predictor_all_name = list(params$model$predictor$all),
  model_predictor_categorical_count = length(params$model$predictor$categorical),
  model_predictor_categorical_name = list(params$model$predictor$categorical)
) %>%
  bind_cols(dvc_md5_df) %>%
  relocate(starts_with("dvc_id_"), .after = "input_complex_match_fuzzy_value") %>%
  arrow::write_parquet(paths$output$metadata$local)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Save Timings --------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Saving run timings")

# Filter ensure we only get timing files for stages that actually ran
if (run_type == "full") {
  timings <- list.files(
    paste0(paths$intermediate$timing, "/"),
    full.names = TRUE
  )
} else {
  timings <- list.files(
    paste0(paths$intermediate$timing, "/"),
    pattern = "train|evaluate",
    full.names = TRUE
  )
}

# Convert the intermediate timing logs to a wide data frame, then save to file
timings_df <- purrr::map_dfr(timings, read_parquet) %>%
  mutate(
    run_id = run_id,
    run_end_timestamp = run_end_timestamp,
    elapsed = round(toc - tic, 2),
    stage = paste0(tolower(stringr::word(msg, 1)), "_sec_elapsed"),
    order = recode(
      msg,
      "Train" = "01", "Assess" = "02",
      "Evaluate" = "03", "Interpret" = "04"
    )
  ) %>%
  arrange(order) %>%
  select(-c(tic:toc, msg)) %>%
  tidyr::pivot_wider(
    id_cols = c(run_id, run_end_timestamp),
    names_from = stage,
    values_from = elapsed
  ) %>%
  mutate(overall_sec_elapsed = rowSums(across(ends_with("_sec_elapsed")))) %>%
  mutate(across(ends_with("_sec_elapsed"), function(x) round(x, 2))) %>%
  write_parquet(paths$output$timing$local)

# Clear any remaining logs from tictoc
tictoc::tic.clearlog()




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4. Upload --------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Uploading run artifacts")

# Only upload files if explicitly enabled
if (params$toggle$upload_to_s3) {
  # Initialize a dictionary of paths AND S3 URIs specific to the run ID and year
  paths <- model_file_dict(
    run_id = run_id,
    year = params$assessment$year
  )


  ## 4.1. Train ----------------------------------------------------------------

  # Upload lightgbm fit
  aws.s3::put_object(
    paths$output$workflow_fit$local,
    paths$output$workflow_fit$s3
  )

  # Upload Tidymodels recipe
  aws.s3::put_object(
    paths$output$workflow_recipe$local,
    paths$output$workflow_recipe$s3
  )

  # Upload finalized run parameters
  read_parquet(paths$output$parameter_final$local) %>%
    mutate(run_id = run_id) %>%
    relocate(run_id) %>%
    # Max_depth is set by lightsnip if link_max_depth is true, so we need to
    # back out its value. Otherwise, use whichever value is chosen by CV
    mutate(max_depth = {
      if (link_max_depth) {
        as.integer(floor(log2(num_leaves)) + add_to_linked_depth)
      } else if (!is.null(.[["max_depth"]])) {
        .$max_depth
      } else {
        NULL
      }
    }) %>%
    write_parquet(paths$output$parameter_final$s3)

  # Upload the test set predictions
  read_parquet(paths$output$test_card$local) %>%
    mutate(run_id = run_id) %>%
    relocate(run_id) %>%
    write_parquet(paths$output$test_card$s3)

  # Upload the parameter search objects if CV was enabled. Requires some
  # cleaning since the Tidymodels output is stored as a nested data frame
  if (cv_enable) {
    message("Uploading cross-validation artifacts")
    
    # Upload the raw parameters object to S3 in case we need to use it later
    aws.s3::put_object(
      paths$output$parameter_raw$local,
      paths$output$parameter_raw$s3
    )

    # Upload the parameter ranges used for CV
    read_parquet(paths$output$parameter_range$local) %>%
      mutate(run_id = run_id) %>%
      relocate(run_id) %>%
      write_parquet(paths$output$parameter_range$s3)

    # Clean and unnest the raw parameters data, then write the results to S3
    bind_cols(
      read_parquet(paths$output$parameter_raw$local) %>%
        tidyr::unnest(cols = .metrics) %>%
        mutate(run_id = run_id) %>%
        left_join(
          rename(., notes = .notes) %>%
            tidyr::unnest(cols = notes) %>%
            rename(notes = note)
        ) %>%
        select(-.notes) %>%
        rename_with(~ gsub("^\\.", "", .x)) %>%
        tidyr::pivot_wider(names_from = "metric", values_from = "estimate") %>%
        relocate(
          all_of(c(
            "run_id",
            "iteration" = "iter",
            "configuration" = "config", "fold_id" = "id"
          ))
        ) %>%
        relocate(c(location, type, notes), .after = everything()),
      read_parquet(paths$output$parameter_raw$local) %>%
        tidyr::unnest(cols = .extracts) %>%
        tidyr::unnest(cols = .extracts) %>%
        dplyr::select(num_iterations = .extracts)
    ) %>%
      dplyr::select(-any_of(c("estimator")), -extracts) %>%
      write_parquet(paths$output$parameter_search$s3)
  }


  # 4.2. Assess ----------------------------------------------------------------
  message("Uploading final assessment results")

  # Upload PIN and card-level values for full runs. These outputs are very
  # large, so to help reduce file size and improve query performance we
  # partition them by year, run ID, and township
  if (run_type == "full") {
    read_parquet(paths$output$assessment_card$local) %>%
      mutate(run_id = run_id, year = params$assessment$year) %>%
      group_by(year, run_id, township_code) %>%
      arrow::write_dataset(
        path = paths$output$assessment_card$s3,
        format = "parquet",
        hive_style = TRUE,
        compression = "snappy"
      )
    read_parquet(paths$output$assessment_pin$local) %>%
      mutate(run_id = run_id, year = params$assessment$year) %>%
      group_by(year, run_id, township_code) %>%
      arrow::write_dataset(
        path = paths$output$assessment_pin$s3,
        format = "parquet",
        hive_style = TRUE,
        compression = "snappy"
      )
  }


  # 4.3. Evaluate --------------------------------------------------------------
  
  # Upload test set performance
  message("Uploading test set evaluation")
  read_parquet(paths$output$performance_test$local) %>%
    mutate(run_id = run_id) %>%
    relocate(run_id) %>%
    write_parquet(paths$output$performance_test$s3)
  read_parquet(paths$output$performance_quantile_test$local) %>%
    mutate(run_id = run_id) %>%
    relocate(run_id) %>%
    write_parquet(paths$output$performance_quantile_test$s3)

  # Upload assessment set performance if a full run
  if (run_type == "full") {
    message("Uploading assessment set evaluation")
    read_parquet(paths$output$performance_assessment$local) %>%
      mutate(run_id = run_id) %>%
      relocate(run_id) %>%
      write_parquet(paths$output$performance_assessment$s3)
    read_parquet(paths$output$performance_quantile_assessment$local) %>%
      mutate(run_id = run_id) %>%
      relocate(run_id) %>%
      write_parquet(paths$output$performance_quantile_assessment$s3)
  }


  # 4.4. Interpret -------------------------------------------------------------

  # Upload SHAP values if a full run. SHAP values are one row per card and one
  # column per feature, so the output is very large. Therefore, we partition
  # the data by year, run, and township
  if (run_type == "full" && shap_enable) {
    message("Uploading SHAP values")
    read_parquet(paths$output$shap$local) %>%
      mutate(run_id = run_id, year = params$assessment$year) %>%
      group_by(year, run_id, township_code) %>%
      arrow::write_dataset(
        path = paths$output$shap$s3,
        format = "parquet",
        hive_style = TRUE,
        compression = "snappy"
      )
  }
  
  # Upload feature importance metrics
  if (run_type == "full") {
    message("Uploading feature importance metrics")
    read_parquet(paths$output$feature_importance$local) %>%
      mutate(run_id = run_id) %>%
      relocate(run_id) %>%
      write_parquet(paths$output$feature_importance$s3)
  }


  # 4.5. Finalize --------------------------------------------------------------
  message("Uploading run metadata and timings")

  # Upload metadata
  aws.s3::put_object(
    paths$output$metadata$local,
    paths$output$metadata$s3
  )

  # Upload finalized timings
  aws.s3::put_object(
    paths$output$timing$local,
    paths$output$timing$s3
  )
}




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 5. Wrap-Up -------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# This will run a Glue crawler to update schemas and send an email to any SNS
# subscribers. Only run when actually uploading
if (params$toggle$upload_to_s3) {
  message("Sending run email and running model crawler")
  
  # If assessments and SHAP values were uploaded, trigger a Glue crawler to find
  # any new partitions
  if (run_type == "full") {
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
        tolower(town_get_triad(geography_id, name = TRUE)) ==
          params$assessment$triad,
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
      Subject = paste("Model Run Complete:", run_id),
      Message = paste0(
        "Model run: ", run_id, " complete\n",
        "Finished in: ", pipeline_sns_total_time, "\n\n",
        pipeline_sns_results
      ),
      TopicArn = Sys.getenv("AWS_SNS_ARN_MODEL_STATUS")
    )
  }
}

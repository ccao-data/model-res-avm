#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Setup ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# NOTE: See DESCRIPTION for library dependencies and R/setup.R for
# variables used in each pipeline stage

# NOTE: This script requires CCAO employee access. See wiki for S3 credentials
# setup and multi-factor authentication help

# Load libraries, helpers, and recipes from files
purrr::walk(list.files("R/", "\\.R$", full.names = TRUE), source)

# Load various parameters as defined in the `finalize` step
metadata <- read_parquet(paths$output$metadata$local)
run_id <- metadata$run_id




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. Upload --------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Uploading run artifacts")

# Only upload files if explicitly enabled
if (upload_enable) {
  # Initialize a dictionary of paths AND S3 URIs specific to the run ID and year
  paths <- model_file_dict(
    run_id = run_id,
    year = params$assessment$working_year
  )


  ## 2.1. Train ----------------------------------------------------------------

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
    mutate(run_id = !!run_id) %>%
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
    mutate(run_id = !!run_id) %>%
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
      mutate(run_id = !!run_id) %>%
      relocate(run_id) %>%
      write_parquet(paths$output$parameter_range$s3)

    # Clean and unnest the raw parameters data, then write the results to S3
    bind_cols(
      read_parquet(paths$output$parameter_raw$local) %>%
        tidyr::unnest(cols = .metrics) %>%
        mutate(run_id = !!run_id) %>%
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
      dplyr::select(-any_of(c("estimator", "trees")), -extracts) %>%
      write_parquet(paths$output$parameter_search$s3)
  }


  # 2.2. Assess ----------------------------------------------------------------
  message("Uploading final assessment results")

  # Upload PIN and card-level values. These outputs are very large, so to help
  # reduce file size and improve query performance we partition them by year,
  # run ID, and township
  read_parquet(paths$output$assessment_card$local) %>%
    mutate(run_id = !!run_id, year = params$assessment$working_year) %>%
    group_by(year, run_id, township_code) %>%
    arrow::write_dataset(
      path = paths$output$assessment_card$s3,
      format = "parquet",
      hive_style = TRUE,
      compression = "snappy"
    )
  read_parquet(paths$output$assessment_pin$local) %>%
    mutate(run_id = !!run_id, year = params$assessment$working_year) %>%
    group_by(year, run_id, township_code) %>%
    arrow::write_dataset(
      path = paths$output$assessment_pin$s3,
      format = "parquet",
      hive_style = TRUE,
      compression = "snappy"
    )



  # 2.3. Evaluate --------------------------------------------------------------

  # Upload test set performance
  message("Uploading test set evaluation")
  read_parquet(paths$output$performance_test$local) %>%
    mutate(run_id = !!run_id) %>%
    relocate(run_id) %>%
    write_parquet(paths$output$performance_test$s3)
  read_parquet(paths$output$performance_quantile_test$local) %>%
    mutate(run_id = !!run_id) %>%
    relocate(run_id) %>%
    write_parquet(paths$output$performance_quantile_test$s3)

  message("Uploading test linear baseline")
  read_parquet(paths$output$performance_test_linear$local) %>%
    mutate(run_id = !!run_id) %>%
    relocate(run_id) %>%
    write_parquet(paths$output$performance_test_linear$s3)
  read_parquet(paths$output$performance_quantile_test_linear$local) %>%
    mutate(run_id = !!run_id) %>%
    relocate(run_id) %>%
    write_parquet(paths$output$performance_quantile_test_linear$s3)

  # Upload assessment set performance
  message("Uploading assessment set evaluation")
  read_parquet(paths$output$performance_assessment$local) %>%
    mutate(run_id = !!run_id) %>%
    relocate(run_id) %>%
    write_parquet(paths$output$performance_assessment$s3)
  read_parquet(paths$output$performance_quantile_assessment$local) %>%
    mutate(run_id = !!run_id) %>%
    relocate(run_id) %>%
    write_parquet(paths$output$performance_quantile_assessment$s3)


  # 2.4. Interpret -------------------------------------------------------------

  # Upload SHAP values. One row per card and on column per feature, so the
  # output is very large. Therefore, we partition the data by
  # year, run ID, and township
  if (shap_enable) {
    message("Uploading SHAP values")
    read_parquet(paths$output$shap$local) %>%
      mutate(run_id = !!run_id, year = params$assessment$working_year) %>%
      group_by(year, run_id, township_code) %>%
      arrow::write_dataset(
        path = paths$output$shap$s3,
        format = "parquet",
        hive_style = TRUE,
        compression = "snappy"
      )
  }

  # Upload feature importance metrics
  message("Uploading feature importance metrics")
  read_parquet(paths$output$feature_importance$local) %>%
    mutate(run_id = !!run_id) %>%
    relocate(run_id) %>%
    write_parquet(paths$output$feature_importance$s3)

  # Upload comps
  if (comp_enable) {
    message("Uploading comps")
    read_parquet(paths$output$comp$local) %>%
      mutate(run_id = !!run_id, year = params$assessment$working_year) %>%
      group_by(year, run_id) %>%
      arrow::write_dataset(
        path = paths$output$comp$s3,
        format = "parquet",
        hive_style = TRUE,
        compression = "snappy"
      )
  }


  # 2.5. Finalize --------------------------------------------------------------
  message("Uploading run metadata, timings, and reports")

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

  # Upload performance report
  aws.s3::put_object(
    paths$output$report_performance$local,
    paths$output$report_performance$s3
  )
}




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Crawl and notify ----------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# This will run a Glue crawler to update schemas and send an email to any SNS
# subscribers. Only run when actually uploading
if (upload_enable) {
  message("Sending run email and running model crawler")

  # If values were uploaded, trigger a Glue crawler to find any new partitions
  glue_srv <- paws.analytics::glue(
    config = list(region = Sys.getenv("AWS_REGION"))
  )
  tryCatch(
    glue_srv$start_crawler("ccao-model-results-crawler"),
    error = function(e) message(e),
    warning = function(w) message(e)
  )

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

    # Get a link to the uploaded Quarto report
    report_path_parts <- strsplit(
      paths$output$report_performance$s3[1],
      "/"
    )[[1]]
    report_bucket <- report_path_parts[3]
    report_path <- report_path_parts[4:length(report_path_parts)] %>%
      paste(collapse = "/")
    # Use direct link to the console instead of to the object so that we don't
    # have to bother with signed URLs
    report_url <- paste0(
      "https://s3.console.aws.amazon.com/s3/object/",
      "{report_bucket}/{report_path}?region=us-east-1&tab=overview"
    ) %>%
      glue::glue()

    # Publish to SNS
    pipeline_sns$publish(
      Subject = paste("Model Run Complete:", run_id),
      Message = paste0(
        "Model run: ", run_id, " complete\n",
        "Finished in: ", pipeline_sns_total_time, "\n",
        "Run note: ", metadata$run_note, "\n\n",
        "Report link: ", report_url, "\n\n",
        pipeline_sns_results
      ),
      TopicArn = Sys.getenv("AWS_SNS_ARN_MODEL_STATUS")
    )
  }
}

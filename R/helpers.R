# Function to generate a dictionary list of file names, local paths,
# and mirrored S3 location URIs from file_dict.csv
model_file_dict <- function(run_id = NULL, year = NULL) {
  env <- environment()
  wd <- here::here()
  suppressPackageStartupMessages(library(magrittr))

  # Convert flat dictionary file to nested list
  dict <- read.csv(
    here::here("misc", "file_dict.csv"),
    colClasses = c("character", "character", "numeric", rep("character", 9)),
    na.strings = ""
  ) %>%
    dplyr::mutate(
      s3 = as.character(purrr::map_if(
        path_s3, ~ !is.na(.x), glue::glue,
        .envir = env, .na = NULL, .null = NA_character_
      )),
      s3 = ifelse(!is.na(s3), file.path(paste0("s3://", s3_bucket), s3), NA),
      local = ifelse(!is.na(path_local), file.path(wd, path_local), NA)
    ) %>%
    dplyr::select(type, name, s3, local) %>%
    split(., .$type) %>%
    purrr::map(., ~ split(.x, .x$name, drop = TRUE)) %>%
    purrr::map(., ~ purrr::map(.x, function(x) {
      as.list(x)[!is.na(x) & names(x) %in% c("s3", "local")]
    }))

  return(dict)
}

# Get a vector of S3 paths to the artifacts for a given model run
model_get_s3_artifacts_for_run <- function(run_id, year) {
  # Get paths of all run objects based on the file dictionary
  paths <- model_file_dict(run_id, year)
  s3_objs <- grep("s3://", unlist(paths), value = TRUE)
  bucket <- strsplit(s3_objs[1], "/")[[1]][3]

  # First get anything partitioned only by year
  s3_objs_limited <- grep(
    ".parquet$|.zip$|.rds|.html$", s3_objs,
    value = TRUE
  ) %>%
    unname()

  # Next get the prefix of anything partitioned by year and run_id
  s3_objs_dir_path <- file.path(
    grep(
      ".parquet$|.zip$|.rds$",
      s3_objs,
      value = TRUE, invert = TRUE
    ),
    glue::glue("year={year}"),
    glue::glue("run_id={run_id}")
  )
  s3_objs_dir_path <- gsub(paste0("s3://", bucket, "/"), "", s3_objs_dir_path)
  s3_objs_dir_path <- gsub("//", "/", s3_objs_dir_path)
  s3_objs_w_run_id <- s3_objs_dir_path %>%
    purrr::map(~ aws.s3::get_bucket_df(bucket, .x)$Key) %>%
    unlist() %>%
    purrr::map_chr(~ glue::glue("s3://{bucket}/{.x}"))

  return(c(s3_objs_limited, s3_objs_w_run_id))
}

# Used to delete erroneous, incomplete, or otherwise unwanted runs
# Use with caution! Deleted models are retained for a period of time before
# being permanently deleted
model_delete_run <- function(run_id, year) {
  model_get_s3_artifacts_for_run(run_id, year) %>%
    purrr::walk(aws.s3::delete_object)
}

# Used to fetch a run's output from S3 and populate it locally. Useful for
# running reports and performing local troubleshooting
model_fetch_run <- function(run_id, year) {
  tictoc::tic(paste0("Fetched run: ", run_id))

  paths <- model_file_dict(run_id, year)
  s3_objs <- grep("s3://", unlist(paths), value = TRUE)
  bucket <- strsplit(s3_objs[1], "/")[[1]][3]

  for (path in paths$output) {
    is_dataset <- endsWith(path$s3, "/")
    if (is_dataset) {
      dataset_path <- paste0(path$s3, "year=", year, "/run_id=", run_id, "/")
      message("Now fetching: ", dataset_path)

      obj_path <- paste0(dataset_path, "township_code=10/part-0.parquet")
      if (aws.s3::object_exists(obj_path)) {
        df <- dplyr::collect(arrow::open_dataset(dataset_path))
        arrow::write_parquet(df, path$local)
      } else {
        warning(path$local, " does not exist for this run")
      }
    } else {
      message("Now fetching: ", path$s3)
      if (aws.s3::object_exists(path$s3, bucket = bucket)) {
        aws.s3::save_object(path$s3, bucket = bucket, file = path$local)
      } else {
        warning(path$local, " does not exist for this run")
      }
    }
  }
  tictoc::toc()
}

# Extract the number of iterations that occurred before early stopping during
# cross-validation. See the tune::tune_bayes() argument `extract`
extract_num_iterations <- function(x) {
  fit <- workflows::extract_fit_engine(x)
  evals <- purrr::pluck(fit, "record_evals", "validation", 1, "eval")
  length(evals)
}

# Extract weights for model features based on feature importance. Assumes that
# the model was trained with the `valids` parameter set such that error metrics
# are saved for each tree on the model$record_evals attribute. The output
# weights are useful for computing comps using leaf node assignments
extract_weights <- function(model, train, outcome_col, metric = "rmse") {
  train_lgb <- lgb.Dataset(as.matrix(train), label = train[[outcome_col]])

  # Get the initial error for base model before the first tree
  set_field(train_lgb, "init_score", as.matrix(train[[outcome_col]]))
  initial_predictions <- get_field(train_lgb, "init_score")
  message("initial_predictions length:")
  message(length(initial_predictions))
  init_score <- mean(initial_predictions)
  message("init_score:")
  message(init_score)

  # Index into the errors list, and un-list so it is a flat/1dim list
  record_evals <- model$record_evals
  if (is.null(record_evals)) {
    stop("Model is missing required record_evals; was it trained with valids?")
  }
  message("record_evals names:")
  message(names(record_evals$valids))
  message(glue::glue("record_evals {metric} length:"))
  message(length(record_evals$valids[[metric]]$eval))
  errors <- unlist(record_evals$valids[[metric]]$eval)
  message("errors length:")
  message(length(errors))
  errors <- c(init_score, errors)
  diff_in_errors <- diff(errors, 1, 1)
  message("diff_in_errors length:")
  message(length(diff_in_errors))

  # Take proportion of diff in errors over total diff in
  # errors from all trees
  weights <- diff_in_errors / sum(diff_in_errors)
  message("weights length:")
  message(length(weights))

  return(weights)
}

# Given the result of a CV search, get the number of iterations from the
# result set with the best performing hyperparameters
select_iterations <- function(tune_results, metric, type = "mean") {
  stopifnot(type %in% c("mean", "median", "max"))
  func <- switch(type,
    mean = mean,
    median = median,
    max = max
  )

  tune_results %>%
    dplyr::select(id, .metrics, .extracts) %>%
    tidyr::unnest(cols = .metrics) %>%
    dplyr::filter(.metric == params$cv$best_metric) %>%
    dplyr::select(-.extracts) %>%
    dplyr::left_join(
      tune_results %>%
        tidyr::unnest(cols = .extracts) %>%
        tidyr::unnest(cols = .extracts) %>%
        dplyr::select(!dplyr::where(is.list), -.config, -.iter)
    ) %>%
    dplyr::inner_join(tune::select_best(tune_results, metric = metric)) %>%
    suppressMessages() %>%
    dplyr::summarize(num_iterations = ceiling(func(.extracts)))
}

# Silly copy of ccao::vars_recode to convert text versions of categoricals back
# to numbers
var_encode <- function(data,
                       cols = dplyr::everything(),
                       dict = ccao::vars_dict) {
  var <- "var_code"

  dict_long <- dict %>%
    dplyr::filter(
      .data$var_type == "char" &
        .data$var_data_type == "categorical"
    ) %>%
    dplyr::select(
      dplyr::starts_with("var_name_"),
      .data$var_code:.data$var_value_short
    ) %>%
    tidyr::pivot_longer(
      dplyr::starts_with("var_name_"),
      names_to = "var_type",
      values_to = "var_name"
    ) %>%
    dplyr::distinct(
      .data$var_code,
      .data$var_value, .data$var_value_short, .data$var_name
    )

  dplyr::mutate(
    data,
    dplyr::across(dplyr::all_of(cols), function(x, y = dplyr::cur_column()) {
      if (y %in% dict_long$var_name) {
        var_rows <- which(dict_long$var_name == y)
        idx <- match(x, dict_long$var_value[var_rows])
        out <- dict_long[[var]][var_rows][idx]
        return(out)
      } else {
        return(x)
      }
    })
  )
}

# Yardstick doesn't currently include MdAPE, so we'll add it here
mdape_vec <- function(truth, estimate, case_weights = NULL, na_rm = TRUE) {
  yardstick::check_numeric_metric(truth, estimate, case_weights)

  if (na_rm) {
    result <- yardstick::yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
  } else if (yardstick::yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  errors <- abs((truth - estimate) / truth)
  out <- median(errors)
  out <- out * 100
  out
}

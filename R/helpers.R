# Function to generate a dictionary list of file names, local paths,
# and mirrored S3 location URIs from file_dict.csv
model_file_dict <- function(run_id = NULL, year = NULL) {
  env <- environment()
  wd <- here::here()
  library(magrittr)
  
  # Convert flat dictionary file to nested list
  dict <- readr::read_csv(
      here::here("misc", "file_dict.csv"),
      col_types = readr::cols()
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


# Used to delete erroneous, incomplete, or otherwise unwanted runs
# Use with caution! Deleted models are retained for a period of time before
# being permanently deleted
model_delete_run <- function(run_id, year) {
  
  # Get paths of all run objects based on the file dictionary
  paths <- model_file_dict(run_id, year)
  s3_objs <- grep("s3://", unlist(paths), value = TRUE)
  bucket <- strsplit(s3_objs[1], "/")[[1]][3]
  
  # First get anything partitioned only by year
  s3_objs_limited <- grep(".parquet$|.zip$|.rds$", s3_objs, value = TRUE)
  
  # Next get the prefix of anything partitioned by year and run_id
  s3_objs_dir_path <- file.path(
    grep(
      ".parquet$|.zip$|.rds$",
      s3_objs, value = TRUE, invert = TRUE
    ),
    glue::glue("year={year}"),
    glue::glue("run_id={run_id}")
  )
  s3_objs_dir_path <- gsub(paste0("s3://", bucket, "/"), "", s3_objs_dir_path)
  s3_objs_dir_path <- gsub("//", "/", s3_objs_dir_path)
  s3_objs_w_run_id <- unlist(purrr::map(
    s3_objs_dir_path,
    ~ aws.s3::get_bucket_df(bucket, .x)$Key
  ))
  
  # Delete current version of objects
  purrr::walk(s3_objs_limited, aws.s3::delete_object)
  purrr::walk(s3_objs_w_run_id, aws.s3::delete_object, bucket = bucket)
}


# Extract the number of iterations that occurred before early stopping during
# cross-validation. See the tune::tune_bayes() argument `extract`
extract_num_iterations <- function(x) {
  fit <- workflows::extract_fit_engine(x)
  evals <- fit$record_evals$validation$rmse$eval
  length(evals)
}


# Given the result of a CV search, get the max number of iterations from the
# result set with the best performing hyperparameters
select_max_iterations <- function(tune_results, metric) {
  dplyr::bind_cols(
    tune_results %>%
      dplyr::select(id, .metrics, .extracts) %>%
      tidyr::unnest(cols = .metrics) %>%
      dplyr::filter(.metric == params$cv$best_metric) %>%
      dplyr::select(-.extracts),
    tune_results %>%
      tidyr::unnest(cols = .extracts) %>%
      tidyr::unnest(cols = .extracts) %>%
      dplyr::select(.extracts)
  ) %>%
    dplyr::inner_join(
      tune::select_best(tune_results, metric = metric),
      by = ".config"
    ) %>%
    dplyr::summarize(num_iterations = max(.extracts))
}


# Modified rolling origin forecast split function. Splits the training data into
# a cumulatively expanding time window. The window contains the training data
# and the most recent X% of the window is validation data (they overlap! see
# issue #82). See README for more information
rolling_origin_pct_split <- function(data, order_col, split_col, assessment_pct) {
  data <- dplyr::arrange(data, {{ order_col }})
  split_sc <- data %>%
    dplyr::group_by({{ split_col }}) %>%
    dplyr::group_size() %>%
    cumsum()
  starts <- rep(1, length(split_sc))
  in_idx <- mapply(seq, starts, split_sc, SIMPLIFY = FALSE)
  out_idx <- lapply(in_idx, function(x) {
    len <- length(x)
    start_idx <- len - floor(len * assessment_pct) + 1
    seq(start_idx, len)
  })
  indices <- mapply(rsample:::merge_lists, in_idx, out_idx, SIMPLIFY = FALSE)
  split_objs <- purrr::map(
    indices, rsample::make_splits, data = data, class = "rof_split"
  )
  split_objs <- list(
    splits = split_objs,
    id = names0(length(split_objs), "Slice")
  )
  new_rset(
    splits = split_objs$splits,
    ids = split_objs$id, 
    subclass = c("rolling_origin", "rset")
  )
}


# Silly copy of ccao::vars_recode to convert text versions of categoricals back
# to numbers
var_encode <- function(data, cols = dplyr::everything(), dict = ccao::vars_dict) {
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

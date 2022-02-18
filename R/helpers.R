# Function to translate the ratio study year and stage into a column name within
# used in the input data
get_rs_col_name <- function(data_year,  study_year,  study_stage) {
  stopifnot(study_stage %in% c("board",  "mailed",  "certified"))
  year_diff <- as.numeric(data_year) - as.numeric(study_year)
  time_prefix <- ifelse(
    year_diff > 0, 
    paste0(as.integer(year_diff),  "yr_pri"), 
    NA_character_
  )
  
  paste(na.omit(c("meta",  time_prefix,  study_stage,  "tot")),  collapse = "_")
}


# Function to generate a dictionary list of file names, local paths,
# and mirrored S3 location URIs from file_dict.csv
model_file_dict <- function(run_id = NULL, year = NULL) {
  env <- environment()
  wd <- here::here()
  library(magrittr)
  
  # Convert flat dictionary file to nested list
  dict <- readr::read_csv(
      here::here("R", "file_dict.csv"),
      col_types = readr::cols()
    ) %>%
    dplyr::mutate(
      s3 = as.character(purrr::map_if(
        path_s3, ~ !is.na(.x), glue::glue,
        .envir = env, .na = NULL, .null = NA_character_
      )),
      s3 = ifelse(!is.na(s3), file.path(paste0("s3://", s3_bucket), s3), NA),
      local = ifelse(!is.na(path_local), file.path(wd, path_local), NA),
      dvc = ifelse(!is.na(path_dvc), file.path(wd, path_dvc), NA)
    ) %>%
    dplyr::select(type, name, s3, local, dvc) %>%
    split(., .$type) %>%
    purrr::map(., ~ split(.x, .x$name, drop = TRUE)) %>%
    purrr::map(., ~ purrr::map(.x, function(x) {
      as.list(x)[!is.na(x) & names(x) %in% c("s3", "local", "dvc")]
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
  s3_objs_w_run_id <- unlist(purrr::map(
    s3_objs_dir_path,
    ~ aws.s3::get_bucket_df(bucket, .x)$Key
  ))
  
  # Delete current version of objects
  purrr::walk(s3_objs_limited, aws.s3::delete_object)
  purrr::walk(s3_objs_w_run_id, aws.s3::delete_object, bucket = bucket)
}


# Slightly rewrite of arrow::write_dataset with more verbose output and
# faster runtime
write_partitions_to_s3 <- function(df, s3_output_path, overwrite = FALSE) {
  if (!dplyr::is.grouped_df(df)) {
    warning("Input data must contain grouping vars for partitioning")
  }
  
  dplyr::group_walk(df, ~ {
    partitions_df <- purrr::map_dfr(
      .y, replace_na, "__HIVE_DEFAULT_PARTITION__"
    )
    partition_path <- paste0(purrr::map2_chr(
      names(partitions_df),
      partitions_df[1, ],
      function(x, y) paste0(x, "=", y)
    ), collapse = "/")
    remote_path <- file.path(
      s3_output_path, partition_path, "part-0.parquet"
    )
    remote_path <- gsub("//*", "/", remote_path)
    remote_path <- gsub("s3:/*", "s3://", remote_path)
    if (!object_exists(remote_path) | overwrite) {
      message("Now uploading: ", partition_path)
      tmp_file <- tempfile(fileext = ".parquet")
      arrow::write_parquet(.x, tmp_file, compression = "snappy")
      aws.s3::put_object(tmp_file, remote_path)
    }
  })
}
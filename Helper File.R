model_fetch_run_subset <- function(run_id, year, paths, append_run_id = TRUE) {
  tictoc::tic(paste0("Fetched run: ", run_id))

  construct_s3_path <- function(lockfile) {
    paste0(
      "s3://ccao-data-dvc-us-east-1/files/md5/",
      substr(lockfile, 1, 2), "/",
      substr(lockfile, 3, nchar(lockfile))
    )
  }

  s3_objs <- grep("s3://", unlist(paths), value = TRUE)
  bucket <- strsplit(s3_objs[1], "/")[[1]][3]

  for (path in paths) {
    if (!is.null(path$lockfile)) {
      # Handle dynamic S3 paths for assessment data
      s3_path_assessment <- construct_s3_path(path$lockfile)
      assessment_data <- s3read_using(FUN = read_parquet, object = s3_path_assessment)
      variable_name <- basename(path$local)
      if (append_run_id) {
        variable_name <- paste0(variable_name, "_", run_id)
      }
      assign(variable_name, assessment_data, envir = .GlobalEnv)
    } else {
      is_directory <- endsWith(path$s3, "/")
      if (is_directory) {
        partitioned_by_run <- endsWith(path$s3, paste0("run_id=", run_id, "/"))
        if (partitioned_by_run) {
          dir_path <- path$s3
        } else {
          dir_path <- paste0(path$s3, "year=", year, "/run_id=", run_id, "/")
        }

        message("Now fetching: ", dir_path)
        objs_prefix <- sub(paste0("s3://", bucket, "/"), "", dir_path)
        objs <- aws.s3::get_bucket_df(bucket, objs_prefix)
        objs <- dplyr::filter(objs, Size > 0)
        if (nrow(objs) > 0 && all(endsWith(objs$Key, ".parquet"))) {
          df <- dplyr::collect(arrow::open_dataset(dir_path))
          variable_name <- basename(path$local)
          if (append_run_id) {
            variable_name <- paste0(variable_name, "_", run_id)
          }
          assign(variable_name, df, envir = .GlobalEnv)
        } else if (nrow(objs) > 0) {
          for (key in objs$Key) {
            message("Now fetching: ", key)
            variable_name <- file.path(path$local, basename(key))
            variable_name <- basename(gsub("//", "/", variable_name))
            obj <- aws.s3::get_object(key, bucket = bucket)
            if (append_run_id) {
              variable_name <- gsub("\\.parquet$", paste0("_", run_id, ".parquet"), variable_name)
            }
            assign(variable_name, obj, envir = .GlobalEnv)
          }
        } else {
          warning(path$local, " does not exist for this run")
        }
      } else {
        message("Now fetching: ", path$s3)
        if (aws.s3::object_exists(path$s3, bucket = bucket)) {
          obj <- aws.s3::get_object(path$s3, bucket = bucket)
          variable_name <- basename(path$local)
          if (append_run_id) {
            variable_name <- gsub("\\.parquet$", paste0("_", run_id, ".parquet"), variable_name)
          }
          assign(variable_name, obj, envir = .GlobalEnv)
        } else {
          warning(path$local, " does not exist for this run")
        }
      }
    }
  }

  tictoc::toc()
}

subset_paths <- list(paths$output$assessment_pin)
print(subset_paths)

# Calling the function directly with the subset paths
model_fetch_run_subset("2024-07-09-cool-takuya", "2024", subset_paths, TRUE)

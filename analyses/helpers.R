model_fetch_run_subset <- function(
    run_id, year, analyses_paths, append_run_id = FALSE) {
  s3_objs <- grep("s3://", unlist(analyses_paths$output), value = TRUE)
  bucket <- strsplit(s3_objs[1], "/")[[1]][3]

  data_list <- list()

  for (analyses_path in analyses_paths$output) {
    is_directory <- endsWith(analyses_path$s3, "/")
    if (is_directory) {
      partitioned_by_run <- endsWith(
        analyses_path$s3,
        paste0("run_id=", run_id, "/")
      )
      dir_path <- if (partitioned_by_run) {
        analyses_path$s3
      } else {
        paste0(analyses_path$s3, "year=", year, "/run_id=", run_id, "/")
      }

      message("Now fetching directory: ", dir_path)
      objs_prefix <- sub(paste0("s3://", bucket, "/"), "", dir_path)
      objs <- aws.s3::get_bucket_df(bucket, objs_prefix)
      objs <- dplyr::filter(objs, Size > 0)

      combined_data <- purrr::map_dfr(objs$Key, function(key) {
        message("Now fetching file: ", key)
        local_temp_path <- file.path(tempdir(), basename(key))
        aws.s3::save_object(key, bucket = bucket, file = local_temp_path)
        arrow::read_parquet(local_temp_path)
      })

      if (nrow(objs) > 0) {
        data_key <- if (append_run_id) {
          paste0(analyses_path$key, "_", run_id)
        } else {
          analyses_path$key
        }
        data_list[[data_key]] <- combined_data
      } else {
        warning(analyses_path$key, " does not exist for this run")
      }
    } else {
      message("Now fetching file: ", analyses_path$s3)
      if (aws.s3::object_exists(analyses_path$s3, bucket = bucket)) {
        local_temp_path <- file.path(tempdir(), basename(analyses_path$s3))
        aws.s3::save_object(
          analyses_path$s3,
          bucket = bucket, file = local_temp_path
        )
        data_key <- if (append_run_id) {
          paste0(analyses_path$key, "_", run_id)
        } else {
          analyses_path$key
        }
        data_list[[data_key]] <- arrow::read_parquet(local_temp_path)
      } else {
        warning(analyses_path$key, " does not exist for this run")
      }
    }
  }

  return(data_list)
}


rename_var <- function(var_name, suffix, new_suffix) {
  if (exists(var_name) && is.data.frame(get(var_name))) {
    if (grepl(paste0("_", suffix, "$"), var_name)) {
      new_name <- sub(paste0("_", suffix, "$"), new_suffix, var_name)
      assign(new_name, get(var_name), envir = .GlobalEnv)
      rm(list = var_name, envir = .GlobalEnv)
    }
  }
}

clean_column_values <- function(df, column_name) {
  df[[column_name]] <- df[[column_name]] %>%
    gsub("^meta_|^prox_|^other_|^loc_|^char_|^acs5|^acs_|^ccao_", "", .) %>%
    gsub("_", " ", .) %>%
    stringr::str_to_title()
  return(df)
}

s3_data_download <- function(dvc_md5_assessment_data) {
  # Define the S3 path for assessment data
  s3_path <- paste0(
    "s3://ccao-data-dvc-us-east-1/files/md5/",
    substr(dvc_md5_assessment_data, 1, 2), "/",
    substr(dvc_md5_assessment_data, 3, nchar(dvc_md5_assessment_data))
  )

  # Read and return the parquet data
  read_parquet(s3_path)
}

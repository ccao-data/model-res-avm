library(aws.s3)
library(dplyr)
library(arrow)
library(tictoc)

fetch_analyses <- function(run_id, year) {
  tictoc::tic(paste0("Fetched run: ", run_id))

  analyses_paths <- list(
    output = list(
      list(
        s3 = paste0("s3://ccao-model-results-us-east-1/performance/year=", year, "/stage=assessment/", run_id, ".parquet"),
        local = file.path(Sys.getenv("HOME"), paste0(run_id, "-performance.parquet"))
      ),
      list(
        s3 = paste0("s3://ccao-model-results-us-east-1/metadata/year=", year, "/", run_id, ".parquet"),
        local = file.path(Sys.getenv("HOME"), paste0(run_id, "-metadata.parquet"))
      ),
      list(
        s3 = paste0("s3://ccao-model-results-us-east-1/shap/year=", year, "/run_id=", run_id, "/"),
        local = file.path(Sys.getenv("HOME"), paste0(run_id, "-shap.parquet"))
      )
    )
  )

  s3_objs <- grep("s3://", unlist(analyses_paths$output), value = TRUE)
  bucket <- strsplit(s3_objs[1], "/")[[1]][3]

  for (analyses_path in analyses_paths$output) {
    is_directory <- endsWith(analyses_path$s3, "/")
    if (is_directory) {
      partitioned_by_run <- endsWith(analyses_path$s3, paste0("run_id=", run_id, "/"))
      if (partitioned_by_run) {
        dir_path <- analyses_path$s3
      } else {
        dir_path <- paste0(analyses_path$s3, "year=", year, "/run_id=", run_id, "/")
      }

      message("Now fetching: ", dir_path)
      objs_prefix <- sub(paste0("s3://", bucket, "/"), "", dir_path)
      objs <- aws.s3::get_bucket_df(bucket, objs_prefix)
      objs <- dplyr::filter(objs, Size > 0)

      if (nrow(objs) > 0) {
        shap_data <- NULL
        for (key in objs$Key) {
          message("Now fetching: ", key)
          local_temp_path <- file.path(tempdir(), basename(key))
          aws.s3::save_object(key, bucket = bucket, file = local_temp_path)

          # Read the Parquet file and append it to shap_data
          temp_data <- arrow::read_parquet(local_temp_path)
          if (is.null(shap_data)) {
            shap_data <- temp_data
          } else {
            shap_data <- dplyr::bind_rows(shap_data, temp_data)
          }
        }

        # Save the combined Parquet file
        arrow::write_parquet(shap_data, analyses_path$local)
      } else {
        warning(analyses_path$local, " does not exist for this run")
      }
    } else {
      message("Now fetching: ", analyses_path$s3)
      if (aws.s3::object_exists(analyses_path$s3, bucket = bucket)) {
        aws.s3::save_object(analyses_path$s3, bucket = bucket, file = analyses_path$local)
      } else {
        warning(analyses_path$local, " does not exist for this run")
      }
    }
  }
  tictoc::toc()
}

# Parameters
params <- list(
  run_id = "2024-03-17-stupefied-maya",
  run_id_year = "2024"
)

# Call the function with the params list
fetch_analyses(params$run_id, params$run_id_year)


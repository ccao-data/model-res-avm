# NOTE: This script requires CCAO employee access. See wiki for S3 credential
# setup

# Load the necessary libraries
library(aws.s3)
library(here)

# Setup some useful file names
model_run_id_file <- paste0(model_run_id, ".parquet")

# If remote upload is enabled, upload the file to S3 with the run ID as the name
aws.s3::put_object(
  model_metadata_file,
  file.path(model_s3_bucket, "metadata", paste0(model_run_id, ".parquet"))
)

aws.s3::put_object(
  here("output", "parameter", "model_parameter.parquet"),
  file.path(model_s3_bucket, "parameter", paste0(model_run_id, ".parquet"))
)
aws.s3::put_object(
  here("output", "workflow", "fit", "fit.zip"),
  file.path(model_s3_bucket, "workflow", "fit", paste0(model_run_id, ".zip"))
)
aws.s3::put_object(
  here("output", "workflow", "recipe", "recipe.rds"),
  file.path(model_s3_bucket, "workflow", "recipe", paste0(model_run_id, ".rds"))
)
if (model_upload_to_s3) {
  aws.s3::put_object(
    model_timing_file,
    file.path(model_s3_bucket, "timing", paste0(model_run_id, ".parquet"))
  )
}
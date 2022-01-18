# This script runs the full residential valuation pipeline from start to finish
# It includes the following steps:
#   1. Environment setup (pipeline/01-setup.R)
#   2. Training the model (pipeline/02-train.R) (inc. CV, full model training)
#   3. Evaluating the model on the test set (pipeline/03-evauate.R)
#   4. Using the trained model to value the current 
#      assessment year (pipeline/04-assess.R)
#   5. Record the time taken for each of the steps above to a log
#
# Each of these steps can also be run independently of one another. Note that by
# default, this script will upload model artifacts to S3 for CCAO employees

# Load reporting and timing libraries. Other libraries are loaded by each
# specific script
library(beepr)
library(dplyr)
library(here)
library(stringr)
library(tictoc)
library(tidyr)
tictoc::tic("Overall model pipeline")




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Setup Model #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Run the setup script to fetch environmental variables, metadata, etc. Begin
# the overall script timer
tictoc::tic("Setup model environment")
source("pipeline/01-setup.R")

# Save the metadata for the run to a local file
model_metadata_file <- here("output", "metadata", "model_metadata.parquet")
arrow::write_parquet(model_metadata, model_metadata_file)

# If remote upload is enabled, upload the file to S3 with the run ID as the name
if (model_upload_to_s3) {
  aws.s3::put_object(
    model_metadata_file,
    file.path(model_s3_bucket, "metadata", paste0(model_run_id, ".parquet"))
  )
}

# Remove values and data not needed for further pipeline steps
env_rm_list <- ls()[!ls() %in% c(
  "model_run_id", "model_run_timestamp",
  "model_upload_to_s3", "model_s3_bucket")
]
rm(list = c(env_rm_list, "env_rm_list"))

# Log the time taken for setup
tictoc::toc(log = TRUE)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Timings #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Close and log the highest level timing for the entire pipeline
tictoc::toc(log = TRUE)

# Transform the timing values into a single-row data frame and save
model_timing_file <- here("output", "timing", "model_timing.parquet")
bind_rows(tic.log(format = FALSE)) %>%
  mutate(
    model_run_id = model_run_id,
    model_run_timestamp = model_run_timestamp,
    elapsed = toc - tic,
    stage = tolower(word(msg, 1))
  ) %>%
  select(-c(tic:toc, msg)) %>%
  tidyr::pivot_wider(
    id_cols = c(model_run_id, model_run_timestamp),
    names_from = stage,
    values_from = elapsed
  ) %>%
  arrow::write_parquet(model_timing_file)

# If remote upload is enabled, upload the file to S3 with the run ID as the name
if (model_upload_to_s3) {
  aws.s3::put_object(
    model_timing_file,
    file.path(model_s3_bucket, "timing", paste0(model_run_id, ".parquet"))
  )
}

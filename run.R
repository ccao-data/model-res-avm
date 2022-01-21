# This script runs the full residential valuation pipeline from start to finish.
# The steps involved are described under each heading below.
#
# NOTES:
#   - Each of the pipeline scripts can be run independently as long as their
#     requisite files exist from previous runs. If a run dies, simply re-run
#     whichever script failed to continue the run
#   - By default, this script will upload model artifacts to S3 (for CCAO
#     employees only). Set MODEL_UPLOAD_TO_S3 to FALSE in the .Renviron file
#     if you don't want this behavior
#   - The script to ingest the training and assessment data is included (see
#     pipeline/00-ingest.R) but is not run automatically. Use DVC (CCAO) or 
#     git LFS (public users) to retrieve training and assessment data before
#     running this script
library(here)
source(here("R", "helpers.R"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Clear Previous Output #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

### WARNING: By default, this script will clear the output of previous runs
# when it is run. This is to prevent the artifacts of multiple runs from
# becoming jumbled. Disable this behavior in the included .Renviron file
model_clear_on_new_run <- as.logical(
  Sys.getenv("MODEL_CLEAR_ON_NEW_RUN", FALSE)
)

if (model_clear_on_new_run) {
  paths <- model_file_dict()
  local_paths <- unlist(paths)[
    grepl("local", names(unlist(paths)), fixed = TRUE)
  ]
  for (path in local_paths) if (file.exists(path)) file.remove(path)
}




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### 01. Setup Model #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Run the setup script to fetch environmental variables, metadata, etc. Will
# output a metadata file which stores run information. All further
source("pipeline/01-setup.R")




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### 02. Train Model #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Run the training script. This usually takes quite a long time, depending on
# the cross-validation settings
source("pipeline/02-train.R")




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### 03. Evaluate Results #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Evaluate the model's performance using two methods:
#   1. The standard holdout test set, in this case the most recent 10% of sales
#   2. An assessor-specific ratio study, comparing future assessments to 
#      current sales
#
# The script will generate a very large data frame of aggregate performance
# statistics for different levels of geography
source("pipeline/03-evaluate.R")




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### 05. Record Timings #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Close and log the highest level timing for the entire pipeline
tictoc::toc(log = TRUE)

# Save overall pipeline timings to file
source("pipeline/05-timing.R")




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### 06. Upload Results #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# If enabled, upload pipeline run to S3
if (model_upload_to_s3) source("pipeline/06-upload.R")

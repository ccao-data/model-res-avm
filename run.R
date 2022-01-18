# This script runs the full residential valuation pipeline from start to finish.
# The steps involved are described under each heading below.
#
# NOTES:
#   - Each of the pipeline scripts (except for pipeline/05-timing.R and
#     06-upload.R) and can be run independently and without first running this
#     script (as long as their requisite files exist from previous runs)
#   - By default, this script will upload model artifacts to S3 (for CCAO
#     employees only). Set MODEL_UPLOAD_TO_S3 to FALSE in the .Renviron file
#     if you don't want this behavior
#   - The script to ingest the training and assessment data is included (see
#     pipeline/00-ingest.R) but is not run automatically. Use DVC (CCAO) or 
#     git LFS (public users) to retrieve training and assessment data before
#     running this script

# Load reporting and timing libraries. Other libraries are loaded by each
# specific script
library(beepr)
library(here)
library(tictoc)

# Start overall pipeline timer
tictoc::tic("Overall model pipeline")




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### 01. Setup Model #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Run the setup script to fetch environmental variables, metadata, etc. Begin
# the overall script timer
source("pipeline/01-setup.R")




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### 02. Train Model #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Run the training script. This usually takes quite a long time, depending on
# the cross-validation settings
source("pipeline/02-train.R")




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
#if (model_upload_to_s3) source("pipeline/06-upload.R")

# BIG beep once pipeline finished
beepr::beep(8)
# This script runs the full residential valuation pipeline from start to finish.
# The steps involved are described under each heading below.

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
source("R/helpers.R")


#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 0. Clear Previous Output -----------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

### WARNING: By default, this script will clear the output of previous runs
# when it is run. This is to prevent the artifacts of multiple runs from
# becoming jumbled. Disable this behavior in the included .Renviron file
model_clear_on_new_run <- as.logical(
  Sys.getenv("MODEL_CLEAR_ON_NEW_RUN", FALSE)
)

if (model_clear_on_new_run) {
  paths <- model_file_dict()
  local_paths <- unlist(paths)[
    grepl("local", names(unlist(paths)), fixed = TRUE) &
      grepl("output", names(unlist(paths)), fixed = TRUE)
  ]
  for (path in local_paths) if (file.exists(path)) file.remove(path)
}




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Setup ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Fetch environmental variables, model parameters, metadata, etc.
source("pipeline/01-setup.R")




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. Train ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Train the LightGBM model using optional cross-validation. Generate model
# objects, data recipes, and predictions on the test set
source("pipeline/02-train.R")




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Assess --------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Use the trained model to estimate sale prices for all cards in the
# County. Also generate flags, attach land values, and make any post-modeling
# changes

# Only run this step if running interactively (non-CI). Otherwise,
# use only the test set data for performance measurement. See README for details
if (interactive()) source("pipeline/03-assess.R")




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4. Evaluate ------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Evaluate the model's performance using two methods:
#   1. The standard holdout test set, in this case the most recent 10% of sales
#   2. An assessor-specific ratio study, comparing future assessments to
#      current sales

# The script will generate a very large data frame of aggregate performance
# statistics for different levels of geography, as well as a secondary data
# frame for each quantile
source("pipeline/04-evaluate.R")




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 5. Interpret -----------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

### WARNING: Calculating SHAP values is very expensive. The time complexity is
# O(rows * num_iterations * num_leaves * max_depth^2). Calculating SHAP values
# for 1.1 million records for a complex model can take many hours (or even
# days). Therefore, this stage is only executed for interactive
# candidate and final runs

# Generate SHAP values for each PIN in the assessment data
if (interactive()) source("pipeline/05-interpret.R")




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 6. Finalize ------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Save run timings and upload pipeline run results to S3 for
# visualization and storage
source("pipeline/06-finalize.R")

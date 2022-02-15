# This script runs the full residential valuation pipeline from start to finish.
# The specific steps involved are described under each heading below

# Notes:
#   - Most of the settings for this pipeline are configured via the included
#     .Renviron file. Check that file before each run. If you change a setting,
#     you MUST restart R in order for the change to take effect
#   - Each pipeline run requires a "type", and many steps only run conditional
#     on certain types. For example, PIN-level output will only be created for
#     "candidate" or "final" runs. Run type is selected when running 01-setup.R
#   - Each pipeline script can be run independently as long as their
#     requisite files exist from a previous run. If a run dies, simply re-run
#     whichever script failed to continue the run
#   - By default, this script will upload model artifacts to S3 (for CCAO
#     employees only). Set MODEL_UPLOAD_TO_S3 to FALSE in the .Renviron file
#     if you don't want this behavior
#   - The script which generates the input data is included (see
#     pipeline/00-ingest.R), but only as a reference. Users of this pipeline do
#     not need to generate their own input data; it is provided by the CCAO via
#     DVC (if a CCAO employee) or git LFS (if a member of the public). See the
#     README for details
#   - This script runs each pipeline stage in a copy of the global environment.
#     After each stage is finished the copied environment is cleared. This
#     preserves the isolation between stages and frees memory after each stage
library(arrow)
library(dplyr)
source("R/helpers.R")

# Create a new environment based on the global environment
script_env <- new.env()

# Initialize a dictionary of file paths and S3 URIs. See R/file_dict.csv
paths <- model_file_dict()

# WARNING: By default, this script will clear the output of previous runs.
# This is to prevent the artifacts of multiple runs from becoming jumbled.
# Disable this behavior in the included .Renviron file by setting
# MODEL_CLEAR_ON_NEW_RUN to FALSE
model_clear_on_new_run <- as.logical(
  Sys.getenv("MODEL_CLEAR_ON_NEW_RUN", FALSE)
)

if (model_clear_on_new_run) {
  local_paths <- unlist(paths)[
    grepl("local", names(unlist(paths)), fixed = TRUE) &
      grepl("output", names(unlist(paths)), fixed = TRUE)
  ]
  for (path in local_paths) if (file.exists(path)) file.remove(path)
}




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 0. File IO -------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Each stage produces a set of files in the output/ directory. These files are
# used to save state and transfer information between stages. Each stages lists
# its corresponding input and output files

# Certain inputs and outputs are used by every stage. These are listed
# here rather than being repeated in the description of each stage

# Inputs (all stages):
#   - output/metadata/model_metadata.parquet (control parameters, info about
#     predictors, model info, etc.)
#   - output/intermediate/model_timing.parquet (loaded from previous stage
#     then appended to)

# Outputs (all stages):
#   - output/intermediate/model_timing.parquet (stage time elapsed is appended
#     to this file for each stage)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Setup ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Fetch environmental variables, model parameters, metadata, etc. This will
# basically record the settings for each run and put them in a data frame. These
# settings are then used to control all the later stages

# Inputs:
#   - input/*.dvc (input data version hashes, added to metadata)
#   - input/training_data.parquet (fetch list of independent variables)

# Outputs:
#   - output/metadata/model_metadata.parquet (data frame of run settings)
#   - output/intermediate/model_parameter_default.parquet (single-row data
#     frame of default model hyperparameters used if CV is disabled)
source("pipeline/01-setup.R", local = script_env)
rm(list = ls(envir = script_env), envir = script_env)
gc()

# Load the run type from the just-created metadata file. This is used to decide
# which later stages to run
model_run_type <- read_parquet(paths$output$metadata$local)$run_type




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. Train ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Train a LightGBM model using optional cross-validation. Generate model
# objects, data recipes, and predictions on the test set (most recent 10% 
# of sales)

# Inputs:
#   - input/training_data.parquet (sales data used to train the model)
#   - output/intermediate/model_parameter_default.parquet (default parameters if
#     CV is disabled)

# Outputs:
#   - output/parameter_search/model_parameter_search.parquet (raw output from
#     Tidymodels cross-validation, can be used to select best model)
#   - output/parameter_range/model_parameter_range.parquet (range of
#     hyperparameters searched by cross-validation, as defined by dials::tune())
#   - output/parameter_final/model_parameter_final.parquet (final
#     hyperparameters chosen for this run using tune::select_best())
#   - output/intermediate/model_test.parquet (predictions on/data from the test
#     set, which is a holdout sample of the most recent 10% of sales)
#   - output/workflow/fit/model_workflow_fit.zip (LightGBM model object and
#     Tidymodels parsnip specification. Use lightsnip::lgbm_save/lgbm_load)
#   - output/workflow/recipe/model_workflow_recipe.rds (Tidymodels recipe for
#     preparing the input data. Can be used to prepare new, unseen input data)
source("pipeline/02-train.R", local = script_env)
rm(list = ls(envir = script_env), envir = script_env)
gc()




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Assess --------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Use the trained model to estimate sale prices for all PINS/cards in Cook
# County. Also generate flags, calculate land values, and make any post-modeling
# changes

# Conditional on:
#   - Will only run for run types "candidate" or "final" due to the high compute
#     and storage costs for this stage

# Inputs:
#   - input/training_data.parquet (sales data gets attached to assessed values
#     to help with desk review and ratio studies, not used for training here)
#   - output/workflow/fit/model_workflow_fit.zip (trained model which is used
#     on the assessment data to estimate sale prices)
#   - output/workflow/recipe/model_workflow_recipe.rds (trained recipe object
#     which is used to prepare the assessment data for input to the model)
#   - input/assessment_data.parquet (the universe of all properties that need
#     estimated values, same features as training data)
#   - input/complex_id_data.parquet (set of townhome 210/295 complex IDs, used
#     to assign complexes the same average value)
#   - input/land_site_rate_data.parquet (pre-determined land values assigned to
#     individual PINs)
#   - input/land_nbhd_rate_data.parquet (neighborhood land rates used if a PIN-
#     level rate doesn't exist)

# Outputs:
#   - output/assessment_card/model_assessment_card.parquet (card-level predicted
#     values for all residential properties with characteristics)
#   - output/assessment_pin/model_assessment_pin.parquet (PIN-level predicted
#     values. These are aggregated from the card-level. See script for details)
#   - output/intermediate/model_assessment.parquet (predictions on the PIN-
#     level saved temporarily to calculate performance stats in the next stage)
if (model_run_type %in% c("candidate", "final")) {
  source("pipeline/03-assess.R", local = script_env)
}
rm(list = ls(envir = script_env), envir = script_env)
gc()




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4. Evaluate ------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Evaluate the model's performance using two methods:
#   1. The standard holdout test set, in this case the most recent 10% of sales
#   2. ("candidate" and "final" runs only) An assessor-specific ratio study
#      comparing estimated assessments to the previous year's sales

# Generate aggregate performance statistics for different levels of geography,
# as well as a secondary data set of statistics for each quantile specified in
# the .Renviron file

# Conditional on:
#   - Will evaluate performance on the assessment set if the run type is
#     "candidate" or "final", otherwise will only evaluate the test set

# Inputs:
#   - output/intermediate/model_test.parquet (predictions from the model used to
#     evaluate hold-out performance)
#   - output/intermediate/model_assessment.parquet (predictions from the model
#     on the universe of properties needing assessments, used to perform sales
#     ratio studies)

# Outputs:
#   - output/performance/model_performance_test.parquet (performance stats on
#     the test set by geography and class)
#   - output/performance_quantile/model_performance_test_quantile.parquet
#     (performance stats on the test set by geography and quantile)
#   - output/performance/model_performance_assessment.parquet (performance stats
#     on the assessment set by geography and class)
#   - output/performance_quantile/model_performance_assessment_quantile.parquet
#     (performance stats on the assessment set by geography and quantile)
source("pipeline/04-evaluate.R", local = script_env)
rm(list = ls(envir = script_env), envir = script_env)
gc()




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 5. Interpret -----------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Generate SHAP values for each card and feature in the assessment data

# WARNING: Calculating SHAP values is very expensive. The time complexity is
# O(rows * num_iterations * num_leaves * max_depth^2). Calculating SHAP values
# for 1.1 million records for a complex model can take many hours (or even
# days)

# Conditional on:
#   - Will only run for run types "candidate" or "final" due to the high compute
#     and storage costs for this stage

# Inputs:
#   - output/workflow/fit/model_workflow_fit.zip (trained model which is used
#     get SHAP values)
#   - output/workflow/recipe/model_workflow_recipe.rds (trained recipe object
#     used to prepare the assessment data for prediction)
#   - input/assessment_data.parquet (the universe of all cards and features
#     that need SHAP values)

# Outputs:
#   - output/shap/model/model_shap.parquet (all SHAP values for all feature and
#     cards in the assessment data)
if (model_run_type %in% c("candidate", "final")) {
  source("pipeline/05-interpret.R", local = script_env)
}
rm(list = ls(envir = script_env), envir = script_env)
gc()




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 6. Finalize ------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Save run timings, upload pipeline run results to S3, and send a notification.
# Will also finalize/clean some of the generated outputs prior to upload

# Conditional on:
#   - Most sections of this stage will only run if MODEL_UPLOAD_TO_S3 is TRUE

# Inputs:
#   - All output data sets

# Outputs:
#   - All data sets, cleaned and uploaded to their respective buckets in S3. See
#     R/file_dict.csv for details on where output objects end up on S3
#   - output/timing/model_timing.parquet (clean, wide version of the stage
#     timing log)
source("pipeline/06-finalize.R", local = script_env)
rm(list = ls(envir = script_env), envir = script_env)
gc()

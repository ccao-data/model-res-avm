# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Setup #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Start the script timer and clear logs from prior script
tictoc::tic.clearlog()
tictoc::tic("Setup model environment")

# The purpose of this file is to set up the environment and variables needed for
# a full model pipeline run and save them to S3. This script is not necessary to
# run if you want to run model stages individually

# Load the necessary libraries
library(arrow)
library(dplyr)
library(aws.s3)
library(git2r)
library(here)
library(lubridate)
library(yaml)
source(here("R", "helpers.R"))

# Initialize a dictionary of file paths and URIs. See R/file_dict.csv
paths <- model_file_dict()


### Read initial configuration parameters from the included .Renviron file

# Get information about the assessment year, date, and the sales sample
model_assessment_year <- Sys.getenv("MODEL_ASSESSMENT_YEAR", "2022")
model_assessment_date <- Sys.getenv("MODEL_ASSESSMENT_DATE", "2022-01-01")
model_assessment_data_year <- Sys.getenv("MODEL_ASSESSMENT_DATA_YEAR", "2021")
model_min_sale_year <- Sys.getenv("MODEL_MIN_SALE_YEAR", "2015")
model_max_sale_year <- Sys.getenv("MODEL_MAX_SALE_YEAR", "2021")

# Get model type, seed, group, and triad
model_group <- Sys.getenv("MODEL_GROUP", "residential")
model_type <- Sys.getenv("MODEL_TYPE", "lightgbm")
model_triad <- Sys.getenv("MODEL_TRIAD", "North")
model_seed <- as.integer(Sys.getenv("MODEL_SEED"), 27)

# Disable CV for non-interactive sessions (GitLab CI) unless overridden
if (interactive() | as.logical(Sys.getenv("MODEL_CV_ENABLE_OVERRIDE", FALSE))) {
  model_cv_enable <- as.logical(Sys.getenv("MODEL_CV_ENABLE", TRUE))
} else {
  model_cv_enable <- FALSE
}

# Get info on cross-validation setup and split proportion
model_cv_num_folds <- as.numeric(Sys.getenv("MODEL_CV_NUM_FOLDS", 6))
model_cv_initial_set <- as.numeric(Sys.getenv("MODEL_CV_INITIAL_SET", 10))
model_cv_max_iterations <- as.numeric(Sys.getenv("MODEL_CV_MAX_ITERATIONS", 25))
model_cv_no_improve <- as.numeric(Sys.getenv("MODEL_CV_NO_IMPROVE", 8))
model_cv_split_prop <- as.numeric(Sys.getenv("MODEL_CV_SPLIT_PROP", 0.90))
model_cv_best_metric <- as.character(Sys.getenv("MODEL_CV_BEST_METRIC", "rmse"))

# Info on type and year of values used for assessment reporting. Typically the
# "near" year is 1 year prior and the "far" year is values after the last
# triennial assessment
model_ratio_study_near_year <- Sys.getenv(
  "MODEL_RATIO_STUDY_NEAR_YEAR", model_assessment_data_year
)
model_ratio_study_near_stage <- Sys.getenv(
  "MODEL_RATIO_STUDY_NEAR_STAGE", "mailed"
)
model_ratio_study_far_year <- Sys.getenv(
  "MODEL_RATIO_STUDY_FAR_YEAR", "2019"
)
model_ratio_study_far_stage <- Sys.getenv(
  "MODEL_RATIO_STUDY_FAR_STAGE",
  as.character(as.numeric(model_assessment_year) - 3)
)
model_ratio_study_num_quantile <- as.integer(strsplit(Sys.getenv(
  "MODEL_RATIO_STUDY_NUM_QUANTILE", "3,5"
), ",")[[1]])




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Gather Metadata #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Generate a random identifier for this run. This will serve as the primary key/
# identifier for this run in perpetuity. See ?ccao_generate_id for details
model_run_id <- ccao::ccao_generate_id()

# Get the current timestamp for when the run started
model_run_start_timestamp <- lubridate::now()

# Get the commit of the current reference
git_commit <- git2r::revparse_single(git2r::repository(), "HEAD")

# If in an interactive session, prompt the user for notes and
# a type related to this run. Otherwise, use the commit message and "automated"
if (interactive()) {
  model_run_note <- readline("Run notes/message: ")
  model_run_type <- switch(
    readline(paste0(
      "What type of run is this? ",
      "\n 1. 'experiment' \n 2. 'candidate' \n 3. 'final' \n"
    )),
    "1" = "experiment",
    "2" = "candidate",
    "3" = "final",
    stop("Must be a value between 1 and 3")
  )
} else {
  model_run_note <- gsub("\n", "", git_commit$message)
  model_run_type <- "automated"
}

# Read the MD5 hash of each input dataset. These are created by DVC and used to
# version and share the input data
assessment_md5 <- read_yaml(paths$input$assessment$dvc)$outs[[1]]$md5
training_md5 <- read_yaml(paths$input$training$dvc)$outs[[1]]$md5

# Get the predictors used for training the model + a count of predictors
model_predictors <- ccao::vars_dict %>%
  dplyr::filter(var_is_predictor) %>%
  dplyr::pull(var_name_model) %>%
  unique() %>%
  na.omit()

# Open the training data schema to extract the variables actually available
training_data_predictors <- arrow::open_dataset(
  paths$input$training$local
)$schema$names
model_predictors <- training_data_predictors[
  training_data_predictors %in% model_predictors &
  training_data_predictors != "meta_sale_price"
]

# Compile the information above into a single tibble with 1 row
model_metadata <- tibble::tibble(
  run_id = model_run_id,
  run_start_timestamp = model_run_start_timestamp,
  run_type = model_run_type,
  run_note = model_run_note,
  model_assessment_year,
  model_assessment_date = lubridate::as_date(model_assessment_date),
  model_assessment_data_year,
  model_group,
  model_triad,
  model_type,
  model_seed,
  git_sha_short = substr(git_commit$sha, 1, 8),
  git_sha_long = git_commit$sha,
  git_message = gsub("\n", "", git_commit$message),
  git_author = git_commit$author$name,
  git_email = git_commit$author$email,
  model_training_data_dvc_id = training_md5,
  model_assessment_data_dvc_id = assessment_md5,
  model_min_sale_year,
  model_max_sale_year,
  model_ratio_study_far_year,
  model_ratio_study_far_stage,
  model_ratio_study_near_year,
  model_ratio_study_near_stage,
  model_ratio_study_num_quantile = list(model_ratio_study_num_quantile),
  model_cv_enable = model_cv_enable,
  model_cv_num_folds = as.integer(model_cv_num_folds),
  model_cv_initial_set = as.integer(model_cv_initial_set),
  model_cv_max_iterations = as.integer(model_cv_max_iterations),
  model_cv_no_improve = as.integer(model_cv_no_improve),
  model_cv_split_prop = as.numeric(model_cv_split_prop),
  model_cv_best_metric = as.character(model_cv_best_metric),
  model_predictor_count = length(model_predictors),
  model_predictor_name = list(model_predictors)
)

# Save the metadata for the run to a local file
arrow::write_parquet(model_metadata, paths$output$metadata$local)

# End the script timer and write the time elapsed to file
tictoc::toc(log = TRUE)
bind_rows(tictoc::tic.log(format = FALSE)) %>%
  arrow::write_parquet(paths$intermediate$timing$local)

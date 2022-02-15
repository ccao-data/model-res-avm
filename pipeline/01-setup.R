#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Setup ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Start the stage timer and clear logs from prior stage
tictoc::tic.clearlog()
tictoc::tic("Setup model environment")

# Load libraries and scripts
library(arrow)
library(dplyr)
library(aws.s3)
library(git2r)
library(here)
library(lubridate)
library(tibble)
library(yaml)
source(here("R", "helpers.R"))

# Initialize a dictionary of file paths and URIs. See R/file_dict.csv
paths <- model_file_dict()




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. Gather Metadata -----------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

## 2.1. Run Info ---------------------------------------------------------------

# Generate a random identifier for this run. This will serve as the primary key/
# identifier for in perpetuity. See ?ccao_generate_id for details
model_run_id <- ccao::ccao_generate_id()

# Get the current timestamp for when the run started
model_run_start_timestamp <- lubridate::now()

# Get the commit of the current reference
git_commit <- git2r::revparse_single(git2r::repository(), "HEAD")

# If in an interactive session, prompt the user for notes and a type related
# to this run. Otherwise, use the commit message and type = "automated"
if (interactive()) {
  model_run_note <- readline("Run notes/message: ")
  model_run_type <- switch(utils::menu(
    choices = c("experiment", "candidate", "final"),
    graphics = FALSE,
    title = "What type of run is this?"
  ),
  "1" = "experiment",
  "2" = "candidate",
  "3" = "final"
  )
} else {
  model_run_note <- gsub("\n", "", git_commit$message)
  model_run_type <- "automated"
}


## 2.2. DVC Hashes -------------------------------------------------------------

# Read the MD5 hash of each input dataset. These are created by DVC and used to
# version and share the input data
assessment_md5 <- read_yaml(paths$input$assessment$dvc)$outs[[1]]$md5
training_md5 <- read_yaml(paths$input$training$dvc)$outs[[1]]$md5
complex_id_md5 <- read_yaml(paths$input$complex_id$dvc)$outs[[1]]$md5
land_site_rate_md5 <- read_yaml(paths$input$land_site_rate$dvc)$outs[[1]]$md5
land_nbhd_rate_md5 <- read_yaml(paths$input$land_nbhd_rate$dvc)$outs[[1]]$md5


## 2.3. Model Parameters -------------------------------------------------------

# Read initial configuration parameters from the included .Renviron file,
# including the assessment year, date, and the sales sample boundaries
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

# Get number of available physical cores to use for lightgbm multi-threading
# Lightgbm docs recommend using only real cores, not logical
# https://lightgbm.readthedocs.io/en/latest/Parameters.html#num_threads
model_num_threads <- parallel::detectCores(logical = FALSE)

# Info on type and year of values used for assessment reporting. Typically the
# "near" year is 1 year prior and the "far" year is BoR values after the last
# triennial reassessment
model_ratio_study_far_year <- Sys.getenv(
  "MODEL_RATIO_STUDY_FAR_YEAR", "2019"
)
model_ratio_study_far_stage <- Sys.getenv(
  "MODEL_RATIO_STUDY_FAR_STAGE",
  as.character(as.numeric(model_assessment_year) - 3)
)
model_ratio_study_far_column <- get_rs_col_name(
  model_assessment_data_year,
  model_ratio_study_far_year,
  model_ratio_study_far_stage
)
model_ratio_study_near_year <- Sys.getenv(
  "MODEL_RATIO_STUDY_NEAR_YEAR", model_assessment_data_year
)
model_ratio_study_near_stage <- Sys.getenv(
  "MODEL_RATIO_STUDY_NEAR_STAGE", "mailed"
)
model_ratio_study_near_column <- get_rs_col_name(
  model_assessment_data_year,
  model_ratio_study_near_year,
  model_ratio_study_near_stage
)
model_ratio_study_num_quantile <- as.integer(strsplit(Sys.getenv(
  "MODEL_RATIO_STUDY_NUM_QUANTILE", "3,5"
), ",")[[1]])


## 2.4. CV Parameters ----------------------------------------------------------

# Enable CV only for experimental and final runs
if (model_run_type %in% c("candidate", "final")) {
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


## 2.5. Post-Valuation Parameters ----------------------------------------------

# Get post-modeling parameters like level of rounding, land caps, etc.
model_pv_multicard_yoy_cap <- as.numeric(
  Sys.getenv("MODEL_PV_MULTICARD_YOY_CAP", 2)
)
model_pv_land_pct_of_total_cap <- as.numeric(
  Sys.getenv("MODEL_PV_LAND_PCT_CAP", 0.4)
)
model_pv_round_break <- as.numeric(strsplit(Sys.getenv(
  "MODEL_PV_ROUND_BREAK", "20000"
), ",")[[1]])
model_pv_round_to_nearest <- as.numeric(strsplit(Sys.getenv(
  "MODEL_PV_ROUND_TO_NEAREST", "100,500"
), ",")[[1]])
model_pv_round_type <- Sys.getenv("MODEL_PV_ROUND_TYPE", "ceiling")


## 2.6. Model Variables --------------------------------------------------------

# Create list of variables that uniquely identify each structure or sale, these
# can be kept in the training data even though they are not regressors
model_id_columns <- c(
  "meta_pin", "meta_class", "meta_card_num", "meta_sale_document_num"
)

# Get the full list of right-hand side predictors from ccao::vars_dict. To
# manually add new features, append the name of the feature as it is stored in
# training_data to this vector
model_predictors_all <- ccao::vars_dict %>%
  dplyr::filter(var_is_predictor) %>%
  dplyr::pull(var_name_model) %>%
  unique() %>%
  na.omit()

# Get a list only of categorical predictors to pass to lightgbm when training
model_predictors_categorical <- ccao::vars_dict %>%
  dplyr::filter(
    var_is_predictor,
    var_data_type %in% c("categorical", "character")
  ) %>%
  dplyr::pull(var_name_model) %>%
  unique() %>%
  na.omit()

# Open the training data schema to extract the variables actually available,
# then use it to filter the lists of predictors
training_data_predictors <- arrow::open_dataset(
  paths$input$training$local
)$schema$names
model_predictors_all <- training_data_predictors[
  training_data_predictors %in% model_predictors_all &
    training_data_predictors != "meta_sale_price"
]
model_predictors_categorical <- training_data_predictors[
  training_data_predictors %in% model_predictors_categorical &
    training_data_predictors != "meta_sale_price"
]




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Compile Metadata ----------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Compile the information above into a single tibble with 1 row then save it
# to a local parquet file
model_metadata <- tibble::tibble(
  
  # 2.1. Run Info
  run_id = model_run_id,
  run_start_timestamp = model_run_start_timestamp,
  run_type = model_run_type,
  run_note = model_run_note,
  git_sha_short = substr(git_commit$sha, 1, 8),
  git_sha_long = git_commit$sha,
  git_message = gsub("\n", "", git_commit$message),
  git_author = git_commit$author$name,
  git_email = git_commit$author$email,
  
  # 2.2. DVC Hashes
  model_training_data_dvc_id = training_md5,
  model_assessment_data_dvc_id = assessment_md5,
  model_complex_id_data_dvc_id = complex_id_md5,
  model_land_site_rate_data_dvc_id = land_site_rate_md5,
  model_land_nbhd_rate_data_dvc_id = land_nbhd_rate_md5,
  
  # 2.3. Model Parameters
  model_assessment_year,
  model_assessment_date = lubridate::as_date(model_assessment_date),
  model_assessment_data_year,
  model_min_sale_year,
  model_max_sale_year,
  model_group,
  model_triad,
  model_type,
  model_seed,
  model_num_threads,
  model_ratio_study_far_year,
  model_ratio_study_far_stage,
  model_ratio_study_far_column,
  model_ratio_study_near_year,
  model_ratio_study_near_stage,
  model_ratio_study_near_column,
  model_ratio_study_num_quantile = list(model_ratio_study_num_quantile),
  
  # 2.4. CV Parameters
  model_cv_enable = model_cv_enable,
  model_cv_num_folds = as.integer(model_cv_num_folds),
  model_cv_initial_set = as.integer(model_cv_initial_set),
  model_cv_max_iterations = as.integer(model_cv_max_iterations),
  model_cv_no_improve = as.integer(model_cv_no_improve),
  model_cv_split_prop = as.numeric(model_cv_split_prop),
  model_cv_best_metric = as.character(model_cv_best_metric),
  
  # 2.5. Post-Valuation Parameters
  model_pv_multicard_yoy_cap,
  model_pv_land_pct_of_total_cap,
  model_pv_round_break = list(model_pv_round_break),
  model_pv_round_to_nearest = list(model_pv_round_to_nearest),
  model_pv_round_type,
  
  # 2.6. Model Variables
  model_identifier_count = length(model_id_columns),
  model_identifier_name = list(model_id_columns),
  model_predictor_all_count = length(model_predictors_all),
  model_predictor_all_name = list(model_predictors_all),
  model_predictor_categorical_count = length(model_predictors_categorical),
  model_predictor_categorical_name = list(model_predictors_categorical)
) %>%
  arrow::write_parquet(paths$output$metadata$local)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4. Gather Hyperparameters ----------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Gather all the hyperparameters set via .Renviron

## 4.1. Manual Hyperparameters -------------------------------------------------
model_param_objective_func <- as.character(
  Sys.getenv("MODEL_PARAM_OBJECTIVE_FUNC", "regression")
)
model_param_num_iterations <- as.integer(
  Sys.getenv("MODEL_PARAM_NUM_ITERATIONS", 500)
)
model_param_learning_rate <- as.numeric(
  Sys.getenv("MODEL_PARAM_LEARNING_RATE", 0.1)
)
model_param_validation_prop <- as.numeric(
  Sys.getenv("MODEL_PARAM_VALIDATION_PROP", 0.1)
)
model_param_validation_metric <- as.character(
  Sys.getenv("MODEL_PARAM_VALIDATION_METRIC", "rmse")
)
model_param_link_max_depth <- as.logical(
  Sys.getenv("MODEL_PARAM_LINK_MAX_DEPTH", TRUE)
)

## 4.2. Tuned Hyperparameters --------------------------------------------------
model_hparam_stop_iter <- as.integer(
  Sys.getenv("MODEL_HPARAM_STOP_ITER", 18)
)
model_hparam_num_leaves <- as.integer(
  Sys.getenv("MODEL_HPARAM_NUM_LEAVES", 2670)
)
model_hparam_add_to_linked_depth <- as.integer(
  Sys.getenv("MODEL_HPARAM_ADD_TO_LINKED_DEPTH", 3)
)
model_hparam_max_depth <- as.integer(
  Sys.getenv("MODEL_HPARAM_MAX_DEPTH", 12)
)
model_hparam_feature_fraction <- as.numeric(
  Sys.getenv("MODEL_HPARAM_FEATURE_FRACTION", 0.9)
)
model_hparam_min_gain_to_split <- as.numeric(
  Sys.getenv("MODEL_HPARAM_MIN_GAIN_TO_SPLIT", 0.0)
)
model_hparam_min_data_in_leaf <- as.integer(
  Sys.getenv("MODEL_HPARAM_MIN_DATA_IN_LEAF", 64)
)
model_hparam_max_cat_threshold <- as.integer(
  Sys.getenv("MODEL_HPARAM_MAX_CAT_THRESHOLD", 72)
)
model_hparam_min_data_per_group <- as.integer(
  Sys.getenv("MODEL_HPARAM_MIN_DATA_PER_GROUP", 66)
)
model_hparam_cat_smooth <- as.numeric(
  Sys.getenv("MODEL_HPARAM_CAT_SMOOTH", 99.0)
)
model_hparam_cat_l2 <- as.numeric(
  Sys.getenv("MODEL_HPARAM_CAT_L2", 0.001)
)
model_hparam_lambda_l1 <- as.numeric(
  Sys.getenv("MODEL_HPARAM_LAMBDA_L1", 0.005)
)
model_hparam_lambda_l2 <- as.numeric(
  Sys.getenv("MODEL_HPARAM_LAMBDA_L2", 1.825)
)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 5. Compile Hyperparameters ---------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Compile all hyperparameters into a single-row tibble. These will serve as the
# default parameters if CV is not run. If CV is run, the tuned parameters will
# be overwritten
model_parameter_final <- tibble::tibble(
  run_id = model_run_id,
  
  # 4.1. Manual Hyperparameters
  objective_func = model_param_objective_func,
  num_iterations = model_param_num_iterations,
  learning_rate = model_param_learning_rate,
  validation_prop = model_param_validation_prop,
  validation_metric = model_param_validation_metric,
  link_max_depth = model_param_link_max_depth,
  
  # 4.2 Tuned Hyperparameters
  stop_iter = model_hparam_stop_iter,
  num_leaves = model_hparam_num_leaves,
  add_to_linked_depth = model_hparam_add_to_linked_depth,
  max_depth = ifelse(
    model_param_link_max_depth,
    as.integer(
      floor(log2(model_hparam_num_leaves)) + model_hparam_add_to_linked_depth
    ),
    model_hparam_max_depth
  ),
  feature_fraction = model_hparam_feature_fraction,
  min_gain_to_split = model_hparam_min_gain_to_split,
  min_data_in_leaf = model_hparam_min_data_in_leaf,
  max_cat_threshold = model_hparam_max_cat_threshold,
  min_data_per_group = model_hparam_min_data_per_group,
  cat_smooth = model_hparam_cat_smooth,
  cat_l2 = model_hparam_cat_l2,
  lambda_l1 = model_hparam_lambda_l1,
  lambda_l2 = model_hparam_lambda_l2,
  .config = "Default"
) %>%
  arrow::write_parquet(paths$output$parameter_final$local)

# End the script timer and write the time elapsed to file
tictoc::toc(log = TRUE)
bind_rows(tictoc::tic.log(format = FALSE)) %>%
  arrow::write_parquet(paths$intermediate$timing$local)

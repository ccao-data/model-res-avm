# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Setup #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Start the script timer and clear logs from prior script
tictoc::tic.clearlog()
tictoc::tic("Train model")

# Load R libraries
options(tidymodels.dark = TRUE)
library(arrow)
library(assessr)
library(butcher)
library(ccao)
library(dplyr)
library(here)
library(lightgbm)
library(purrr)
library(spatialsample)
library(stringr)
library(tictoc)
library(tidymodels)
library(treesnip)
library(vctrs)

# Load helpers, recipes, and lightgbm parsnip bindings from files
walk(list.files("R/", "\\.R$", full.names = TRUE), source)

# Initialize a dictionary of file paths and URIs. See R/helpers.R
paths <- model_file_dict()

# Get number of available physical cores to use for lightgbm multithreading
# lightgbm docs recommend using only real cores, not logical
# https://lightgbm.readthedocs.io/en/latest/Parameters.html#num_threads
num_threads <- parallel::detectCores(logical = FALSE)

# Get train/test split proportion and model seed
model_split_prop <- as.numeric(Sys.getenv("MODEL_SPLIT_PROP", 0.90))
model_seed <- as.integer(Sys.getenv("MODEL_SEED"), 27)
set.seed(model_seed)

# Retrieve hard-coded model hyperparameters from .Renviron
model_param_num_iterations <- as.integer(
  Sys.getenv("MODEL_PARAM_NUM_ITERATIONS", 500)
)
model_param_learning_rate <- as.numeric(
  Sys.getenv("MODEL_PARAM_LEARNING_RATE", 0.1)
)
model_param_max_cat_threshold <- as.integer(
  Sys.getenv("MODEL_PARAM_MAX_CAT_THRESHOLD", 200)
)
model_param_min_data_per_group <- as.integer(
  Sys.getenv("MODEL_PARAM_MIN_DATA_PER_GROUP", 100)
)

# Disable CV for non-interactive sessions (GitLab CI) unless overridden
if (interactive() | as.logical(Sys.getenv("MODEL_CV_ENABLE_OVERRIDE", FALSE))) {
  model_cv_enable <- as.logical(Sys.getenv("MODEL_CV_ENABLE", TRUE))
} else {
  model_cv_enable <- FALSE
}

# Setup cross-validation parameters using .Renviron file
model_cv_num_folds <- as.integer(Sys.getenv("MODEL_CV_NUM_FOLDS", 6))
model_cv_initial_set <- as.integer(Sys.getenv("MODEL_CV_INITIAL_SET", 10))
model_cv_max_iterations <- as.integer(Sys.getenv("MODEL_CV_MAX_ITERATIONS", 25))
model_cv_no_improve <- as.integer(Sys.getenv("MODEL_CV_NO_IMPROVE", 8))





# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Prepare Data #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load the full set of training data, then arrange by sale date in order to
# facilitate out-of-time sampling/validation.
#
# NOTE: It is critical to trim "multicard" sales when training. Multicard means
# there is multiple buildings on a PIN. Sales for multicard PINs are
# often for multiple buildings and will therefore bias the model training
training_data_full <- read_parquet(paths$input$training$local) %>%
  filter(!is.na(loc_longitude), !is.na(loc_latitude), !ind_pin_is_multicard) %>%
  arrange(meta_sale_date)

# Create list of variables that uniquely identify each structure or sale, these
# can be kept in the training data even though they are not regressors
model_id_vars <- c(
  "meta_pin", "meta_class", "meta_card_num", "meta_sale_document_num"
)

# Get the full list of right-hand side predictors from ccao::vars_dict. To
# manually add new features, append the name of the feature as it is stored in
# training_data to this vector
model_predictors <- ccao::vars_dict %>%
  dplyr::filter(var_is_predictor) %>%
  dplyr::pull(var_name_model) %>%
  unique() %>%
  na.omit() 
  
# Create train/test split by time, with most recent observations in the test set
# We want our best model(s) to be predictive of the future, since properties are
# assessed on the basis of past sales
time_split <- initial_time_split(training_data_full, prop = model_split_prop)
test <- testing(time_split)
train <- training(time_split)

# Create V CV folds, where each fold is a distinct area in Cook County. The idea
# here is to create a model that performs well on all areas. This also prevents
# leakage from the training data and overfitting
train_folds <- spatialsample::spatial_clustering_cv(
  data = train,
  coords = c(loc_longitude, loc_latitude),
  v = model_cv_num_folds
)

# Create a recipe for the training data which removes non-predictor columns,
# normalizes/logs data, and removes/imputes missing values
train_recipe <- model_main_recipe(
  data = train,
  keep_vars = model_predictors,
  id_vars = model_id_vars
)

# Extract number of columns actually used in model (P). This is needed to
# properly set certain tuning parameters
juiced_train <- juice(prep(train_recipe), all_predictors())
train_p <- ncol(juiced_train)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### LightGBM Model #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# This is the main model used to value 200-class residential property. It uses
# lightgbm as a backend, which is a boosted tree model similar to xgboost or
# catboost, but with better performance and faster training in our use case
# See https://lightgbm.readthedocs.io/ for more information


### Step 1 - Model initialization

# Initialize lightgbm model specification. Note that categorical columns are
# detected automatically by treesnip's lightgbm implementation as long as they
# are factors. trees argument here maps to num_iterations in lightgbm
lgbm_model <- lgbm_tree(
  trees = model_param_num_iterations, learn_rate = model_param_learning_rate,
  min_n = tune(), tree_depth = tune(), mtry = tune(),
  lambda_l1 = tune(), lambda_l2 = tune(), cat_smooth = tune()
) %>%
  set_engine("lightgbm") %>%
  set_mode("regression") %>%
  set_args(
    # These are lightgbm-specific parameters that are passed to lgb.train
    num_threads = num_threads,
    verbosity = 1L,
    
    # IMPORTANT: Max number of possible splits for categorical features. Needs
    # to be set high for our data due to high cardinality
    # https://lightgbm.readthedocs.io/en/latest/Parameters.html#max_cat_threshold
    max_cat_threshold = model_param_max_cat_threshold,
    min_data_per_group = model_param_min_data_per_group
  )

# Initialize lightgbm workflow, which contains both the model spec AND the
# pre-processing steps needed to prepare the raw data
lgbm_wflow <- workflow() %>%
  add_model(lgbm_model) %>%
  add_recipe(
    recipe = train_recipe,
    blueprint = hardhat::default_recipe_blueprint(allow_novel_levels = TRUE)
  )


### Step 2 - Cross-validation

# Begin CV tuning if enabled. We use Bayesian tuning due to the high number of
# hyperparameters, as grid search or random search take a very long time to
# produce similarly accurate results
if (model_cv_enable) {

  # Begin cross-validation timer. This step typically takes the longest time
  tictoc::tic("CV model")
  
  # Create the parameter search space for hyperparameter optimization
  # Parameter boundaries are taken from the lightgbm docs and hand-tuned
  # See: https://lightgbm.readthedocs.io/en/latest/Parameters-Tuning.html

  # NOTE: num_leaves is implicitly constrained by min_n (which is
  # min_data_in_leaf in lightgbm). Therefore, we don't explicitly tune it
  lgbm_params <- lgbm_model %>%
    parameters() %>%
    update(
      
      ### These are lightgbm tuning parameters included with treesnip. They each
      ### map to lightgbm parameters of different names

      # Maps to min_data_in_leaf in lightgbm. Most important. Optimal/large
      # values can help prevent overfitting. This implicitly defines the number
      # of leaves
      min_n = min_n(c(20L, 1200L)),
      
      # Very important. Maps to max_depth in lightgbm. Higher values increase
      # model complexity but may cause overfitting (also increases train time)
      tree_depth = tree_depth(c(6L, 17L)),

      # Maps to feature_fraction in lightgbm. NOTE: this value is transformed
      # by treesnip and becomes mtry / ncol(data). Max value of 1
      mtry = mtry(c(floor(train_p * 0.2), train_p)),
      
      ### These are custom tuning parameters. See R/bindings.R for more
      ### information about each parameter and its purpose
      lambda_l1 = lambda_l1(c(0.0, 100.0)),
      lambda_l2 = lambda_l2(c(0.0, 100.0)),
      cat_smooth = cat_smooth(c(10.0, 100.0))
    )

  # Use Bayesian tuning to find best performing params. This part takes quite
  # a long time, depending on the compute resources available
  lgbm_search <- tune_bayes(
    object = lgbm_wflow,
    resamples = train_folds,
    initial = model_cv_initial_set,
    iter = model_cv_max_iterations,
    param_info = lgbm_params,
    metrics = metric_set(rmse, mae, mape),
    control = control_bayes(
      verbose = TRUE,
      uncertain = model_cv_no_improve - 2,
      no_improve = model_cv_no_improve,
      seed = model_seed
    )
  )

  # Save tuning results to file. This is a data frame where each row is one
  # CV iteration
  lgbm_search %>%
    ccao::model_axe_tune_data() %>%
    arrow::write_parquet(paths$output$parameter$local)

  # Choose the best model (whichever model minimizes RMSE)
  lgbm_final_params <- lgbm_search %>%
    select_best(metric = "rmse")
  
  # End the cross-validation timer
  tictoc::toc(log = TRUE)
} else {

  # If CV is not enabled, load saved parameters from file if it exists
  # Otherwise use a set of sensible hand-chosen defaults
  if (file.exists(paths$output$parameter$local)) {
    lgbm_final_params <- read_parquet(paths$output$parameter$local) %>%
      select_best(metric = "rmse")
  } else {
    lgbm_final_params <- data.frame(
      min_n = 190L, tree_depth = 13L, mtry = floor(train_p * 0.7),
      lambda_l1 = 1.0, lambda_l2 = 5.0, cat_smooth = 60.0
    )
  }
}


### Step 3 - Fit models

# Fit the final model using the training data and our final hyperparameters
# This is the model used to measure performance on the test set
lgbm_wflow_final_fit <- lgbm_wflow %>%
  ccao::model_lgbm_update_params(lgbm_final_params) %>%
  fit(data = train)

# Fit the final model using the full data (including the test set) and our final
# hyperparameters. This is the model used for actually assessing all properties
lgbm_wflow_final_full_fit <- lgbm_wflow %>%
  ccao::model_lgbm_update_params(lgbm_final_params) %>%
  fit(data = training_data_full)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Finalize Models #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get predictions on the test set using the training data model then save to
# file. These predictions are used to evaluate model performance on the test set 
test %>%
  mutate(
    lgbm = model_predict(
      lgbm_wflow_final_fit %>% extract_fit_parsnip(),
      lgbm_wflow_final_fit %>% extract_recipe(),
      test
    )
  ) %>%
  write_parquet(paths$output$test$local)

# Save the finalized model object to file so it can be used elsewhere. Note the
# model_lgbm_save() function, which uses lgb.save() rather than saveRDS(), since
# lightgbm is picky about how its model objects are stored on disk
lgbm_wflow_final_full_fit %>%
  extract_fit_parsnip() %>%
  ccao::model_lgbm_save(paths$output$workflow$fit$local)

# Save the finalized recipe object to file so it can be used to preprocess
# new data
lgbm_wflow_final_full_fit %>%
  extract_recipe() %>%
  ccao::model_axe_recipe() %>%
  saveRDS(paths$output$workflow$recipe$local)

# End the script timer and write the time elapsed to file
tictoc::toc(log = TRUE)
arrow::read_parquet(paths$output$timing$local) %>%
  bind_rows(., tictoc::tic.log(format = FALSE)) %>%
    arrow::write_parquet(paths$output$timing$local)

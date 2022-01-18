# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Setup #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Start the script timer
tictoc::tic("Train model")

# Load R libraries
options(tidymodels.dark = TRUE)
library(arrow)
library(assessr)
library(butcher)
library(ccao)
library(dplyr)
library(glmnet)
library(here)
library(lightgbm)
library(purrr)
library(stringr)
library(tictoc)
library(tidymodels)
library(treesnip)
library(vctrs)

# Load helpers, recipes, and lightgbm parsnip bindings from files
walk(list.files("R", full.names = TRUE), source)

# Initialize a dictionary of file paths and URIs. See R/helpers.R
paths <- model_file_dict()

# Get number of available physical cores to use for lightgbm multithreading
# lightgbm docs recommend using only real cores, not logical
num_threads <- parallel::detectCores(logical = TRUE) ###########

# Get train/test split proportion and model seed
model_split_prop <- as.numeric(Sys.getenv("MODEL_SPLIT_PROP", 0.90))
model_seed <- as.integer(Sys.getenv("MODEL_SEED"), 27)
set.seed(model_seed)

# Setup cross-validation parameters using .Renviron file
model_cv_enable <- as.logical(Sys.getenv("MODEL_CV_ENABLE", FALSE))
model_cv_num_folds <- as.numeric(Sys.getenv("MODEL_CV_NUM_FOLDS", 7))
model_cv_control <- control_bayes(verbose = TRUE,
  no_improve = 8,
  seed = model_seed
)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Prepare Data #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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
  na.omit() %>%
  .[c(1, 7, 10, 18)] #######################

# Load the full set of training data, then arrange by sale date in order to
# facilitate out-of-time sampling/validation
training_data_full <- read_parquet(paths$input$training$local) %>%
  arrange(meta_sale_date)
  
# Create train/test split by time, with most recent observations in the test set
# We want our best model(s) to be predictive of the future, since properties are
# assessed on the basis of past sales
time_split <- initial_time_split(training_data_full, prop = model_split_prop)
test <- testing(time_split)
train <- training(time_split)

# Create v-fold CV splits of the main training set
train_folds <- vfold_cv(train, v = model_cv_num_folds)

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
  trees = 500,  ####################
  num_leaves = tune(), tree_depth = tune(), min_n = tune(),
  mtry = tune(), loss_reduction = tune(), learn_rate = tune()
) %>%
  set_engine("lightgbm") %>%
  set_mode("regression") %>%
  set_args(
    num_threads = num_threads,
    verbosity = -1L
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
  lgbm_params <- lgbm_model %>%
    parameters() %>%
    update(

      # Most important. Specific to lightgbm. Ideally equal to < 2 ^ tree_depth
      # Higher values increase training time and model complexity
      num_leaves = num_leaves(c(2 ^ 5L, 2 ^ 13L)),

      # Very important. Maps to max_depth in lightgbm. Higher values increase
      # model complexity but may cause overfitting
      tree_depth = tree_depth(c(5L, 13L)),

      # Very important. Maps to min_data_in_leaf in lightgbm. Optimal/large
      # values can help prevent overfitting
      min_n = min_n(c(2L, 200L)),

      # Maps to feature_fraction in lightgbm. NOTE: this value is transformed
      # by treesnip and becomes mtry / ncol(data). Max value of 1
      mtry = mtry(c(5L, train_p)),

      # Maps to min_gain_to_split in lightgbm. Will prevent splitting if the
      # training gain is too small. Higher values reduce training time
      loss_reduction = loss_reduction(c(-4, -2)),

      # Maps to learning_rate in lightgbm. Should be changed in tune with
      # number of trees
      learn_rate = learn_rate(c(-3, -0.5))
    )

  # Use Bayesian tuning to find best performing params. This part takes quite
  # a long time, depending on the compute resources available
  lgbm_search <- tune_bayes(
    object = lgbm_wflow,
    resamples = train_folds,
    initial = 8,
    iter = 25,
    param_info = lgbm_params,
    metrics = metric_set(rmse, mae, mape),
    control = model_cv_control
  )

  # Save tuning results to file. This is a data frame where each row is one
  # CV iteration
  lgbm_search %>%
    ccao::model_axe_tune_data() %>%
    arrow::write_parquet(paths$output$parameter$local)

  # Choose the best model (whichever model minimizes RMSE)
  lgbm_final_params <- lgbm_search %>%
    select_best(metric = "rmse") %>%
    ccao::model_lgbm_cap_num_leaves()
  
  # End the cross-validation timer
  tictoc::toc(log = TRUE)
} else {

  # If CV is not enabled, load saved parameters from file if it exists
  # Otherwise use a set of sensible hand-chosen defaults
  if (file.exists(paths$output$parameter$local)) {
    lgbm_final_params <- read_parquet(paths$output$parameter$local) %>%
      select_best(metric = "rmse") %>%
      ccao::model_lgbm_cap_num_leaves()
  } else {
    lgbm_final_params <- data.frame(
      mtry = 13, min_n = 197, tree_depth = 13, learn_rate = 0.0105,
      loss_reduction = 0.0002, num_leaves = 3381
    ) %>%
      ccao::model_lgbm_cap_num_leaves()
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

# End the script timer
tictoc::toc(log = TRUE)

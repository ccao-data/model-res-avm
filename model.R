# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Setup ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load modeling and data libraries
options(tidymodels.dark = TRUE)
library(arrow)
library(assessr)
library(beepr)
library(ccao)
library(dplyr)
library(purrr)
library(sf)
library(stringr)
library(tictoc)
library(tidymodels)
source("R/recipes.R")
source("R/metrics.R")
source("R/model_funs.R")

# Start full script timer
tictoc::tic(msg = "Full Modeling Complete!")

# Get number of available cores
all_cores <- parallel::detectCores() - 1

# Set seed for reproducibility
set.seed(27)

# Toggle cross validation and set number of folds to use
cv_enable <- as.logical(model_get_env("R_CV_ENABLE", FALSE))
cv_write_params <- as.logical(model_get_env("R_CV_WRITE_PARAMS", FALSE))
cv_num_folds <- as.numeric(model_get_env("R_CV_NUM_FOLDS", 5))
cv_control <- control_bayes(verbose = TRUE, no_improve = 10, seed = 27)

# Get the full list of right-hand side predictors from ccao::vars_dict
mod_predictors <- ccao::vars_dict %>%
  filter(var_is_predictor) %>%
  pull(var_name_standard) %>%
  unique() %>%
  na.omit()




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Loading/Splitting Data #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load the full set of training data, keep only good, complete observations
# Arrange by sale date in order to facilitate out-of-time sampling/validation
full_data <- read_parquet("data/modeldata.parquet") %>%
  filter(ind_arms_length & ind_complete_predictors & !is.na(geo_longitude)) %>%
  arrange(meta_sale_date)

# Create train/test split by time, with most recent observations in the test set
# We want our best model(s) to be predictive of the future
time_split <- initial_time_split(full_data, prop = 0.90)
test <- testing(time_split)
train <- training(time_split)

# Create v-fold CV splits for the main training set
train_folds <- vfold_cv(train, v = cv_num_folds)

# Create a recipe for the training data which removes non-predictor columns,
# normalizes/logs data, and deals with missing values
train_recipe <- mod_recp_prep(train, mod_predictors)
train_p <- ncol(juice(prep(train_recipe))) - 1

# Remove unnecessary data
rm(time_split); gc()




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### ElasticNet Model #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

### Step 1 - Model initialization

# Set model params save path
enet_params_path <- "data/models/enet_params.rds"

# Setup basic ElasticNet model specification
enet_model <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# Define basic ElasticNet model workflow
enet_wflow <- workflow() %>%
  add_model(enet_model) %>%
  add_recipe(train_recipe %>% dummy_recp_prep())


### Step 2 - Cross-validation

# Begin CV tuning if enabled
if (cv_enable) {

  # Create parameter space to search through
  enet_params <- enet_model %>%
    parameters() %>%
    update(penalty = penalty(), mixture = mixture()) %>%
    finalize(select(train, -meta_sale_price))

  # Use Bayesian tuning to find best performing params
  tictoc::tic(msg = "ElasticNet CV model fitting complete!")
  enet_search <- tune_bayes(
    object = enet_wflow,
    resamples = train_folds,
    initial = 5, iter = 50,
    param_info = enet_params,
    metrics = metric_set(rmse, codm),
    control = cv_control
  )
  tictoc::toc(log = TRUE)
  beepr::beep(2)

  # Save tuning results to file
  if (cv_write_params) {
    enet_search %>%
      model_strip_data() %>%
      saveRDS(enet_params_path)
  }

  # Choose the best model that minimizes COD
  enet_final_params <- enet_search %>%
    select_by_one_std_err(
      penalty, mixture,
      metric = "rmse"
    )
  
} else {

  # If no CV, load best params from file if exists, otherwise use defaults
  if (file.exists(enet_params_path)) {
    enet_final_params <- readRDS(enet_params_path) %>%
      select_by_one_std_err(penalty, mixture, metric = "rmse")
  } else {
    enet_final_params <- list(penalty = 1e-10, mixture = 0.3)
  }
}


### Step 3 - Finalize model

# Fit the final model using the full training data
enet_wflow_final_fit <- enet_wflow %>%
  finalize_workflow(as.list(enet_final_params)) %>%
  fit(data = train)

# Extract steps from the fit necessary to predict on the test set
# https://hansjoerg.me/2020/02/09/tidymodels-for-machine-learning/
enet_final_recp <- pull_workflow_prepped_recipe(enet_wflow_final_fit)
enet_final_fit <- pull_workflow_fit(enet_wflow_final_fit)

# Remove unnecessary objects
rm_intermediate("enet")




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### xgboost Model #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

### Step 1 - Model initialization

# Set model params save path
xgb_params_path <- "data/models/xgb_params.rds"

# Initialize xgboost model specification
xgb_model <- boost_tree(
  trees = 500, tree_depth = tune(), min_n = tune(),
  loss_reduction = tune(), sample_size = tune(), mtry = tune(),
  learn_rate = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

# Initialize xgb workflow, note the added recipe for formatting factors
xgb_wflow <- workflow() %>%
  add_model(xgb_model) %>%
  add_recipe(train_recipe %>% dummy_recp_prep())


### Step 2 - Cross-validation

# Begin CV tuning if enabled
if (cv_enable) {

  # Create param search space for xgb
  xgb_params <- xgb_model %>%
    parameters() %>%
    update(
      mtry = mtry(c(5L, floor(train_p / 3))),
      min_n = min_n(c(5L, 20L)),
      tree_depth = tree_depth(c(1L, 5L)),
      loss_reduction = loss_reduction(),
      learn_rate = learn_rate(),
      sample_size = sample_prop()
    ) %>%
    finalize(select(train, -meta_sale_price))

  # Use Bayesian tuning to find best performing params
  tictoc::tic(msg = "XGB CV model fitting complete!")
  xgb_search <- tune_bayes(
    object = xgb_wflow,
    resamples = train_folds,
    initial = 5, iter = 50,
    param_info = xgb_params,
    metrics = metric_set(rmse, codm),
    control = cv_control
  )
  tictoc::toc(log = TRUE)
  beepr::beep(2)

  # Save tuning results to file
  if (cv_write_params) {
    xgb_search %>%
      model_strip_data() %>%
      saveRDS(xgb_params_path)
  }

  # Choose the best model that minimizes COD
  xgb_final_params <- xgb_search %>%
    select_by_one_std_err(
      mtry, min_n, tree_depth, loss_reduction,
      learn_rate, sample_size,
      metric = "rmse"
    )
  
} else {

  # If no CV, load best params from file if exists, otherwise use defaults
  if (file.exists(xgb_params_path)) {
    xgb_final_params <- readRDS(xgb_params_path) %>%
      select_by_one_std_err(
        mtry, min_n, tree_depth, loss_reduction,
        learn_rate, sample_size,
        metric = "rmse"
      )
  } else {
    xgb_final_params <- list(
      tree_depth = 13, min_n = 9, loss_reduction = 0.0125,
      mtry = 10, sample_size = 0.5, learn_rate = 0.05
    )
  }
}


### Step 3 - Finalize model

# Fit the final model using the full training data
xgb_wflow_final_fit <- xgb_wflow %>%
  finalize_workflow(as.list(xgb_final_params)) %>%
  fit(data = train)

# Extract steps from the fit necessary to predict on the test set
xgb_final_recp <- pull_workflow_prepped_recipe(xgb_wflow_final_fit)
xgb_final_fit <- pull_workflow_fit(xgb_wflow_final_fit)

# Remove unnecessary objects
rm_intermediate("xgb")




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Random Forest Model #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

### Step 1 - Model initialization

# Set model params save path
rf_params_path <- "data/models/rf_params.rds"

# Initialize RF model specification
rf_model <- rand_forest(trees = tune(), mtry = tune(), min_n = tune()) %>%
  set_mode("regression") %>%
  set_engine("ranger") %>%
  set_args(num.threads = all_cores, verbose = TRUE)

# Initialize RF workflow
rf_wflow <- workflow() %>%
  add_model(rf_model) %>%
  add_recipe(train_recipe)


### Step 2 - Cross-validation

# Begin CV tuning if enabled
if (cv_enable) {

  # Create param search space for RF
  rf_params <- rf_model %>%
    parameters() %>%
    update(
      trees = trees(range = c(500, 1000)),
      mtry = mtry(range = c(5, floor(train_p / 3))),
      min_n = min_n()
    ) %>%
    finalize(select(train, -meta_sale_price))

  # Use Bayesian tuning to find best performing params
  tictoc::tic(msg = "RF CV model fitting complete!")
  rf_search <- tune_bayes(
    object = rf_wflow,
    resamples = train_folds,
    initial = 5, iter = 50,
    param_info = rf_params,
    metrics = metric_set(rmse, codm),
    control = cv_control
  )
  tictoc::toc(log = TRUE)
  beepr::beep(2)

  # Save tuning results to file
  if (cv_write_params) {
    rf_search %>%
      model_strip_data() %>%
      saveRDS(rf_params_path)
  }

  # Choose the best model that minimizes COD
  rf_final_params <- rf_search %>%
    select_by_one_std_err(mtry, min_n, metric = "rmse")
  
} else {

  # If no CV, load best params from file if exists, otherwise use defaults
  if (file.exists(rf_params_path)) {
    rf_final_params <- readRDS(rf_params_path) %>%
      select_by_one_std_err(mtry, min_n, metric = "rmse")
  } else {
    rf_final_params <- list(trees = 1000, mtry = 12, min_n = 13)
  }
}


### Step 3 - Finalize model

# Fit the final model using the full training data
rf_wflow_final_fit <- rf_wflow %>%
  finalize_workflow(as.list(rf_final_params)) %>%
  fit(data = train)

# Extract steps from the fit necessary to predict on the test set
rf_final_recp <- pull_workflow_prepped_recipe(rf_wflow_final_fit)
rf_final_fit <- pull_workflow_fit(rf_wflow_final_fit)

# Remove unnecessary objects
rm_intermediate("rf")




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Stacked Model (Regularized Regression) #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Initialize model specification for meta model
sm_meta_model <- linear_reg(penalty = 0.01, mixture = 0) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# Prepare lists of final fitted models and recipes for input to stacked model
sm_models = list(
  "enet" = enet_final_fit,
  "xgb" = xgb_final_fit,
  "rf" = rf_final_fit
)
sm_recipes = list(
  "enet" = enet_final_recp,
  "xgb" = xgb_final_recp,
  "rf" = rf_final_recp
)

# Create stacked model object with training data
# This model is used to evaluate performance on the test set
sm_final_fit_test <- stack_model(
  models = sm_models,
  recipes = sm_recipes,
  meta_spec = sm_meta_model,
  add_vars = "meta_town_code",
  data = train
)

# Get predictions on the test set using the stacked model then save to file
test %>%
  bind_cols(predict(sm_final_fit_test, test)) %>%
  write_parquet("data/testdata.parquet")




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Finish Up #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Generate modeling diagnostic/performance report
rmarkdown::render(
  input = "report.Rmd",
  output_file = "report.html"
)

# Stop all timers and write CV timers to file
tictoc::toc(log = TRUE)
if (cv_enable & cv_write_params) {
  bind_rows(tic.log(format = FALSE)) %>%
    mutate(elapsed = toc - tic, model = tolower(word(msg, 1))) %>%
    saveRDS("data/models/model_timings.rds")
}

# BIG BEEP
beepr::beep(8)

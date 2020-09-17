# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Setup ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load R libraries
options(tidymodels.dark = TRUE)
library(arrow) 
library(assessr)
library(beepr)
library(ccao)
library(dplyr)
library(here)
library(purrr)
library(sf)
library(stringr)
library(tictoc)
library(tidymodels)
library(treesnip)
source("R/recipes.R")
source("R/metrics.R")
source("R/model_funs.R")

# Get number of available cores and number of threads to use per core
num_threads <- parallel::detectCores(logical = TRUE) - 1

# Start full script timer
tictoc::tic(msg = "Full Modeling Complete!")

# Set seed for reproducibility
set.seed(27)

# Toggle cross validation and set number of folds to use
cv_enable <- as.logical(model_get_env("R_CV_ENABLE", FALSE))
cv_write_params <- as.logical(model_get_env("R_CV_WRITE_PARAMS", FALSE))
cv_num_folds <- as.numeric(model_get_env("R_CV_NUM_FOLDS", 5))
cv_control <- control_bayes(verbose = TRUE, no_improve = 10, seed = 27)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Preparing Data #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# List of variables that uniquely identify each structure
mod_id_vars <- c("meta_pin", "meta_class", "meta_multi_code")

# Get the full list of right-hand side predictors from ccao::vars_dict
mod_predictors <- ccao::vars_dict %>%
  filter(var_is_predictor) %>%
  pull(var_name_standard) %>%
  unique() %>%
  na.omit()

# Load the full set of training data, keep only good, complete observations
# Arrange by sale date in order to facilitate out-of-time sampling/validation
full_data <- read_parquet(here("input", "modeldata.parquet")) %>%
  filter(ind_arms_length & ind_complete_predictors & !is.na(geo_longitude)) %>%
  arrange(meta_sale_date)

# Create train/test split by time, with most recent observations in the test set
# We want our best model(s) to be predictive of the future, since properties are
# assessed on the basis of past sales
time_split <- initial_time_split(full_data, prop = 0.90)
test <- testing(time_split)
train <- training(time_split)

# Create v-fold CV splits for the main training set
train_folds <- vfold_cv(train, v = cv_num_folds)

# Create a recipe for the training data which removes non-predictor columns,
# normalizes/logs data, and removes/imputes with missing values
train_recipe <- mod_recp_prep(
  data = train,
  keep_vars = mod_predictors, 
  id_vars = mod_id_vars
)

# Extract number of cols used in model (P) and categorical column names
juiced_train <- juice(prep(train_recipe)) %>% 
  select(-any_of(c(mod_id_vars, "meta_sale_price")))

train_p <- ncol(juiced_train)
train_cat_vars <- juiced_train %>%
  select(where(is.factor)) %>%
  colnames()

# Remove unnecessary data
rm(time_split, juiced_train); gc()




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### ElasticNet Model #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

### Step 1 - Model initialization

# Set model params save path
enet_params_path <- here("output", "params", "enet_params.rds")

# Setup basic ElasticNet model specification
enet_model <- linear_reg(penalty = 1e-7, mixture = 0.16) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# Define basic ElasticNet model workflow
enet_wflow <- workflow() %>%
  add_model(enet_model) %>%
  add_recipe(train_recipe %>% dummy_recp_prep())


### Step 2 - Fit the model

# Fit the final model using the training data
enet_wflow_final_fit <- enet_wflow %>%
  fit(data = train)

# Remove unnecessary objects
rm_intermediate("enet")




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### XGBoost Model #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

### Step 1 - Model initialization

# Set model params save path
xgb_params_path <- here("output", "params", "xgb_params.rds")

# Initialize xgboost model specification
xgb_model <- boost_tree(
  trees = tune(), tree_depth = tune(), min_n = tune(),
  loss_reduction = tune(), sample_size = tune(), 
  mtry = tune(), learn_rate = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("regression") %>%
  set_args(nthread = num_threads)

# Initialize xgboost workflow, note the added recipe for formatting factors
# Here categoricals are explicitly converted to one-hot encoding, since xgboost
# doesn't have built in categorical handling like lightgbm and catboost
xgb_wflow <- workflow() %>%
  add_model(xgb_model) %>%
  add_recipe(train_recipe %>% dummy_recp_prep())


### Step 2 - Cross-validation

# Begin CV tuning if enabled
if (cv_enable) {
  
  # Create param search space for xgboost
  xgb_params <- xgb_model %>%
    parameters() %>%
    update(
      trees = trees(range = c(500, 1500)),
      mtry = mtry(c(5L, floor(train_p / 3))),
      min_n = min_n(),
      tree_depth = tree_depth(c(3L, 12L)),
      loss_reduction = loss_reduction(c(-3, 0.5)),
      learn_rate = learn_rate(c(-3, -0.3)),
      sample_size = sample_prop()
    )
  
  # Use Bayesian tuning to find best performing params
  tictoc::tic(msg = "XGBoost CV model fitting complete!")
  xgb_search <- tune_bayes(
    object = xgb_wflow,
    resamples = train_folds,
    initial = 5, iter = 50,
    param_info = xgb_params,
    metrics = metric_set(rmse, codm, rsq),
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
  
  # Choose the best model that minimizes RMSE
  xgb_final_params <- select_best(xgb_search, metric = "rmse")
  
} else {
  
  # If no CV, load best params from file if exists, otherwise use defaults
  if (file.exists(xgb_params_path)) {
    xgb_final_params <- select_best(readRDS(xgb_params_path), metric = "rmse")
  } else {
    xgb_final_params <- list(
      trees = 1500, tree_depth = 13, min_n = 9, loss_reduction = 0.0125,
      mtry = 10, sample_size = 0.5, learn_rate = 0.05
    )
  }
}


### Step 3 - Finalize model

# Fit the final model using the training data
xgb_wflow_final_fit <- xgb_wflow %>%
  finalize_workflow(as.list(xgb_final_params)) %>%
  fit(data = train)

# Fit the final model using the full data, this is the model used for assessment
xgb_wflow_final_full_fit <- xgb_wflow %>%
  finalize_workflow(as.list(xgb_final_params)) %>%
  fit(data = full_data)

# Remove unnecessary objects
rm_intermediate("xgb")




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### LightGBM Model #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

### Step 1 - Model initialization

# Set model params save path
lgbm_params_path <- here("output", "params", "lgbm_params.rds")

# Initialize lightbgm model specification
# Note that categorical vars must be explicitly specified for lightgbm
lgbm_model <- boost_tree(
  trees = tune(), tree_depth = tune(), min_n = tune(),
  loss_reduction = tune(), sample_size = tune(),
  mtry = tune(), learn_rate = tune()
) %>%
  set_engine("lightgbm") %>%
  set_mode("regression") %>%
  set_args(
    num_threads = num_threads,
    categorical_feature = train_cat_vars,
    verbose = -1 
  )

# Initialize lightgbm workflow, note the added recipe for formatting factors
lgbm_wflow <- workflow() %>%
  add_model(lgbm_model) %>%
  add_recipe(train_recipe)


### Step 2 - Cross-validation

# Begin CV tuning if enabled
if (cv_enable) {

  # Create param search space for lgbm
  lgbm_params <- lgbm_model %>%
    parameters() %>%
    update(
      trees = trees(range = c(500, 1500)),
      mtry = mtry(c(5L, floor(train_p / 3))),
      min_n = min_n(),
      tree_depth = tree_depth(c(3L, 12L)),
      loss_reduction = loss_reduction(c(-3, 0.5)),
      learn_rate = learn_rate(c(-3, -0.3)),
      sample_size = sample_prop()
    )

  # Use Bayesian tuning to find best performing params
  tictoc::tic(msg = "LightGBM CV model fitting complete!")
  lgbm_search <- tune_bayes(
    object = lgbm_wflow,
    resamples = train_folds,
    initial = 5, iter = 50,
    param_info = lgbm_params,
    metrics = metric_set(rmse, codm, rsq),
    control = cv_control
  )
  tictoc::toc(log = TRUE)
  beepr::beep(2)

  # Save tuning results to file
  if (cv_write_params) {
    lgbm_search %>%
      model_strip_data() %>%
      saveRDS(lgbm_params_path)
  }

  # Choose the best model that minimizes RMSE
  lgbm_final_params <- select_best(lgbm_search, metric = "rmse")
  
} else {

  # If no CV, load best params from file if exists, otherwise use defaults
  if (file.exists(lgbm_params_path)) {
    lgbm_final_params <- select_best(readRDS(lgbm_params_path), metric = "rmse")
  } else {
    lgbm_final_params <- list(
      trees = 1500, tree_depth = 5, min_n = 8, loss_reduction = 0.2613,
      mtry = 8, sample_size = 0.66, learn_rate = 0.0175
    )
  }
}


### Step 3 - Finalize model

# Fit the final model using the training data
lgbm_wflow_final_fit <- lgbm_wflow %>%
  finalize_workflow(as.list(lgbm_final_params)) %>%
  fit(data = train)

# Fit the final model using the full data, this is the model used for assessment
lgbm_wflow_final_full_fit <- lgbm_wflow %>%
  finalize_workflow(as.list(lgbm_final_params)) %>%
  fit(data = full_data)

# Remove unnecessary objects
rm_intermediate("lgbm")




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### CatBoost Model #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

### Step 1 - Model initialization

# Set model params save path
cat_params_path <- here("output", "params", "cat_params.rds")

# Initialize catboost model specification
# treesnip CatBoost implementation detects categorical columns automatically
# https://github.com/curso-r/treesnip/blob/master/R/catboost.R#L237 
cat_model <- boost_tree(
  trees = tune(), tree_depth = tune(), min_n = tune(),
  sample_size = tune(), mtry = tune(), learn_rate = tune()
) %>%
  set_engine("catboost") %>%
  set_mode("regression") %>%
  set_args(nthread = num_threads)

# Initialize catboost workflow, note the added recipe for formatting factors
cat_wflow <- workflow() %>%
  add_model(cat_model) %>%
  add_recipe(train_recipe)


### Step 2 - Cross-validation

# Begin CV tuning if enabled
if (cv_enable) {
  
  # Create param search space for catboost
  cat_params <- cat_model %>%
    parameters() %>%
    update(
      trees = trees(range = c(500, 1500)),
      mtry = mtry(c(5L, floor(train_p / 3))),
      min_n = min_n(),
      tree_depth = tree_depth(c(3L, 12L)),
      learn_rate = learn_rate(c(-3, -0.3)),
      sample_size = sample_prop()
    )
  
  # Use Bayesian tuning to find best performing params
  tictoc::tic(msg = "CatBoost CV model fitting complete!")
  cat_search <- tune_bayes(
    object = cat_wflow,
    resamples = train_folds,
    initial = 5, iter = 50,
    param_info = cat_params,
    metrics = metric_set(rmse, codm, rsq),
    control = cv_control
  )
  tictoc::toc(log = TRUE)
  beepr::beep(2)
  
  # Save tuning results to file
  if (cv_write_params) {
    cat_search %>%
      model_strip_data() %>%
      saveRDS(cat_params_path)
  }
  
  # Choose the best model that minimizes RMSE
  cat_final_params <- select_best(cat_search, metric = "rmse")
  
} else {
  
  # If no CV, load best params from file if exists, otherwise use defaults
  if (file.exists(cat_params_path)) {
    cat_final_params <- select_best(readRDS(cat_params_path), metric = "rmse")
  } else {
    cat_final_params <- list(
      trees = 1500, tree_depth = 5, min_n = 8,
      mtry = 8, sample_size = 0.66, learn_rate = 0.0175
    )
  }
}


### Step 3 - Finalize model

# Fit the final model using the training data
cat_wflow_final_fit <- cat_wflow %>%
  finalize_workflow(as.list(cat_final_params)) %>%
  fit(data = train)

# Fit the final model using the full data, this is the model used for assessment
cat_wflow_final_full_fit <- cat_wflow %>%
  finalize_workflow(as.list(cat_final_params)) %>%
  fit(data = full_data)

# Remove unnecessary objects
rm_intermediate("cat")




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Stacked Model #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

### Step 1 - Model initialization

# Initialize model specification for meta model. In this case we're using a
# simple regularized (ridge) regression
sm_meta_model <- linear_reg(penalty = 0.01, mixture = 0) %>%
  set_engine("glmnet") %>%
  set_mode("regression")


### Step 2 - Predict on test set

# Create stacked model object with training data, including only gbm models
# This model is used to evaluate performance on the test set
# Fit models and recipes are extracted from the final saved workflow
# https://hansjoerg.me/2020/02/09/tidymodels-for-machine-learning/
sm_final_fit <- stack_model(
  specs = list(
    "xgb" = xgb_wflow_final_fit %>% pull_workflow_fit(),
    "lgbm" = lgbm_wflow_final_fit %>% pull_workflow_fit(),
    "cat" = cat_wflow_final_fit %>% pull_workflow_fit()
  ),
  recipes = list(
    "xgb" = xgb_wflow_final_fit %>% pull_workflow_prepped_recipe(),
    "lgbm" = lgbm_wflow_final_fit %>% pull_workflow_prepped_recipe(),
    "cat" = cat_wflow_final_fit %>% pull_workflow_prepped_recipe()
  ),
  meta_spec = sm_meta_model,
  meta_keep_vars = "meta_town_code",
  data = train
)

# Get predictions on the test set using the stacked model then save to file
# Also predict using the baseline linear model
test %>%
  mutate(
    enet = model_predict(
      enet_wflow_final_fit %>% pull_workflow_fit(),
      enet_wflow_final_fit %>% pull_workflow_prepped_recipe(),
      test
    )
  ) %>%
  bind_cols(predict(sm_final_fit, test)) %>%
  write_parquet(here("output", "data", "testdata.parquet"))


### Step 3 - Create finalized assessment model

# Create a model fit from the full sales dataset using hyperparameters
# discovered during the cross-validation process. This is the model used to
# actually created initial assessed values
sm_final_full_fit <- stack_model(
  specs = list(
    "xgb" = xgb_wflow_final_full_fit %>% pull_workflow_fit(),
    "lgbm" = lgbm_wflow_final_full_fit %>% pull_workflow_fit(),
    "cat" = cat_wflow_final_full_fit %>% pull_workflow_fit()
  ),
  recipes = list(
    "xgb" = xgb_wflow_final_full_fit %>% pull_workflow_prepped_recipe(),
    "lgbm" = lgbm_wflow_final_full_fit %>% pull_workflow_prepped_recipe(),
    "cat" = cat_wflow_final_full_fit %>% pull_workflow_prepped_recipe()
  ),
  meta_spec = sm_meta_model,
  meta_keep_vars = "meta_town_code",
  data = full_data
)

# Save the finalized model object to file so it can be used elsewhere
sm_final_full_fit %>%
  saveRDS(here("output", "models", "stacked_model.rds"))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Finish Up #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Generate modeling diagnostic/performance report
rmarkdown::render(
  input = here("reports", "model_report.Rmd"),
  output_file = here("output", "reports", "model_report.html")
)

# Stop all timers and write CV timers to file
tictoc::toc(log = TRUE)
if (cv_enable & cv_write_params) {
  bind_rows(tic.log(format = FALSE)) %>%
    mutate(elapsed = toc - tic, model = tolower(word(msg, 1))) %>%
    saveRDS(here("output", "params", "model_timings.rds"))
}

# BIG BEEP
beepr::beep(8)

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
library(furrr)
library(purrr)
library(sf)
library(stringr)
library(doFuture)
library(tictoc)
library(tidymodels)
source("R/cknn_funs.R")
source("R/recipes.R")
source("R/metrics.R")
source("R/model_funs.R")

# Start full script timer
tictoc::tic(msg = "Full Modeling Complete!")

# Get number of available cores
all_cores <- availableCores() - 1

# Set seed for reproducibility
set.seed(27)

# Toggle cross validation and set number of folds to use
cv_enable <- as.logical(model_get_env("R_CV_ENABLE", FALSE))
cv_write_results <- as.logical(model_get_env("R_CV_WRITE_RESULTS", FALSE))
cv_num_folds <- as.numeric(model_get_env("R_CV_NUM_FOLDS", 5))
cv_control <- control_bayes(verbose = TRUE, no_improve = 5, seed = 27)

# Get the full list of possible RHS predictors from ccao::vars_dict
mod_predictors <- ccao::vars_dict %>%
  filter(var_is_predictor) %>%
  pull(var_name_standard) %>%
  unique() %>%
  na.omit()




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Loading/Splitting Data #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load the full set of training data, keep only good, complete observations
full_data <- read_parquet("data/modeldata.parquet") %>%
  filter(ind_arms_length & ind_complete_predictors & !is.na(geo_longitude)) %>%

  # Transform from lat/lon to planar coordinates in meters, necessary for
  # spatial clustering
  st_as_sf(coords = c("geo_longitude", "geo_latitude"), crs = 4326) %>%
  st_transform(3435) %>%
  mutate(
    geo_longitude = st_coordinates(.)[, 1],
    geo_latitude = st_coordinates(.)[, 2]
  ) %>%
  st_set_geometry(NULL) %>%
  arrange(meta_sale_date)

# Create train/test split by time, with most recent observations in the test set
# We want our best model(s) to be predictive of the future
time_split <- initial_time_split(full_data, prop = 0.90)
test <- testing(time_split)
train <- training(time_split)

# Create v-fold CV splits for the main training set
train_folds <- vfold_cv(train, v = cv_num_folds)

# Setup training data recipe to remove unnecessary vars, log-log variables, etc
train_recipe <- mod_recp_prep(train, mod_predictors)
train_p <- ncol(juice(prep(train_recipe))) - 1

# Remove unnecessary data
rm(full_data, time_split)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### ElasticNet Model #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

### Step 1 - Model initialization

# Set model params save path
enet_params_path <- "data/models/enet_results.rds"

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
    metrics = metric_set(codm, rmse),
    control = cv_control
  )
  tictoc::toc(log = TRUE)
  beepr::beep(2)

  # Save tuning results to file
  if (cv_write_results) {
    enet_search %>%
      model_strip_data() %>%
      saveRDS(enet_params_path)
  }

  # Choose the best model that minimizes COD
  enet_final_params <- enet_search %>%
    select_by_one_std_err(
      penalty, mixture,
      metric = "codm"
    )
} else {

  # If no CV, load best params from file if exists, otherwise use defaults
  if (file.exists(enet_params_path)) {
    enet_final_params <- readRDS(enet_params_path) %>%
      select_by_one_std_err(penalty, mixture, metric = "codm")
  } else {
    enet_final_params <- list(penalty = 1e-10, mixture = 0.3)
  }
}


### Step 3 - Finalize model

# Fit the final model using the full training data
enet_final_fit <- enet_wflow %>%
  finalize_workflow(as.list(enet_final_params)) %>%
  fit(data = train)

# Extract steps from the fit necessary to predict on the test set
# https://hansjoerg.me/2020/02/09/tidymodels-for-machine-learning/
enet_final_recp <- pull_workflow_prepped_recipe(enet_final_fit)
enet_final_fit <- pull_workflow_fit(enet_final_fit)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### xgboost Model #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

### Step 1 - Model initialization

# Set model params save path
xgb_params_path <- "data/models/xgb_results.rds"

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
    metrics = metric_set(codm, rmse),
    control = cv_control
  )
  tictoc::toc(log = TRUE)
  beepr::beep(2)

  # Save tuning results to file
  if (cv_write_results) {
    xgb_search %>%
      model_strip_data() %>%
      saveRDS(xgb_params_path)
  }

  # Choose the best model that minimizes COD
  xgb_final_params <- xgb_search %>%
    select_by_one_std_err(
      mtry, min_n, tree_depth, loss_reduction,
      learn_rate, sample_size,
      metric = "codm"
    )
} else {

  # If no CV, load best params from file if exists, otherwise use defaults
  if (file.exists(xgb_params_path)) {
    xgb_final_params <- readRDS(xgb_params_path) %>%
      select_by_one_std_err(
        mtry, min_n, tree_depth, loss_reduction,
        learn_rate, sample_size,
        metric = "codm"
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




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Random Forest Model #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

### Step 1 - Model initialization

# Set model params save path
rf_params_path <- "data/models/rf_results.rds"

# Initialize RF model specification
rf_model <- rand_forest(mtry = tune(), trees = 500, min_n = tune()) %>%
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
    metrics = metric_set(codm, rmse),
    control = cv_control
  )
  tictoc::toc(log = TRUE)
  beepr::beep(2)

  # Save tuning results to file
  if (cv_write_results) {
    rf_search %>%
      model_strip_data() %>%
      saveRDS(rf_params_path)
  }

  # Choose the best model that minimizes COD
  rf_final_params <- rf_search %>%
    select_by_one_std_err(mtry, min_n, metric = "codm")
} else {

  # If no CV, load best params from file if exists, otherwise use defaults
  if (file.exists(rf_params_path)) {
    rf_final_params <- readRDS(rf_params_path) %>%
      select_by_one_std_err(mtry, min_n, metric = "codm")
  } else {
    rf_final_params <- list(mtry = 12, min_n = 13)
  }
}


### Step 3 - Finalize model

# Fit the final model using the full training data
rf_wflow_final_fit <- rf_wflow %>%
  update_model(rf_model %>% set_args(importance = "impurity")) %>%
  finalize_workflow(as.list(rf_final_params)) %>%
  fit(data = train)

# Extract steps from the fit necessary to predict on the test set
rf_final_recp <- pull_workflow_prepped_recipe(rf_wflow_final_fit)
rf_final_fit <- pull_workflow_fit(rf_wflow_final_fit)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### CkNN Model #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

### Step 0 - Initialize parallel backend for furrr functions used by cknn
if (cv_enable) plan(multiprocess, workers = all_cores)


### Step 1 - Model initialization and determine variable weights

# Setup model results path
cknn_params_path <- "data/models/cknn_results.rds"
cknn_weights_path <- "data/models/cknn_weights.rds"

# Get the set of possible vars for clustering from ccao::vars_dict
cknn_possible_vars <- ccao::vars_dict %>%
  filter(var_is_clustered) %>%
  pull(var_name_standard) %>%
  unique() %>%
  na.omit()

# Create feature weights using feature importance metric from random forest
# keeping the top N weights
cknn_noncluster_vars <- c("geo_longitude", "geo_latitude", "meta_sale_price")
cknn_var_weights <- cknn_rel_importance(rf_final_fit, cknn_possible_vars, 10)
cknn_predictors <- c(cknn_noncluster_vars, names(cknn_var_weights))

# Save variable weights used to file
if (cv_write_results) {
  enframe(cknn_var_weights) %>%
    saveRDS(cknn_weights_path)
}

# Create a recipe using cknn predictors which removes unnecessary vars,
# collapses rare factors, and converts NA in factors to "unknown"
cknn_recipe <- cknn_recp_prep(train, cknn_predictors)


### Step 2 - Cross-validation

# If cross validating, manually cycle through a grid of tuning parameters to
# find the best ones
if (cv_enable) {

  # Create a grid of possible cknn parameter values to loop through
  cknn_grid <- expand_grid(
    m = seq(7, 16, 2),
    k = seq(7, 19, 3),
    l = seq(0.5, 0.9, 0.1)
  ) %>%
    sample_n(60)

  # Create v folds in the training data and keep only clustering vars, then for
  # each CV fold, calculate the values for all hyperparameters in cknn_grid
  tictoc::tic(msg = "CkNN CV model fitting complete!")
  cknn_search <- train_folds %>%
    mutate(
      df_ana = map(splits, analysis),
      df_ass = map(splits, assessment)
    ) %>%
    mutate(
      recipe = map(df_ana, ~ prep(cknn_recipe, training = .x)),
      df_ana = map(recipe, juice),
      df_ass = map2(recipe, df_ass, ~ bake(.x, new_data = .y))
    ) %>%
    mutate(
      cknn_results = cknn_search(
        analysis = df_ana,
        assessment = df_ass,
        param_grid = cknn_grid,
        noncluster_vars = cknn_noncluster_vars,
        weights = cknn_var_weights
      )
    )
  tictoc::toc(log = TRUE)
  beepr::beep(2)

  # Summarize results by parameter groups, averaging across folds
  cknn_results <- bind_rows(cknn_search$cknn_results) %>%
    group_by(m, k, l) %>%
    summarize(across(everything(), mean)) %>%
    ungroup() %>%
    arrange(cod)

  # Write grid search results to file
  if (cv_write_results) {
    cknn_results %>%
      saveRDS(cknn_params_path)
  }

  # Take the best params according to lowest COD
  cknn_final_params <- cknn_results %>%
    filter(cod == min(cod, na.rm = TRUE))
} else {

  # If no CV, load best params from file if exists, otherwise use defaults
  if (file.exists(cknn_params_path)) {
    cknn_final_params <- readRDS(cknn_params_path) %>%
      filter(cod == min(cod, na.rm = TRUE))
  } else {
    cknn_final_params <- list(m = 8, k = 12, l = 0.8)
  }
}


### Step 3 - Finalize model

# Fit the final model using the full training data
cknn_final_fit <- cknn(
  data = juice(prep(cknn_recipe)) %>%
    select(-meta_sale_price, -geo_longitude, -geo_latitude) %>%
    as.data.frame(),
  lon = train %>% pull(geo_longitude),
  lat = train %>% pull(geo_latitude),
  m = cknn_final_params$m,
  k = cknn_final_params$k,
  l = cknn_final_params$l,
  var_weights = cknn_var_weights,
  keep_data = TRUE
)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Meta Model (Linear Regression) #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

meta_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

stacked_model <- stack_model(
  models = list(
    "enet" = enet_final_fit,
    "xgb" = xgb_final_fit,
    "rf" = rf_final_fit,
    "cknn" = cknn_final_fit
  ),
  recipes = list(
    "enet" = enet_final_recp,
    "xgb" = xgb_final_recp,
    "rf" = rf_final_recp,
    "cknn" = cknn_recipe
  ),
  meta_spec = meta_model,
  data = train
)

stacked_preds <- predict(stacked_model, test)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Finish Up #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Save test set results to file then generate report
test %>%
  bind_cols(stacked_preds) %>%
  write_parquet("data/testdata.parquet")

# Generate modeling report
rmarkdown::render(
  input = "report.Rmd",
  output_file = "report.html"
)

# Stop all timers and write CV timers to log file
tictoc::toc(log = TRUE)
if (cv_enable & cv_write_results) {
  bind_rows(tic.log(format = FALSE)) %>%
    mutate(elapsed = toc - tic, model = tolower(word(msg, 1))) %>%
    saveRDS("data/models/model_timings.rds")
}

# BEEP!
beepr::beep(8)


# TODO: CHECK/FINISH STACKED MODEL INTERFACE, ADD CKNN
# TODO: Switch Cknn to random grid search

# TODO: Save features weights and tuning params
# TODO: Add autoplots to report, add outlier analysis
# TODO: Add correlation between models
# TODO: Test log transforming cknn continuous vars


# TODO: Create interaction terms: step_interact().
# TODO: figure out how to have a single model interface for training/prediction

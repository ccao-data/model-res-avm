# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Setup ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load R libraries
options(tidymodels.dark = TRUE)
library(arrow) 
library(assessr)
library(beepr)
library(butcher)
library(ccao)
library(dplyr)
library(glmnet)
library(here)
library(lightgbm)
library(purrr)
library(sf)
library(stringr)
library(tictoc)
library(tidymodels)
library(treesnip)
library(vctrs)

# Load helper functions from file
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
cv_control <- control_bayes(verbose = TRUE, no_improve = 15, seed = 27)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Prepare Data #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# List of variables that uniquely identify each structure or sale
mod_id_vars <- c(
  "meta_pin", "meta_class", "meta_multi_code", "meta_document_num"
)

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

# Extract number of columns actually used in model (P)
juiced_train <- juice(prep(train_recipe)) %>% 
  select(-any_of(c(mod_id_vars, "meta_sale_price")))
train_p <- ncol(juiced_train)

# Remove unnecessary data
rm(time_split, juiced_train); gc()




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### ElasticNet Model #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Simple linear model to use as a baseline for comparison. Uses glmnet since it
# seems to be a bit faster than lm()


### Step 1 - Model initialization

# Setup basic ElasticNet model specification
enet_model <- linear_reg(penalty = 1e-7, mixture = 0.16) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# Define basic ElasticNet model workflow
enet_wflow <- workflow() %>%
  add_model(enet_model) %>%
  add_recipe(train_recipe %>% dummy_recp_prep())


### Step 2 - Fit the model

# Fit the final model using the training data, this will be used on the test set
enet_wflow_final_fit <- enet_wflow %>%
  fit(data = train)

# Remove unnecessary objects
rm_intermediate("enet")




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### LightGBM Model #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Non-linear boosted tree model that performs better than xgboost/catboost and
# trains faster. See https://lightgbm.readthedocs.io/ for more information.

# Note that the num_leaves parameter is not optimized here, as parsnip does not
# yet support it as a hyperparameter


### Step 1 - Model initialization

# Set model params save path
lgbm_params_path <- here("output", "params", "lgbm_params.rds")

# Initialize lightbgm model specification
# Note that categorical columns are detected automatically by treesnip's
# lightgbm parsnip implementation as long as they are factors
lgbm_model <- boost_tree(
  trees = 1500, tree_depth = tune(), min_n = tune(),
  loss_reduction = tune(), sample_size = tune(),
  mtry = tune(), learn_rate = tune()
) %>%
  set_engine("lightgbm") %>%
  set_mode("regression") %>%
  set_args(
    num_threads = num_threads,
    verbose = -1 
  )

# Initialize lightgbm workflow
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
    initial = 10, iter = 50,
    param_info = lgbm_params,
    metrics = metric_set(rmse, codm, rsq),
    control = cv_control
  )
  tictoc::toc(log = TRUE)
  beepr::beep(2)
  
  # Save tuning results to file
  if (cv_write_params) {
    lgbm_search %>%
      model_axe_tune_data() %>%
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
      trees = 1500, tree_depth = 9, min_n = 17, loss_reduction = 0.056,
      mtry = 13, sample_size = 0.448, learn_rate = 0.0158
    )
  }
}


### Step 3 - Finalize model

# Fit the final model using the training data, this is the model used to measure
# performance on the test set
lgbm_wflow_final_fit <- lgbm_wflow %>%
  finalize_workflow(as.list(lgbm_final_params)) %>%
  fit(data = train)

# Fit the final model using the full data, this is the model used for actually
# assessing all properties
lgbm_wflow_final_full_fit <- lgbm_wflow %>%
  finalize_workflow(as.list(lgbm_final_params)) %>%
  fit(data = full_data)

# Remove unnecessary objects
rm_intermediate("lgbm")




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Finalize Models #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

### Get predictions and save objects

# Get predictions on the test set using all models then save to file. These 
# predictions are used to evaluate model performance in model_report.Rmd
test %>%
  mutate(
    enet = model_predict(
      enet_wflow_final_fit %>% pull_workflow_fit(),
      enet_wflow_final_fit %>% pull_workflow_prepped_recipe(),
      test
    ),
    lgbm = model_predict(
      lgbm_wflow_final_fit %>% pull_workflow_fit(),
      lgbm_wflow_final_fit %>% pull_workflow_prepped_recipe(),
      test
    )
  ) %>%
  write_parquet(here("output", "data", "testdata.parquet"))

# Save the finalized model object to file so it can be used elsewhere
lgbm_wflow_final_full_fit %>%
  pull_workflow_fit() %>%
  model_save(here("output", "models", "lgbm_model.zip"))
  
# Save the finalized recipe object to file so it can be used to prep new data
lgbm_wflow_final_full_fit %>%
  pull_workflow_prepped_recipe() %>%
  model_axe_recipe() %>%
  saveRDS(here("output", "models", "lgbm_recipe.rds"))


### Generate reports

# Stop all timers and write CV timers to file
tictoc::toc(log = TRUE)
if (cv_enable & cv_write_params) {
  bind_rows(tic.log(format = FALSE)) %>%
    mutate(elapsed = toc - tic, model = tolower(word(msg, 1))) %>%
    saveRDS(here("output", "params", "model_timings.rds"))
}

# Generate modeling diagnostic/performance report
rmarkdown::render(
  input = here("reports", "model_report.Rmd"),
  output_file = here("output", "reports", "model_report.html")
)

# BIG BEEP
beepr::beep(8)
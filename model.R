# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
##### Setup ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Load the necessary libraries
library(tidymodels)
library(stringr)
library(arrow)
library(dplyr)
library(purrr)
library(sf)
library(assessr)
library(ccao)
library(furrr)
library(tictoc)
source("R/cknn_funs.R")
source("R/recipes.R")
source("R/metrics.R")

# Set seed for reproducibility 
set.seed(27)

# Set number of folds to use for all cross validation
cv_enable <- FALSE
cv_num_folds <- 5

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
  # good spatial clustering
  st_as_sf(coords = c("geo_longitude", "geo_latitude"), crs = 4326) %>%
  st_transform(3435) %>%
  mutate(
    geo_longitude = st_coordinates(.)[, 1],
    geo_latitude = st_coordinates(.)[, 2]
  ) %>%
  st_set_geometry(NULL) %>%
  arrange(meta_sale_date) 

# Create train/test split by time, with most recent observations in the test set
split_train_test <- initial_time_split(full_data, prop = 0.90)
test <- testing(split_train_test) 
train <- training(split_train_test)

# Secondary split into train vs train meta is to keep a holdout sample for the
# purpose of training a meta model (stacking)
split_train_meta <- initial_split(train, prop = 0.90)
train_meta <- testing(split_train_meta)
train <- training(split_train_meta)

# Create v-fold CV splits for the main training set
train_folds <- vfold_cv(train, v = cv_num_folds) 

# Setup training data recipe to remove unnecessary vars, log-log variables, etc
train_recipe <- mod_recp_prep(train, mod_predictors) 



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
##### Baseline Linear Model #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Setup basic linear model specification
lm_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

# Define basic linear model workflow
lm_wflow <- workflow() %>%
  add_model(lm_model) %>%
  add_recipe(train_recipe) 

# No CV for this linearmodel, just predict right away
lm_final_fit <-  fit(lm_wflow, data = train)

# Extract steps from the fit necessary to predict on the test set
lm_final_recp <- pull_workflow_prepped_recipe(lm_final_fit)
lm_final_fit <- pull_workflow_fit(lm_final_fit)

# Get predictions on the meta training set and test set
train_meta <- model_fit(train_meta, lm_final_recp, lm_final_fit, lm_sale_price)
test <- model_fit(test, lm_final_recp, lm_final_fit, lm_sale_price)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
##### Random Forest #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

### Step 1 - Model initialization

# Initialize random forest model
rf_model <- rand_forest(mtry = tune(), trees = 500, min_n = tune()) %>%
  set_mode("regression") %>%
  set_engine("ranger") 

# Initialize tuning parameters  
rf_params <- rf_model %>%
  parameters() %>%
  update(
    mtry = mtry(range = c(5, floor((ncol(juice(prep(train_recipe))) - 1) / 3))),
    min_n = min_n(range = c(5, 12))
  ) 

# Initialize RF workflow
rf_wflow <- workflow() %>%
  add_model(rf_model) %>%
  add_recipe(train_recipe) 


### Step 2 - Cross-validation

# Begin CV tuning if enabled
if (cv_enable) {
  
  # Initialize tuning grid
  rf_grid <- grid_regular(rf_params, levels = 3)
  
  # Loop through grid of tuning parameters
  tictoc::tic(msg = "RF CV model fitting complete!")
  rf_search <- tune_grid(
    object = rf_wflow, 
    resamples = train_folds,
    grid = rf_grid,
    metrics = metric_set(rmse, rsq, codm),
    control = control_grid(verbose = TRUE, allow_par = FALSE)
  )
  tictoc::toc()
  
  # Save CV results to dataframe
  rf_search %>%
    collect_metrics() %>%
    write_parquet("data/rf_results.parquet")
  
  # Choose the best model that minimizes COD
  rf_final_params <- select_best(rf_search, mtry, trees, metric = "codm")
  
} else {
  
  # If not running CV, set default rf params
  rf_final_params <- list(mtry = 9, min_n = 12)
  
}


### Step 3 - Finalize model and predict

# Fit the final model using the full training data
rf_wflow_final_fit <- rf_wflow %>%
  finalize_workflow(as.list(rf_final_params)) %>%
  fit(data = train)

# Pull recipe and final fit. Kinda buggy, see:
# https://hansjoerg.me/2020/02/09/tidymodels-for-machine-learning/
rf_final_recp <- pull_workflow_prepped_recipe(rf_wflow_final_fit)
rf_final_fit <- pull_workflow_fit(rf_wflow_final_fit)

# Get predictions from on the meta training set and test set using RF
train_meta <- model_fit(train_meta, rf_final_recp, rf_final_fit, rf_sale_price)
test <- model_fit(test, rf_final_recp, rf_final_fit, rf_sale_price)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
##### CkNN #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

### Step 0 - Initialize parallel backend for furrr functions used by cknn
if (cv_enable) {
  all_cores <- availableCores() - 1
  plan("multiprocess", workers = all_cores)
}


### Step 1 - Determine variable weights and which vars to keep

# Get the set of variables to keep for clustering from ccao::vars_dict
cknn_possible_vars <- ccao::vars_dict %>%
  filter(var_is_clustered) %>%
  pull(var_name_standard) %>%
  unique() %>%
  na.omit()

# Create feature weights using feature importance
cknn_var_weights <- c(
  "char_bldg_sf" = 11, "char_age" = 5, "char_bsmt" = 3,
  "char_gar1_size" = 3, "char_ext_wall" = 2.5, "char_air" = 2
)

# Get the set of variables to keep when clustering
cknn_noncluster_vars <- c("geo_longitude", "geo_latitude", "meta_sale_price")
cknn_predictors <- c(
  intersect(cknn_possible_vars, names(cknn_var_weights)),
  cknn_noncluster_vars
)

# Create a recipe using cknn predictors which removes unnecessary vars, 
# collapses rare factors, and converts NA in factors to "unknown"
cknn_recipe <- cknn_recp_prep(train, cknn_predictors)


### Step 2 - Cross-validation

# If cross validating, manually cycle through a grid of tuning paramerters to
# find the best ones
if (cv_enable) {
  
  # Create a grid of possible cknn parameter values to loop through
  cknn_param_grid <- expand_grid(
    m = seq(6, 14, 2),
    k = seq(10, 16, 2),
    l = seq(0.6, 0.9, 0.1)
  )
  
  # Create v folds in the training data and keep only clustering vars, then for
  # each CV fold, calculate the values for all hyperparameters in param_grid
  cknn_cv_fits <- train_folds %>%
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
        param_grid = cknn_param_grid, 
        noncluster_vars = cknn_noncluster_vars,
        weights = cknn_var_weights
      )
    )
  
  # Summarize results by parameter groups, averaging across folds
  cknn_results <- bind_rows(cknn_cv_fits$cknn_results) %>%
    group_by(m, k, l) %>%
    summarize(across(everything(), mean)) %>%
    ungroup() %>%
    arrange(cod) %>%
    write_parquet("data/cknn_results.parquet")
  
  # Take the best params according to lowest COD
  cknn_best_params <- cknn_results %>%
    filter(cod == min(cod, na.rm = TRUE))
  
} else {

  # If not running CV, set default cknn params
  cknn_best_params <- list(m = 8, k = 12, l = 0.8)
  
}


### Step 3 - Finalize model and predict

# Finalize best model
cknn_best_model <- cknn(
  data = juice(prep(cknn_recipe)) %>%
    select(-meta_sale_price, -geo_longitude, -geo_latitude) %>%
    as.data.frame(),
  lon = train %>% pull(geo_longitude),
  lat = train %>% pull(geo_latitude),
  m = cknn_best_params$m,
  k = cknn_best_params$k,
  l = cknn_best_params$l,
  keep_data = TRUE
)

# Get predictions from on the meta training set and test set using cknn
train_meta <- cknn_fit(
  train_meta, cknn_recipe, cknn_best_model,
  train$meta_sale_price, cknn_sale_price
)

test <- cknn_fit(
  test, cknn_recipe, cknn_best_model,
  train$meta_sale_price, cknn_sale_price
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
##### Meta Model (LM) #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Setup basic linear model specification for stacking
stack_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

# Setup model recipe to remove vars and log transform prices
stack_recipe <- stack_recp_prep(train_meta)

# Define basic linear model workflow
stack_wflow <- workflow() %>%
  add_model(stack_model) %>%
  add_recipe(stack_recipe) 

# No CV for this model
stack_final_fit <-  fit(stack_wflow, data = train_meta)

# Extract steps from the fit necessary to predict on the test set
stack_final_recp <- pull_workflow_prepped_recipe(stack_final_fit)
stack_final_fit <- pull_workflow_fit(stack_final_fit)

# Get predictions on the test set using the fully trained model
test <- model_fit(test, stack_final_recp, stack_final_fit, stack_sale_price)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
##### Finish Up #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Save test set results to file then generate report
test %>%
  write_parquet("data/test_set.parquet")



# TODO: Feature importance vars

# TODO: Save params for RF
# TODO: Add xgboost based model + lasso
# TODO: Caution on selection of time data for cknn

# TODO: figure out how to have a single model interface for training/prediction here

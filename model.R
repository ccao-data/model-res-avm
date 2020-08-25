# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
##### Setup ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Load modeling and data libraries
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
source("R/utils.R")

# Start full script timer
tictoc::tic(msg = "Full Modeling Complete!")

# Set seed for reproducibility 
set.seed(27)

# Toggle cross validation and set number of folds to use
cv_env_var <- as.logical(Sys.getenv("R_CV_ENABLE"))
cv_enable <- ifelse(!is.na(cv_env_var), cv_env_var, TRUE)
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
train_p <- ncol(juice(prep(train_recipe))) - 1

# Remove unnecessary data
rm(full_data, split_train_test, split_train_meta)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
##### ElasticNet Model #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

### Step 1 - Model initialization

# Set model params save path
enet_params_path <- "data/models/enet_results.parquet"

# Setup basic linear model specification
enet_model <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# Define basic linear model workflow
enet_wflow <- workflow() %>%
  add_model(enet_model) %>%
  add_recipe(train_recipe %>% dummy_recp_prep()) 


### Step 2 - Cross-validation

# Begin CV tuning if enabled
if (cv_enable) {
  
  # Create grid search space for ElasticNet params
  enet_grid <- grid_regular(penalty(), mixture(), levels = 40)
  
  # Loop through grid of tuning parameters
  tictoc::tic(msg = "ElasticNet CV model fitting complete!")
  enet_search <- tune_grid(
    object = enet_wflow, 
    resamples = train_folds,
    grid = enet_grid,
    metrics = metric_set(rmse, codm),
    control = control_grid(verbose = TRUE, allow_par = FALSE)
  )
  tictoc::toc(log = TRUE)
  beepr::beep(2)
  
  # Save CV results to data frame and file
  enet_search %>%
    collect_metrics() %>%
    write_parquet(enet_params_path)
  
  # Choose the best model that minimizes COD
  enet_final_params <- select_best(enet_search, metric = "codm")
  
} else {
  
  # Load best params from last file if they exists, otherwise use defaults
  if (file.exists(enet_params_path)) {
    enet_final_params <- model_get_stored_params(enet_params_path)
  } else {
    enet_final_params <- list(penalty = 0, mixture = 1)
  }
}


### Step 3 - Finalize model and predict

# Fit the final model using the full training data
enet_final_fit <-  enet_wflow %>%
  finalize_workflow(as.list(enet_final_params)) %>%
  fit(data = train)

# Extract steps from the fit necessary to predict on the test set
enet_final_recp <- pull_workflow_prepped_recipe(enet_final_fit)
enet_final_fit <- pull_workflow_fit(enet_final_fit)

# Get predictions on the training meta set and test set
train_meta <- model_fit(train_meta, enet_final_recp, enet_final_fit, enet_sale_price)
test <- model_fit(test, enet_final_recp, enet_final_fit, enet_sale_price)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
##### xgboost Model #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

### Step 1 - Model initialization

# Set model params save path
xgb_params_path <- "data/models/xgb_results.parquet"

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
  
  # Create grid search space for xgb
  xgb_grid <- grid_latin_hypercube(
    tree_depth(),
    min_n(),
    loss_reduction(),
    sample_size = sample_prop(),
    mtry = mtry(c(5, floor(train_p / 3))),
    learn_rate(),
    size = 20
  )
  
  # Loop through grid of tuning parameters
  tictoc::tic(msg = "XGB CV model fitting complete!")
  xgb_search <- tune_grid(
    object = xgb_wflow, 
    resamples = train_folds,
    grid = xgb_grid,
    metrics = metric_set(rmse, codm),
    control = control_grid(verbose = TRUE, allow_par = FALSE)
  )
  tictoc::toc(log = TRUE)
  beepr::beep(2)
  
  # Save CV results to data frame and file
  xgb_search %>%
    collect_metrics() %>%
    write_parquet(xgb_params_path)
  
  # Choose the best model that minimizes COD
  xgb_final_params <- select_best(xgb_search, metric = "codm")
  
} else {
  
  # Load best params from last file if they exists, otherwise use defaults
  if (file.exists(xgb_params_path)) {
    xgb_final_params <- model_get_stored_params(xgb_params_path)
  } else {
    xgb_final_params <- list(
      tree_depth = 9, min_n = 12, loss_reduction = 0.0000146,
      mtry = 10, sample_size = 0.557, learn_rate = 0.001
    )
  }
}


### Step 3 - Finalize model and predict

# Fit the final model using the full training data
xgb_wflow_final_fit <- xgb_wflow %>%
  finalize_workflow(as.list(xgb_final_params)) %>%
  fit(data = train)

# Pull recipe and final fit. Kinda buggy, see:
# https://hansjoerg.me/2020/02/09/tidymodels-for-machine-learning/
xgb_final_recp <- pull_workflow_prepped_recipe(xgb_wflow_final_fit)
xgb_final_fit <- pull_workflow_fit(xgb_wflow_final_fit)

# Get predictions from on the meta training set and test set using RF
train_meta <- model_fit(train_meta, xgb_final_recp, xgb_final_fit, xgb_sale_price)
test <- model_fit(test, xgb_final_recp, xgb_final_fit, xgb_sale_price)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
##### Random Forest Model #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

### Step 1 - Model initialization

# Model results file path
rf_params_path <- "data/models/rf_results.parquet"

# Initialize random forest model
rf_model <- rand_forest(mtry = tune(), trees = 500, min_n = tune()) %>%
  set_mode("regression") %>%
  set_engine("ranger") 

# Initialize RF workflow
rf_wflow <- workflow() %>%
  add_model(rf_model) %>%
  add_recipe(train_recipe) 


### Step 2 - Cross-validation

# Begin CV tuning if enabled
if (cv_enable) {
  
  # Initialize tuning grid
  rf_grid <- grid_latin_hypercube(
    mtry(range = c(5, floor(train_p / 3))),
    min_n(),
    size = 10
  )
  
  # Loop through grid of tuning parameters
  tictoc::tic(msg = "RF CV model fitting complete!")
  rf_search <- tune_grid(
    object = rf_wflow, 
    resamples = train_folds,
    grid = rf_grid,
    metrics = metric_set(rmse, codm),
    control = control_grid(verbose = TRUE, allow_par = FALSE)
  )
  tictoc::toc(log = TRUE)
  beepr::beep(2)
  
  # Save CV results to dataframe
  rf_search %>%
    collect_metrics() %>%
    write_parquet(rf_params_path)
  
  # Choose the best model that minimizes COD
  rf_final_params <- select_best(rf_search, mtry, trees, metric = "codm")
  
} else {
  
  # Load best params from last file if they exists, otherwise use defaults
  if (file.exists(rf_params_path)) {
    rf_final_params <- model_get_stored_params(rf_params_path)
  } else {
    rf_final_params <- list(mtry = 9, min_n = 12)
  }
  
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
##### CkNN Model #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

### Step 0 - Initialize parallel backend for furrr functions used by cknn
if (cv_enable) {
  all_cores <- availableCores() - 1
  plan(multiprocess, workers = all_cores)
}

# Setup model results path
cknn_params_path <- "data/models/cknn_results.parquet"


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
  cknn_grid <- expand_grid(
    m = seq(7, 15, 2),
    k = seq(7, 19, 3),
    l = seq(0.5, 0.9, 0.1)
  )
  
  # Create v folds in the training data and keep only clustering vars, then for
  # each CV fold, calculate the values for all hyperparameters in param_grid
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
    arrange(cod) %>%
    write_parquet(cknn_params_path)
  
  # Take the best params according to lowest COD
  cknn_final_params <- cknn_results %>%
    filter(cod == min(cod, na.rm = TRUE))
  
} else {
  
  # Load best params from last file if they exists, otherwise use defaults
  if (file.exists(cknn_params_path)) {
    cknn_final_params <- read_parquet(cknn_params_path) %>%
      filter(cod == min(cod)) %>%
      distinct(cod, .keep_all = TRUE)
  } else {
    cknn_final_params <- list(m = 8, k = 12, l = 0.8)
  }

}


### Step 3 - Finalize model and predict

# Finalize best model
cknn_final_fit <- cknn(
  data = juice(prep(cknn_recipe)) %>%
    select(-meta_sale_price, -geo_longitude, -geo_latitude) %>%
    as.data.frame(),
  lon = train %>% pull(geo_longitude),
  lat = train %>% pull(geo_latitude),
  m = cknn_final_params$m,
  k = cknn_final_params$k,
  l = cknn_final_params$l,
  keep_data = TRUE
)

# Get predictions from on the meta training set and test set using cknn
train_meta <- cknn_fit(
  train_meta, cknn_recipe, cknn_final_fit,
  train$meta_sale_price, cknn_sale_price
)

test <- cknn_fit(
  test, cknn_recipe, cknn_final_fit,
  train$meta_sale_price, cknn_sale_price
)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
##### Meta Model (Linear) #####
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
write_parquet(test, "data/testdata.parquet")

# Generate modeling report
rmarkdown::render(
  input = "R/report.Rmd",
  output_file = "report.html",
  output_dir = "data/models"
)

# Stop all timers and write CV timers to log file
tictoc::toc(log = TRUE)
if (cv_enable) bind_rows(tic.log(format = FALSE)) %>%
  mutate(elapsed = toc - tic, model = tolower(word(msg, 1))) %>%
  write_parquet("data/models/model_timings.parquet")

# BEEP!
beepr::beep(8)


# TODO: Set default params to best CV outcomes
# TODO: Create interaction terms: step_interact().
# TODO: Feature importance vars
# TODO: Caution on selection of time data for cknn
# TODO: figure out how to have a single model interface for training/prediction

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Setup #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Start the script timer and clear logs from prior script
tictoc::tic.clearlog()
tictoc::tic("Train")

# Load R libraries
options(tidymodels.dark = TRUE)
library(arrow)
library(assessr)
library(butcher)
library(ccao)
library(dplyr)
library(here)
library(lightgbm)
library(lightsnip)
library(purrr)
library(spatialsample)
library(stringr)
library(tictoc)
library(tidymodels)
library(vctrs)

# Load helpers and recipes from files
walk(list.files("R/", "\\.R$", full.names = TRUE), source)

# Initialize a dictionary of file paths and URIs. See R/helpers.R
paths <- model_file_dict()

# Get number of available physical cores to use for lightgbm multi-threading
# lightgbm docs recommend using only real cores, not logical
# https://lightgbm.readthedocs.io/en/latest/Parameters.html#num_threads
num_threads <- parallel::detectCores(logical = FALSE)

# Get train/test split proportion and model seed
model_seed <- as.integer(Sys.getenv("MODEL_SEED"), 27)
set.seed(model_seed)

# Retrieve hard-coded model hyperparameters from .Renviron
model_param_objective <- as.character(
  Sys.getenv("MODEL_PARAM_OBJECTIVE", "regression")
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
model_cv_split_prop <- as.numeric(Sys.getenv("MODEL_CV_SPLIT_PROP", 0.90))
model_cv_best_metric <- as.character(Sys.getenv("MODEL_CV_BEST_METRIC", "rmse"))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Prepare Data #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load the full set of training data, then arrange by sale date in order to
# facilitate out-of-time sampling/validation

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

# Get list only of categorical predictors to pass to lightgbm when training
model_predictors_categorical = ccao::vars_dict %>%
  dplyr::filter(
    var_is_predictor,
    var_data_type %in% c("categorical", "character"),
    var_name_model %in% names(training_data_full)
  ) %>%
  dplyr::pull(var_name_model) %>%
  unique() %>%
  na.omit()
  
# Create train/test split by time, with most recent observations in the test set
# We want our best model(s) to be predictive of the future, since properties are
# assessed on the basis of past sales
time_split <- initial_time_split(training_data_full, prop = model_cv_split_prop)
test <- testing(time_split)
train <- training(time_split)

# Create v-fold CV splits of the main training set
train_folds <- vfold_cv(data = train, v = model_cv_num_folds)

# Create a recipe for the training data which removes non-predictor columns,
# normalizes/logs data, and removes/imputes missing values
train_recipe <- model_main_recipe(
  data = train,
  keep_vars = model_predictors,
  id_vars = model_id_vars
)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### LightGBM Model #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# This is the main model used to value 200-class residential property. It uses
# lightgbm as a backend, which is a boosted tree model similar to xgboost or
# catboost, but with better performance and faster training time in our use case
# See https://lightgbm.readthedocs.io/ for more information


### Step 1 - Model initialization

# Initialize lightgbm model specification. Most hyperparameters are passed to
# lightgbm as "engine arguments" i.e. things specific to lightgbm
lgbm_model <- parsnip::boost_tree(
    trees = model_param_num_iterations,
    stop_iter = tune()
  ) %>%
  set_mode("regression") %>%
  set_engine(
    engine = "lightgbm",
    
    ##### Static Parameters #####
    
    # These are static lightgbm-specific engine parameters passed to lgb.train()
    # See lightsnip::train_lightgbm for details
    num_threads = num_threads,
    verbose = -1L,
    
    # Set the objective function. This is what lightgbm will try to minimize
    objective = model_param_objective,
    
    # Typically set statically along with the number of iterations (trees)
    learning_rate = model_param_learning_rate,
    
    # Names of integer-encoded categorical columns. This is CRITICAL else
    # lightgbm will treat these columns as numeric
    categorical_feature = model_predictors_categorical,

    # Enable early stopping using a proportion of each training sample as a
    # validation set. If lgb.train goes stop_iter() rounds without improvement
    # in the provided metric, then it will end training early
    validation = model_param_validation_prop,
    metric = model_param_validation_metric,
    
    # Lightsnip custom parameter. Links the value of max_depth to num_leaves
    # using floor(log2(num_leaves)) + add_to_linked_depth. Useful since
    # otherwise Bayesian opt spends time exploring irrelevant parameter space
    link_max_depth = model_param_link_max_depth,
    
    
    ##### Varying Parameters #####
    
    # These are parameters that are tuned using cross-validation. These are the
    # main parameters determining model complexity
    num_leaves = tune(),
    add_to_linked_depth = tune(),
    feature_fraction = tune(),
    min_gain_to_split = tune(),
    min_data_in_leaf = tune(),

    # Categorical-specific paramters
    max_cat_threshold = tune(),
    min_data_per_group = tune(),
    cat_smooth = tune(),
    cat_l2 = tune(),

    # Regularization parameters
    lambda_l1 = tune(),
    lambda_l2 = tune()
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

# Begin CV tuning if enabled. We use Bayesian tuning as due to the high number
# of hyperparameters, grid search or random search take a very long time to
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
      stop_iter           = dials::stop_iter(c(5L, 30L)),
      num_leaves          = lightsnip::num_leaves(c(500L, 5000L)),
      add_to_linked_depth = lightsnip::add_to_linked_depth(c(1, 3)),
      feature_fraction    = lightsnip::feature_fraction(),
      min_gain_to_split   = lightsnip::min_gain_to_split(c(-4, 1.5)),
      min_data_in_leaf    = lightsnip::min_data_in_leaf(c(2L, 200L)),
      max_cat_threshold   = lightsnip::max_cat_threshold(),
      min_data_per_group  = lightsnip::min_data_per_group(),
      cat_smooth          = lightsnip::cat_smooth(),
      cat_l2              = lightsnip::cat_l2(),
      lambda_l1           = lightsnip::lambda_l1(),
      lambda_l2           = lightsnip::lambda_l2()
    )

  # Use Bayesian tuning to find best performing params. This part takes quite
  # a long time, depending on the compute resources available
  lgbm_search <- tune_bayes(
    object = lgbm_wflow,
    resamples = train_folds,
    initial = model_cv_initial_set,
    iter = model_cv_max_iterations,
    param_info = lgbm_params,
    metrics = metric_set(rmse, mape, mae),
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
    lightsnip::axe_tune_data() %>%
    arrow::write_parquet(paths$output$parameter_search$local)
  
  # Save the possible parameter ranges used for tuning
  lgbm_params %>%
    mutate(range = purrr::map(object, dials::range_get)) %>%
    tidyr::unnest_wider(range) %>%
    select(
      parameter_name = name,
      parameter_type = component_id,
      lower, upper
    ) %>%
    arrow::write_parquet(paths$output$parameter_range$local)

  # Choose the best model (whichever model minimizes the chosen objective)
  lgbm_final_params <- lgbm_search %>%
    select_best(metric = model_param_objective) %>%
    arrow::write_parquet(paths$output$parameter_final$local)
  
  # End the cross-validation timer
  tictoc::toc(log = TRUE)
} else {

  # If CV is not enabled, load saved parameters from file if it exists
  # Otherwise use a set of hand-chosen parameters
  if (file.exists(paths$output$parameter_search$local)) {
    lgbm_final_params <- read_parquet(paths$output$parameter_search$local) %>%
      select_best(metric = model_param_objective) %>%
      arrow::write_parquet(paths$output$parameter_final$local)
  } else {
    lgbm_final_params <- data.frame(
      stop_iter = 18L, num_leaves = 2670L, add_to_linked_depth = 3L,
      feature_fraction = 0.9, min_gain_to_split = 0.0, min_data_in_leaf = 64L,
      max_cat_threshold = 72L, min_data_per_group = 66L,
      cat_smooth = 99.0, cat_l2 = 0.001, lambda_l1 = 0.005,
      lambda_l2 = 1.837, .config = "Default"
    ) %>%
    arrow::write_parquet(paths$output$parameter_final$local)
  }
}


### Step 3 - Fit models

# Fit the final model using the training data and our final hyperparameters
# This is the model used to measure performance on the test set
lgbm_wflow_final_fit <- lgbm_wflow %>%
  finalize_workflow(lgbm_final_params) %>%
  fit(data = train)

# Fit the final model using the full data (including the test set) and our final
# hyperparameters. This is the model used for actually assessing all properties
lgbm_wflow_final_full_fit <- lgbm_wflow %>%
  finalize_workflow(lgbm_final_params) %>%
  fit(data = training_data_full)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Finalize Models #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get predictions on the test set using the training data model then save to
# file. These predictions are used to evaluate model performance on the test set 
test %>%
  mutate(lgbm = predict(lgbm_wflow_final_fit, test)$.pred) %>%
  write_parquet(paths$output$data$test$local)

# Save the finalized model object to file so it can be used elsewhere. Note the
# model_lgbm_save() function, which uses lgb.save() rather than saveRDS(), since
# lightgbm is picky about how its model objects are stored on disk
lgbm_wflow_final_full_fit %>%
  workflows::extract_fit_parsnip() %>%
  lightsnip::lgbm_save(paths$output$workflow$fit$local)

# Save the finalized recipe object to file so it can be used to preprocess
# new data
lgbm_wflow_final_full_fit %>%
  workflows::extract_recipe() %>%
  lightsnip::axe_recipe() %>%
  saveRDS(paths$output$workflow$recipe$local)

# End the script timer and write the time elapsed to file
tictoc::toc(log = TRUE)
arrow::read_parquet(paths$output$timing$local) %>%
  bind_rows(., tictoc::tic.log(format = FALSE)) %>%
    arrow::write_parquet(paths$output$timing$local)

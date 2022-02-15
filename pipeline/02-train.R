#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Setup ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Start the stage timer and clear logs from prior stage
tictoc::tic.clearlog()
tictoc::tic("Train")

# Load libraries and scripts
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
library(stringr)
library(tictoc)
library(tidymodels)
library(vctrs)

# Load helpers and recipes from files
walk(list.files("R/", "\\.R$", full.names = TRUE), source)

# Initialize a dictionary of file paths and S3 URIs. See R/file_dict.csv
paths <- model_file_dict()

# Load the metadata file containing the run settings
metadata <- read_parquet(paths$output$metadata$local)

# Set the overall run/stage seed
set.seed(metadata$model_seed)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. Prepare Data --------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load the full set of training data, then arrange by sale date in order to
# facilitate out-of-time sampling/validation

# NOTE: It is critical to trim "multicard" sales when training. Multicard means
# there is multiple buildings on a PIN. Sales for multicard PINs are
# often for multiple buildings and will therefore bias the model training
training_data_full <- read_parquet(paths$input$training$local) %>%
  filter(!is.na(loc_longitude), !is.na(loc_latitude), !ind_pin_is_multicard) %>%
  arrange(meta_sale_date)

# Create train/test split by time, with most recent observations in the test set
# We want our best model(s) to be predictive of the future, since properties are
# assessed on the basis of past sales
time_split <- initial_time_split(
  data = training_data_full,
  prop = metadata$model_cv_split_prop
)
test <- testing(time_split)
train <- training(time_split)

# Create v-fold CV splits of the main training set
train_folds <- vfold_cv(data = train, v = metadata$model_cv_num_folds)

# Create a recipe for the training data which removes non-predictor columns and
# preps categorical data
train_recipe <- model_main_recipe(
  data = train,
  keep_vars = metadata$model_predictor_all_name[[1]],
  id_vars = metadata$model_identifier_name[[1]]
)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. LightGBM Model ------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# This is the main model used to value 200-class residential property. It uses
# lightgbm as a backend, which is a boosted tree model similar to xgboost or
# catboost, but with better performance and faster training time in our use case
# See https://lightgbm.readthedocs.io/ for more information


## 3.1. Default Parameters -----------------------------------------------------

# Load the default set of parameters created in the setup stage. These are
# pulled from the included .Renviron file
lgbm_default_params <- read_parquet(paths$intermediate$parameter_default$local)


## 3.2. Model Initialization ---------------------------------------------------

# Initialize a lightgbm model specification. Most hyperparameters are passed to
# lightgbm as "engine arguments" i.e. things specific to lightgbm, as opposed to
# model arguments, which are provided by parsnip's boost_tree()
lgbm_model <- parsnip::boost_tree(
  trees = lgbm_default_params$num_iterations,
  stop_iter = tune()
) %>%
  set_mode("regression") %>%
  set_engine(
    engine = "lightgbm",
    seed = metadata$model_seed,

    ### 3.2.1. Manual Parameters -----------------------------------------------

    # These are static lightgbm-specific engine parameters passed to lgb.train()
    # See lightsnip::train_lightgbm for details
    num_threads = metadata$model_num_threads,
    verbose = -1L,

    # Set the objective function. This is what lightgbm will try to minimize
    objective = lgbm_default_params$objective_func,

    # Typically set manually along with the number of iterations (trees)
    learning_rate = lgbm_default_params$learning_rate,

    # Names of integer-encoded categorical columns. This is CRITICAL else
    # lightgbm will treat these columns as numeric
    categorical_feature = metadata$model_predictor_categorical_name[[1]],

    # Enable early stopping using a proportion of each training sample as a
    # validation set. If lgb.train goes stop_iter() rounds without improvement
    # in the provided metric, then it will end training early. This saves an
    # immense amount of time during CV
    validation = lgbm_default_params$validation_prop,
    metric = lgbm_default_params$validation_metric,

    # Lightsnip custom parameter. Links the value of max_depth to num_leaves
    # using floor(log2(num_leaves)) + add_to_linked_depth. Useful since
    # otherwise Bayesian opt spends time exploring irrelevant parameter space
    link_max_depth = lgbm_default_params$link_max_depth,


    ### 3.2.2. Tuned Parameters ------------------------------------------------

    # These are parameters that are tuned using cross-validation. These are the
    # main parameters determining model complexity
    num_leaves = tune(),
    add_to_linked_depth = tune(),
    feature_fraction = tune(),
    min_gain_to_split = tune(),
    min_data_in_leaf = tune(),

    # Categorical-specific parameters
    max_cat_threshold = tune(),
    min_data_per_group = tune(),
    cat_smooth = tune(),
    cat_l2 = tune(),

    # Regularization parameters
    lambda_l1 = tune(),
    lambda_l2 = tune()
  )

# Initialize lightgbm workflow, which contains both the model spec AND the
# pre-processing steps/recipe needed to prepare the raw data
lgbm_wflow <- workflow() %>%
  add_model(lgbm_model) %>%
  add_recipe(
    recipe = train_recipe,
    blueprint = hardhat::default_recipe_blueprint(allow_novel_levels = TRUE)
  )


## 3.3. Cross-Validation -------------------------------------------------------

# Begin CV tuning if enabled. We use Bayesian tuning, as due to the high number
# of hyperparameters, grid search or random search take a very long time to
# produce similarly accurate results
if (metadata$model_cv_enable) {

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

  # Use Bayesian tuning to find best performing hyperparameters. This part takes
  # quite a long time, depending on the compute resources available
  lgbm_search <- tune_bayes(
    object = lgbm_wflow,
    resamples = train_folds,
    initial = metadata$model_cv_initial_set,
    iter = metadata$model_cv_max_iterations,
    param_info = lgbm_params,
    metrics = metric_set(rmse, mape, mae),
    control = control_bayes(
      verbose = TRUE,
      uncertain = metadata$model_cv_no_improve - 2,
      no_improve = metadata$model_cv_no_improve,
      seed = metadata$model_seed
    )
  )

  # Save tuning results to file. This is a data frame where each row is one
  # CV iteration
  lgbm_search %>%
    lightsnip::axe_tune_data() %>%
    arrow::write_parquet(paths$output$parameter_raw$local)

  # Save the possible parameter ranges searched for tuning
  lgbm_params %>%
    mutate(range = purrr::map(object, dials::range_get)) %>%
    tidyr::unnest_wider(range) %>%
    select(
      parameter_name = name, parameter_type = component_id,
      lower, upper
    ) %>%
    mutate(run_id = metadata$run_id) %>%
    relocate(run_id, .before = everything()) %>%
    arrow::write_parquet(paths$output$parameter_range$local)

  # Choose the best model (whichever model minimizes the chosen objective,
  # averaged across CV folds), then overwrite any default parameters with
  # the ones found by CV
  lgbm_final_params <- lgbm_search %>%
    select_best(metric = metadata$model_cv_best_metric) %>%
    bind_cols(select(lgbm_default_params, run_id:link_max_depth), .) %>%
    # Max_depth is set by lightsnip if link_max_depth is true, so we need to
    # back out its value. Otherwise, use whichever value is chosen by CV
    mutate(max_depth = {
      if (link_max_depth) {
        as.integer(floor(log2(num_leaves)) + add_to_linked_depth)
      } else if (!is.null(.$max_depth)) {
        .$max_depth
      } else {
        NULL
      }
    }) %>%
    # Keep only the parameters included in the model specification, all others
    # should be dropped since they didn't impact the model
    select(
      run_id, configuration = .config, objective_func:link_max_depth,
      any_of(c("max_depth", names(lgbm_model$args), names(lgbm_model$eng_args)))
    ) %>%
    arrow::write_parquet(paths$output$parameter_final$local)
  
} else {

  # If CV is not enabled, just use the default set of parameters gathered in the
  # setup stage, keeping only the ones named in the model specification
  lgbm_final_params <- lgbm_default_params %>%
    select(
      run_id, configuration = .config, objective_func:link_max_depth,
      any_of(c("max_depth", names(lgbm_model$args), names(lgbm_model$eng_args)))
    ) %>%
    arrow::write_parquet(paths$output$parameter_final$local)
}


## 3.4. Fit Models -------------------------------------------------------------

# NOTE: The model specifications here use early stopping by measuring the change
# in the objective function on the training set (rather than the 10% sample
# validation set used during CV). This is so we can use the full data for
# training but still benefit from early stopping

# Fit the final model using the training data and our final hyperparameters
# This model is used to measure performance on the test set
lgbm_wflow_final_fit <- lgbm_wflow %>%
  update_model(lgbm_model %>% set_args(validation = 0)) %>%
  finalize_workflow(lgbm_final_params) %>%
  fit(data = train)

# Fit the final model using the full data (including the test set) and our final
# hyperparameters. This model is used for actually assessing all properties
lgbm_wflow_final_full_fit <- lgbm_wflow %>%
  update_model(lgbm_model %>% set_args(validation = 0)) %>%
  finalize_workflow(lgbm_final_params) %>%
  fit(data = training_data_full)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4. Finalize Models -----------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get predictions on the test set using the training data model then save to
# file. These predictions are used to evaluate model performance on the test set
test %>%
  mutate(pred_card_initial_fmv = predict(lgbm_wflow_final_fit, test)$.pred) %>%
  write_parquet(paths$intermediate$test$local)

# Save the finalized model object to file so it can be used elsewhere. Note the
# lgbm_save() function, which uses lgb.save() rather than saveRDS(), since
# lightgbm is picky about how its model objects are stored on disk
lgbm_wflow_final_full_fit %>%
  workflows::extract_fit_parsnip() %>%
  lightsnip::lgbm_save(paths$output$workflow_fit$local)

# Save the finalized recipe object to file so it can be used to preprocess
# new data. This is critical since it saves the factor levels used to integer-
# encode any categorical columns
lgbm_wflow_final_full_fit %>%
  workflows::extract_recipe() %>%
  lightsnip::axe_recipe() %>%
  saveRDS(paths$output$workflow_recipe$local)

# End the stage timer and append the time elapsed to a temporary file
tictoc::toc(log = TRUE)
arrow::read_parquet(paths$intermediate$timing$local) %>%
  bind_rows(., tictoc::tic.log(format = FALSE)) %>%
  arrow::write_parquet(paths$intermediate$timing$local)

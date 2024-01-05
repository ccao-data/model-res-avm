#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Setup ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# NOTE: See DESCRIPTION for library dependencies and R/setup.R for
# variables used in each pipeline stage

# Start the stage timer and clear logs from prior stage
tictoc::tic.clearlog()
tictoc::tic("Train")

# Load libraries, helpers, and recipes from files
purrr::walk(list.files("R/", "\\.R$", full.names = TRUE), source)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. Prepare Data --------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Preparing model training data")

# Load the full set of training data, then arrange by sale date in order to
# facilitate out-of-time sampling/validation

# NOTE: It is critical to trim "multicard" sales when training. Multicard means
# there is multiple buildings on a PIN. Since these sales include multiple
# buildings, they are typically higher than a "normal" sale and must be removed
training_data_full <- read_parquet(paths$input$training$local) %>%
  filter(!ind_pin_is_multicard, !sv_is_outlier) %>%
  arrange(meta_sale_date)

# Create train/test split by time, with most recent observations in the test set
# We want our best model(s) to be predictive of the future, since properties are
# assessed on the basis of past sales
split_data <- initial_time_split(
  data = training_data_full,
  prop = params$cv$split_prop
)
test <- testing(split_data)
train <- training(split_data)

# Create a recipe for the training data which removes non-predictor columns and
# preps categorical data, see R/recipes.R for details
train_recipe <- model_main_recipe(
  data = training_data_full %>% select(-any_of("time_split")),
  pred_vars = params$model$predictor$all,
  cat_vars = params$model$predictor$categorical,
  id_vars = params$model$predictor$id
)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. LightGBM Model ------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Initializing LightGBM model")

# This is the main model used to value 200-class residential property. It uses
# lightgbm as a backend, which is a boosted tree model similar to xgboost or
# catboost, but with better performance and faster training time in our use case
# See https://lightgbm.readthedocs.io/ for more information


## 3.1. Model Initialization ---------------------------------------------------

# Initialize a lightgbm model specification. Most hyperparameters are passed to
# lightgbm as "engine arguments" i.e. things specific to lightgbm, as opposed to
# model arguments, which are provided by parsnip's boost_tree()
lgbm_model <- parsnip::boost_tree(
  stop_iter = params$model$parameter$stop_iter,
  trees = params$model$hyperparameter$default$num_iterations
) %>%
  set_mode("regression") %>%
  set_engine(
    engine = params$model$engine,

    # Parameters required to make the model deterministic i.e. output the same
    # predictions if the same hyperparameters are used
    seed = params$model$seed,
    deterministic = params$model$deterministic,
    force_row_wise = params$model$force_row_wise,


    ### 3.1.1. Manual Parameters -----------------------------------------------

    # These are static lightgbm-specific engine parameters passed to lgb.train()
    # See lightsnip::train_lightgbm for details
    num_threads = num_threads,
    verbose = params$model$verbose,

    # Set the objective function. This is what lightgbm will try to minimize
    objective = params$model$objective,

    # Names of integer-encoded categorical columns. This is CRITICAL or else
    # lightgbm will treat these columns as numeric
    categorical_feature = params$model$predictor$categorical,

    # Enable early stopping using a proportion of each training sample as a
    # validation set. If lgb.train goes `stop_iter` rounds without improvement
    # in the chosen metric, then it will end training early. Saves an immense
    # amount of time during CV. WARNING: See GitLab issue #82 for more info
    validation = params$model$parameter$validation_prop,
    sample_type = params$model$parameter$validation_type,
    metric = params$model$parameter$validation_metric,

    # Lightsnip custom parameter. Links the value of max_depth to num_leaves
    # using floor(log2(num_leaves)) + add_to_linked_depth. Useful since
    # otherwise Bayesian opt spends time exploring irrelevant parameter space
    link_max_depth = params$model$parameter$link_max_depth,

    # Initialize the validation set to an empty list. Not actually used for
    # validation; If `validation > 0`, this list will get overridden by a
    # randomly sampled validation set. Instead, it is used during comps
    # calculation to instruct lightgbm to save error metrics for every tree
    # in the model, which allows us to weight the importance of leaf node
    # assignments for each tree. We initialize to an empty list here, since
    # parsnip requires all attributes be initialized during the first
    # set_engine call if they are to be updated later
    valids = list(),

    ### 3.1.2. Tuned Parameters ------------------------------------------------

    # Typically set manually along with the number of iterations (trees)
    learning_rate = tune(),

    # Max number of bins that feature values will be bucketed in
    max_bin = tune(),

    # Main parameters determining model complexity
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


## 3.2. Cross-Validation -------------------------------------------------------

# Begin CV tuning if enabled. We use Bayesian tuning as grid search or random
# search take a very long time to produce good results due to the high number
# of hyperparameters
if (cv_enable) {
  message("Starting cross-validation")

  # Create the cross-validation folds
  train_folds <- vfold_cv(
    data = train,
    v = params$cv$num_folds
  )

  # Create the parameter search space for hyperparameter optimization
  # Parameter boundaries are taken from the lightgbm docs and hand-tuned
  # See: https://lightgbm.readthedocs.io/en/latest/Parameters-Tuning.html
  lgbm_range <- params$model$hyperparameter$range
  lgbm_params <- lgbm_wflow %>%
    hardhat::extract_parameter_set_dials() %>%
    update(
      # nolint start
      learning_rate       = lightsnip::learning_rate(lgbm_range$learning_rate),
      max_bin             = lightsnip::max_bin(lgbm_range$max_bin),
      num_leaves          = lightsnip::num_leaves(lgbm_range$num_leaves),
      add_to_linked_depth = lightsnip::add_to_linked_depth(lgbm_range$add_to_linked_depth),
      feature_fraction    = lightsnip::feature_fraction(lgbm_range$feature_fraction),
      min_gain_to_split   = lightsnip::min_gain_to_split(lgbm_range$min_gain_to_split),
      min_data_in_leaf    = lightsnip::min_data_in_leaf(lgbm_range$min_data_in_leaf),
      max_cat_threshold   = lightsnip::max_cat_threshold(lgbm_range$max_cat_threshold),
      min_data_per_group  = lightsnip::min_data_per_group(lgbm_range$min_data_per_group),
      cat_smooth          = lightsnip::cat_smooth(lgbm_range$cat_smooth),
      cat_l2              = lightsnip::cat_l2(lgbm_range$cat_l2),
      lambda_l1           = lightsnip::lambda_l1(lgbm_range$lambda_l1),
      lambda_l2           = lightsnip::lambda_l2(lgbm_range$lambda_l2)
      # nolint end
    )

  # Use Bayesian tuning to find best performing hyperparameters. This part takes
  # quite a long time, depending on the compute resources available
  lgbm_search <- tune_bayes(
    object = lgbm_wflow,
    resamples = train_folds,
    initial = params$cv$initial_set,
    iter = params$cv$max_iterations,
    param_info = lgbm_params,
    metrics = metric_set(rmse, mape, mae),
    control = control_bayes(
      verbose = TRUE,
      verbose_iter = TRUE,
      uncertain = params$cv$uncertain,
      no_improve = params$cv$no_improve,
      extract = extract_num_iterations,
      seed = params$model$seed
    )
  )

  # Save tuning results to file. This is a data.frame where each row is one
  # CV iteration
  lgbm_search %>%
    lightsnip::axe_tune_data() %>%
    arrow::write_parquet(paths$output$parameter_raw$local)

  # Save the parameter ranges searched while tuning
  lgbm_params %>%
    mutate(range = purrr::map(object, dials::range_get)) %>%
    tidyr::unnest_wider(range) %>%
    select(
      parameter_name = name, parameter_type = component_id,
      lower, upper
    ) %>%
    arrow::write_parquet(paths$output$parameter_range$local)

  # Choose the best model (whichever model minimizes the chosen objective,
  # averaged across CV folds)
  lgbm_final_params <- tibble(
    engine = params$model$engine,
    seed = params$model$seed,
    objective = params$model$objective
  ) %>%
    bind_cols(
      as_tibble(params$model$parameter) %>%
        select(-any_of("num_iterations"))
    ) %>%
    bind_cols(
      select_iterations(lgbm_search, metric = params$cv$best_metric)
    ) %>%
    bind_cols(
      select_best(lgbm_search, metric = params$cv$best_metric) %>%
        select(-any_of("trees"))
    ) %>%
    select(configuration = .config, everything()) %>%
    arrow::write_parquet(paths$output$parameter_final$local)
} else {
  # If CV is disabled, just use the default set of parameters specified in
  # params.yaml, keeping only the ones used in the model specification
  lgbm_missing_params <- names(params$model$hyperparameter$default)
  lgbm_missing_params <- lgbm_missing_params[
    !lgbm_missing_params %in%
      c(hardhat::extract_parameter_set_dials(lgbm_wflow)$name, "num_iterations")
  ]
  lgbm_final_params <- tibble(
    configuration = "Default",
    engine = params$model$engine,
    seed = params$model$seed,
    objective = params$model$objective
  ) %>%
    bind_cols(as_tibble(params$model$parameter)) %>%
    bind_cols(as_tibble(params$model$hyperparameter$default)) %>%
    select(-all_of(lgbm_missing_params)) %>%
    arrow::write_parquet(paths$output$parameter_final$local)

  # If CV is disabled, we still need to write empty stub files for any outputs
  # created by CV so DVC has something to hash/look for
  arrow::write_parquet(data.frame(), paths$output$parameter_raw$local)
  arrow::write_parquet(data.frame(), paths$output$parameter_range$local)
}


## 3.3. Fit Models -------------------------------------------------------------

# Finalize the model specification by disabling early stopping, instead using
# the maximum number of iterations used during the best cross-validation round
# OR the default `num_iterations` if CV was not performed
lgbm_model_final <- lgbm_model %>%
  set_args(
    stop_iter = NULL,
    validation = 0,
    trees = lgbm_final_params$num_iterations
  )

# Fit the final model using the training data and our final hyperparameters
# This model is used to measure performance on the test set
message("Fitting final model on training data")
lgbm_wflow_final_fit <- lgbm_wflow %>%
  update_model(lgbm_model_final) %>%
  finalize_workflow(lgbm_final_params) %>%
  fit(data = train)

if (comp_enable) {
  # Instruct lightgbm to record errors for each tree to the model$record_evals
  # attribute by setting the `valids` engine argument. Use the full set of
  # training data so that the error encapsulates all the data
  valids <- list(valids = lightgbm::lgb.Dataset(
    data = as.matrix(training_data_full),
    label = training_data_full[["meta_sale_price"]]
  ))
  lgbm_model_final_full_fit <- lgbm_model_final %>%
    set_args(valids = valids)
} else {
  lgbm_model_final_full_fit <- lgbm_model_final
}

# Fit the final model using the full data (including the test set) and our final
# hyperparameters. This model is used for actually assessing all properties
message("Fitting final model on full data")
lgbm_wflow_final_full_fit <- lgbm_wflow %>%
  update_model(lgbm_model_final_full_fit) %>%
  finalize_workflow(lgbm_final_params) %>%
  fit(data = training_data_full)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4. Finalize Models -----------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Finalizing and saving trained model")

# Get predictions on the test set using the training data model. These
# predictions are used to evaluate model performance on the unseen test set.
# Keep only the variables necessary for evaluation
test %>%
  mutate(pred_card_initial_fmv = predict(lgbm_wflow_final_fit, test)$.pred) %>%
  select(
    meta_year, meta_pin, meta_class, meta_card_num, meta_triad_code,
    all_of(params$ratio_study$geographies), char_bldg_sf,
    all_of(c(
      "prior_far_tot" = params$ratio_study$far_column,
      "prior_near_tot" = params$ratio_study$near_column
    )),
    pred_card_initial_fmv,
    meta_sale_price, meta_sale_date, meta_sale_document_num
  ) %>%
  # Prior year values are AV, not FMV. Multiply by 10 to get FMV for residential
  mutate(
    prior_far_tot = prior_far_tot * 10,
    prior_near_tot = prior_near_tot * 10
  ) %>%
  as_tibble() %>%
  write_parquet(paths$output$test_card$local)

# Save the finalized model object to file so it can be used elsewhere. Note the
# lgbm_save() function, which uses lgb.save() rather than saveRDS(), since
# lightgbm is picky about how its model objects are stored on disk
lgbm_wflow_final_full_fit %>%
  workflows::extract_fit_parsnip() %>%
  lightsnip::lgbm_save(paths$output$workflow_fit$local)

lgbm_final_full_fit <- lgbm_wflow_final_full_fit %>%
  workflows::extract_fit_parsnip()

lgbm_final_full_recipe <- lgbm_wflow_final_full_fit %>%
  workflows::extract_recipe() %>%
  lightsnip::axe_recipe()

message("Model specs:")
print(lgbm_final_full_fit)

test_preds_data_prepped <- recipes::bake(
  object = lgbm_final_full_recipe,
  new_data = head(training_data_full, 10),
  all_predictors()
)
sample_preds <- predict(
  object = lgbm_final_full_fit$fit,
  newdata = as.matrix(test_preds_data_prepped),
  type = "leaf",
)
message("Sample predictions shape:")
print(dim(sample_preds))

message("Sample predictions content:")
print(sample_preds %>% as_tibble())

message("Checking record_evals")
if (is.null(lgbm_final_full_fit$fit$record_evals)) {
  stop("Trained model is missing required record_evals")
}
trained_record_evals_len <- length(lgbm_final_full_fit$fit$record_evals$valids$rmse$eval)
message(glue::glue("Trained record_evals length: {trained_record_evals_len}"))
message("Trained record_evals preview:")
lgbm_final_full_fit$fit$record_evals$valids$rmse$eval %>%
  head(10) %>%
  print()

loaded_fit <- lightsnip::lgbm_load(paths$output$workflow_fit$local)

print("Saved model specs:")
print(loaded_fit)

loaded_sample_preds <- predict(
  object = loaded_fit$fit,
  newdata = as.matrix(test_preds_data_prepped),
  type = "leaf",
)
message("Loaded sample predictions shape:")
print(dim(loaded_sample_preds))

if (is.null(loaded_fit$fit$record_evals)) {
  stop("Saved model is missing required record_evals")
}
saved_record_evals_len <- length(loaded_fit$fit$record_evals$valids$rmse$eval)
message(glue::glue("Saved record_evals length: {trained_record_evals_len}"))

# Save the finalized recipe object to file so it can be used to preprocess
# new data. This is critical since it saves the factor levels used to integer-
# encode any categorical columns
lgbm_wflow_final_full_fit %>%
  workflows::extract_recipe() %>%
  lightsnip::axe_recipe() %>%
  saveRDS(paths$output$workflow_recipe$local)

# End the stage timer and write the time elapsed to a temporary file
tictoc::toc(log = TRUE)
bind_rows(tictoc::tic.log(format = FALSE)) %>%
  arrow::write_parquet(gsub("//*", "/", file.path(
    paths$intermediate$timing$local,
    "model_timing_train.parquet"
  )))

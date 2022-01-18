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
library(stringr)
library(tictoc)
library(tidymodels)
library(treesnip)
library(vctrs)

# Load helper recipes and lightgbm parsnip bindings from file
source(here("R", "recipes.R"))
source(here("R", "bindings.R"))
source(here("R", "helpers.R"))

# Get number of available physical cores to use for lightgbm multithreading
# lightgbm docs recommend using only real cores, not logical
num_threads <- parallel::detectCores(logical = FALSE)

# Start full script timer
tictoc::tic(msg = "Full Modeling Complete!")

# Toggle cross validation and set number of folds to use
cv_enable <- as.logical(Sys.getenv("MODEL_CV_ENABLE", FALSE))
cv_num_folds <- as.numeric(Sys.getenv("MODEL_CV_NUM_FOLDS", 7))
cv_control <- control_bayes(verbose = TRUE, no_improve = 20, seed = 27)

# Get train/test proportion
model_split_prop <- as.numeric(Sys.getenv("MODEL_SPLIT_PROP", 0.90))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Prepare Data #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Create list of variables that uniquely identify each structure or sale, these
# can be kept in the training data even though they are not regressors
mod_id_vars <- c(
  "meta_pin", "meta_class", "meta_card_num", "meta_sale_document_num"
)

# Get the full list of right-hand side predictors from ccao::vars_dict. To
# manually add new features, append the name of the feature as it is stored in
# training_data to this vector
mod_predictors <- ccao::vars_dict %>%
  filter(var_is_predictor) %>%
  pull(var_name_model) %>%
  unique() %>%
  na.omit()

# Load the full set of training data, keep only good, complete observations
# Arrange by sale date in order to facilitate out-of-time sampling/validation
training_data_full <- read_parquet(here("input", "training_data.parquet")) %>%
  arrange(meta_sale_date)
  
# Create train/test split by time, with most recent observations in the test set
# We want our best model(s) to be predictive of the future, since properties are
# assessed on the basis of past sales
time_split <- initial_time_split(training_data_full, prop = model_split_prop)
test <- testing(time_split)
train <- training(time_split)

# Create v-fold CV splits of the main training set
train_folds <- vfold_cv(train, v = cv_num_folds)

# Create a recipe for the training data which removes non-predictor columns,
# normalizes/logs data, and removes/imputes missing values
train_recipe <- mod_recp_prep(
  data = train,
  keep_vars = mod_predictors,
  id_vars = mod_id_vars
)

# Extract number of columns actually used in model (P). This is needed to
# properly set certain tuning parameters
juiced_train <- juice(prep(train_recipe), all_predictors())
train_p <- ncol(juiced_train)

# Remove unnecessary data from setup
rm(time_split, juiced_train)
gc()




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### ElasticNet Model #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 
# # Simple linear model to use as a baseline for comparison to lightgbm
# # Uses glmnet since it seems to be a bit faster than lm() in this case
# 
# # NOTE: Although hyperparameters for elasticnet are used, they have very little
# # effect on performance with this dataset unless set to extreme values, so we
# # don't bother tuning them here. Again, just a baseline
# 
# 
# ### Step 1 - Model initialization
# 
# # Setup basic elasticnet model specification with hyperparameters
# enet_model <- linear_reg(penalty = 1e-7, mixture = 0.15) %>%
#   set_engine("glmnet") %>%
#   set_mode("regression")
# 
# # Define basic elasticnet model workflow, which includes the model and
# # preprocessing steps. NOTE: While enet uses the same preprocessing recipe as
# # lightgbm, there is an additional step/recipe here which one-hot encodes all
# # categorical variables. This is necessary since glmnet does not natively handle
# # factor variables like lightgbm does
# enet_wflow <- workflow() %>%
#   add_model(enet_model) %>%
#   add_recipe(train_recipe %>% dummy_recp_prep())
# 
# 
# ### Step 2 - Fit the model
# 
# # Fit the final model using the training data, this will be used on the test set
# # and compared to the performance of lightgbm
# enet_wflow_final_fit <- enet_wflow %>%
#   fit(data = train)
# 
# # Remove unnecessary objects created while modeling. This is to save memory
# ccao::rm_intermediate("enet")




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### LightGBM Model #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# This is the main model used to value 200-class residential property. It uses
# lightgbm as a backend, which is a boosted tree model similar to xgboost or
# catboost, but with better performance and faster training in our use case
# See https://lightgbm.readthedocs.io/ for more information


### Step 1 - Model initialization

# Set model params save path. This is where the final set of hyperparameters
# will be saved/loaded from
lgbm_params_path <- here("output", "params", "lgbm_params.rds")

# Initialize lightgbm model specification Note that categorical columns are
# detected automatically by treesnip's lightgbm implementation as long as they
# are factors. trees argument here maps to num_iterations in lightgbm
lgbm_model <- lgbm_tree(
  trees = 2000,
  num_leaves = tune(), tree_depth = tune(), min_n = tune(),
  mtry = tune(), loss_reduction = tune(), learn_rate = tune()
) %>%
  set_engine("lightgbm") %>%
  set_mode("regression") %>%
  set_args(
    num_threads = num_threads,
    verbose = 1
  )

# Initialize lightgbm workflow, which contains both the model spec AND the
# preprocessing steps needed to prepare the raw data
lgbm_wflow <- workflow() %>%
  add_model(lgbm_model) %>%
  add_recipe(train_recipe)


### Step 2 - Cross-validation

# Begin CV tuning if enabled. We use Bayesian tuning due to the high number of
# hyperparameters, as grid search or random search take a very long time to
# produce similarly accurate results
if (cv_enable) {

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
  tictoc::tic(msg = "LightGBM CV model fitting complete!")
  lgbm_search <- tune_bayes(
    object = lgbm_wflow,
    resamples = train_folds,
    initial = 12,
    iter = 100,
    param_info = lgbm_params,
    metrics = metric_set(rmse, mae, mape),
    control = cv_control
  )
  tictoc::toc(log = TRUE)
  beepr::beep(2)

  # Save tuning results to file. This is a data frame where each row is one
  # CV iteration
  if (cv_write_params) {
    lgbm_search %>%
      ccao::model_axe_tune_data() %>%
      saveRDS(lgbm_params_path)
  }

  # Choose the best model (whichever model minimizes RMSE)
  lgbm_final_params <- ccao::model_lgbm_cap_num_leaves(
    select_best(lgbm_search, metric = "rmse")
  )
} else {

  # If CV is not enabled, load saved parameters from file if it exists
  # Otherwise use a set of sensible hand-chosen defaults
  if (file.exists(lgbm_params_path)) {
    lgbm_final_params <- ccao::model_lgbm_cap_num_leaves(
      select_best(readRDS(lgbm_params_path), metric = "rmse")
    )
  } else {
    lgbm_final_params <- ccao::model_lgbm_cap_num_leaves(data.frame(
      mtry = 13, min_n = 197, tree_depth = 13, learn_rate = 0.0105,
      loss_reduction = 0.0002, num_leaves = 3381
    ))
  }
}


### Step 3 - Finalize model

# Fit the final model using the training data and our final hyperparameters
# This is the model used to measure performance on the test set
lgbm_wflow_final_fit <- lgbm_wflow %>%
  ccao::model_lgbm_update_params(lgbm_final_params) %>%
  fit(data = train)

# Fit the final model using the full data (including the test set) and our final
# hyperparameters. This is the model used for actually assessing all properties
lgbm_wflow_final_full_fit <- lgbm_wflow %>%
  ccao::model_lgbm_update_params(lgbm_final_params) %>%
  fit(data = full_data)

# Remove unnecessary objects created while modeling
ccao::rm_intermediate("lgbm")

model_axe_recipe <- function(x) {
  stopifnot("recipe" %in% class(x))
  
  axed <- rapply(x, butcher::butcher, how = "replace")
  axed <- purrr::list_modify(axed, orig_lvls = NULL)
  class(axed) <- "recipe"
  
  return(axed)
}

ccao::model_lgbm_save(lgbm_wflow_final_fit$fit, "lgbm_fit.zip")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Finalize Models #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

### Get predictions and save objects

# Get predictions on the test set using all models then save to file. These
# predictions are used to evaluate model performance in model_report.Rmd
temp <- test %>%
  mutate(
    # enet = ccao::model_predict(
    #   enet_wflow_final_fit %>% pull_workflow_fit(),
    #   enet_wflow_final_fit %>% pull_workflow_prepped_recipe(),
    #   test
    # ),
    lgbm = model_predict(
      temp2 %>% extract_fit_parsnip(),
      temp2 %>% extract_recipe(),
      test, exp = F
    )
  ) %>%
  write_parquet(here("output", "data", "testdata.parquet"))

# Save the finalized model object to file so it can be used elsewhere. Note the
# model_save() function, which uses lgb.save() rather than saveRDS(), since
# lightgbm is picky about how its model objects are stored on disk
lgbm_wflow_final_fit %>%
  extract_fit_parsnip() %>%
  ccao::model_lgbm_save(here("lgbm_model.zip"))

# Save the finalized recipe object to file so it can be used to preprocess
# new data
lgbm_wflow_final_fit %>%
  extract_recipe() %>%
  ccao::model_axe_recipe() %>%
  saveRDS(here("lgbm_recipe.rds"))


### Generate reports

# Stop all timers and write to file. This is to track how long it takes the
# entire model to train
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

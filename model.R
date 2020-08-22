##### Setup ####

# Load the necessary libraries
library(tidymodels)
library(rlang)
library(arrow)
library(dplyr)
library(purrr)
library(sf)
library(assessr)
library(ccao)
library(furrr)
library(doFuture)
library(tictoc)
source("R/cknn_funs.R")
source("R/rf_funs.R")
source("R/metrics.R")

# Set seed for reproducibility and set multiprocessing plan
set.seed(27)
all_cores <- availableCores() - 1
plan("multiprocess", workers = all_cores)

# Set number of folds to use for cknn CV
cv_num_folds <- 5




##### Loading/Splitting Data #####

# Load the full set of training data, keep only good, complete observations
full_data <- read_parquet("data/modeldata.parquet") %>%
  filter(ind_arms_length & ind_complete_predictors & !is.na(geo_longitude)) %>%
  
  # Transform from lat lon to planar coordinates in meters
  st_as_sf(coords = c("geo_longitude", "geo_latitude"), crs = 4326) %>%
  st_transform(3435) %>%
  mutate(
    geo_longitude = st_coordinates(.)[, 1],
    geo_latitude = st_coordinates(.)[, 2]
  ) %>%
  st_set_geometry(NULL) %>%
  arrange(meta_sale_date) 

# Create train/test split by time, with most recent obs in the test set
splits <- initial_time_split(full_data, prop = 0.80)
train <- training(splits) 
test <- testing(splits) 




##### CkNN #####

### Step 1 - Determine variable weights and which vars to keep

# Get the set of variables to keep for clustering from vars_dict
cknn_possible_vars <- vars_dict %>%
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
  cknn_noncluster_vars, "char_beds"
)

# Create a recipe using cknn predictors which removes unnecessary vars and 
# collapses rare factors
cknn_recipe <- cknn_recp_prep(train, cknn_predictors)


### Step 2 - Cross-validation

# Create a grid of possible parameter values to loop through
cknn_param_grid <- expand_grid(
  m = seq(6, 14, 2),
  k = seq(10, 16, 2),
  l = seq(0.6, 0.9, 0.1)
)

# Create v folds in the training data and keep only clustering vars, then for
# each CV fold, calculate the values for all hyperparameters in param_grid
cknn_cv_fits <- train %>%
  vfold_cv(v = cv_num_folds) %>%
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


### Step 3 - Summarize results to find best hyperparameters

# Summarize results by parameter groups, averaging across folds
cknn_results <- bind_rows(cknn_cv_fits$cknn_results) %>%
  group_by(m, k, l) %>%
  summarize(across(everything(), mean)) %>%
  ungroup() %>%
  arrange(cod) 

# Print and plot results
cknn_results
cknn_grid_plot(cknn_results, m, k, l, cod)

# Take the best params according to RMSE
cknn_best_params <- cknn_results %>%
  filter(cod == min(cod, na.rm = TRUE))

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


### Step 4 - Prediction using best model

# Get predictions from cknn using the fully trained model
train <- train %>%
  mutate(cknn_sale_price = map_dbl(
    cknn_best_model$knn,
    ~ median(train$meta_sale_price[.x])
  ))

# Get predictions on the test set using the fully trained model
test <- test %>%
  mutate(cknn_sale_price = map_dbl(
    predict(
      cknn_best_model, 
      bake(prep(cknn_recipe), test) %>%
        select(-meta_sale_price, -geo_longitude, -geo_latitude) %>%
        as.data.frame(),
      lon = test %>% pull(geo_longitude),
      lat = test %>% pull(geo_latitude)
    )$knn,
    ~ median(train$meta_sale_price[.x])
  ))




##### Random Forest #####

### Step 1 - Determine which vars to use and prep data

# Get the full list of possible predictors from vars_dict
rf_possible_vars <- vars_dict %>%
  filter(var_is_predictor) %>%
  pull(var_name_standard) %>%
  unique() %>%
  na.omit()

# Include cknn prediction as predictor
rf_predictors <- c(rf_possible_vars, "cknn_sale_price")
rf_predictors <- rf_predictors[!rf_predictors %in% c("meta_nbhd")]

# Create recipe containing the training data
rf_recipe <- rf_recp_prep(train, rf_predictors)


### Step 2 - Model initialization

# Initialize random forest model
rf_model <- rand_forest(mtry = tune(), trees = tune()) %>%
  set_mode("regression") %>%
  set_engine("ranger") 

# Initialize tuning parameters  
rf_params <- rf_model %>%
  parameters() %>%
  update(
    mtry = mtry(range = c(5, floor(ncol(juice(prep(rf_recipe))) / 3))),
    trees = trees(range = c(500L, 2000L))
  ) 

# Initialize RF workflow
rf_wflow <- workflow() %>%
  add_model(rf_model) %>%
  add_recipe(rf_recipe) 

# Initialize tuning grid
rf_grid <- grid_regular(rf_params, levels = 3)


### Step 3 - Model cross-validation

# Begin CV tuning
tictoc::tic(msg = "RF CV model fitting complete!")
rf_search <- tune_grid(
  object = rf_wflow, 
  resamples = vfold_cv(train, v = cv_num_folds),
  grid = rf_grid,
  metrics = metric_set(rmse, rsq, codm),
  control = control_grid(verbose = TRUE, allow_par = FALSE)
)
tictoc::toc()

# Visualize results
autoplot(rf_search, metric = "rmse") +
  labs(title = "RF Grid Search Results") +
  theme_minimal()

# Choose the best model using 1 sd heuristic
rf_param_final <- select_by_one_std_err(rf_search, mtry, trees, metric = "rmse")

# Fit the final model using the full training data
rf_wflow_final_fit <- rf_wflow %>%
  finalize_workflow(rf_param_final) %>%
  fit(data = train)


### Step 4 - Prediction using best model

# Pull recipe and final fit. Kinda buggy, see:
# https://hansjoerg.me/2020/02/09/tidymodels-for-machine-learning/
rf_final_recp <- pull_workflow_prepped_recipe(rf_wflow_final_fit)
rf_final_fit <- pull_workflow_fit(rf_wflow_final_fit)

# Generate sale price predictions with the final model
test <- test %>%
  mutate(
    rf_sale_price = exp(predict(
      rf_final_fit,
      new_data = bake(rf_final_recp, test))$.pred
    )
  )

# Summarize test set results
test_long <- test %>%
  pivot_longer(
    cols = any_of(c("cknn_sale_price", "rf_sale_price")),
    names_to = "model_type",
    values_to = "predicted"
  ) %>%
  mutate(model_type = gsub("_sale_price", "", model_type)) 

# Summarize by model type
test_long %>%
  mutate(town_name = town_convert(meta_town_code)) %>%
  group_by(meta_town_code, model_type) %>%
  summarise(
    rmse = rmse_vec(meta_sale_price, predicted),
    cod = cod(predicted / meta_sale_price),
    prd = prd(predicted, meta_sale_price),
    prb = prb(predicted, meta_sale_price)
  )

# Ratio distribution by model type
test_long %>%
  mutate(ratio = predicted / meta_sale_price) %>%
ggplot() +
  geom_histogram(aes(x = ratio), binwidth = 0.1) +
  facet_wrap(vars(model_type), ncol = 1) +
  labs(title = "Distribution of Ratios by Model Type") +
  lims(x = c(-1, 3)) +
  theme_minimal() 


# TODO: Model cknn for whole county
# TODO: try model stacking
# TODO: figure out how to have a single model interface for training/prediction here

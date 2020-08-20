##### Setup ####

# Load the necessary libraries
library(arrow)
library(assessr)
library(ccao)
library(dplyr)
library(tidymodels)
library(furrr)
library(purrr)
library(tictoc)
library(sf)
source("R/cknn_funs.R")
source("R/rf_funs.R")

# Set seed for reproducibility and set multiprocessing plan
set.seed(27)
plan("multiprocess", workers = availableCores())



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
  "char_bldg_sf" = 10, "char_age" = 5, "char_beds" = 4,
  "char_bsmt" = 3, "char_gar1_size" = 3, "char_ext_wall" = 2.5
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

# Set number of folds to use for cknn CV
cknn_num_folds <- 5

# Create a grid of possible parameter values to loop through
cknn_param_grid <- expand_grid(
  m = seq(4, 13, 1),
  k = seq(6, 14, 2),
  l = seq(0.4, 0.8, 0.1)
)

# Create v folds in the training data and keep only clusering vars, then for
# each CV fold, calculate the values for all hyperparameters in param_grid
cknn_cv_fits <- train %>%
  filter(meta_town_code %in% c("17")) %>%  # temporary, for testing
  vfold_cv(v = cknn_num_folds) %>%
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
cknn_grid_plot(cknn_results, m, k, l, rmse)

# Take the best params according to RMSE
cknn_best_params <- cknn_results %>%
  filter(cod == min(cod))


### Step 4 - Prediction using best model
train_sub <- train %>%
  filter(meta_town_code %in% c("70"))  # temporary, for testing

# Train a new model on the full training set using the best found parameters
cknn_full_train <- train_sub %>%
  cknn_recp_prep(cknn_predictors) %>%
  prep() %>%
  juice()

cknn_best_model <- cknn(
    data = cknn_full_train %>%
      select(-meta_sale_price, -geo_longitude, -geo_latitude) %>%
      as.data.frame(),
    lon = cknn_full_train %>% pull(geo_longitude),
    lat = cknn_full_train %>% pull(geo_latitude),
    m = cknn_best_params$m,
    k = cknn_best_params$k,
    l = cknn_best_params$l,
    keep_data = TRUE
  )

# Use the model to extract the median sale price for obs. in the training set
train_sub <- train_sub %>%
  mutate( 
    cknn_sale_price = map_dbl(
      cknn_best_model$knn,
      ~ median(train_sub$meta_sale_price[.x])
    )
  )



##### Random Forest #####

rf_folds <- vfold_cv(train_sub, v = 5)

rf_possible_vars <- vars_dict %>%
  filter(var_is_predictor) %>%
  pull(var_name_standard) %>%
  unique() %>%
  na.omit()

rf_predictors <- c(rf_possible_vars, "cknn_sale_price")


rf_model <- 
  rand_forest(mtry = tune(), trees = tune()) %>%
  set_mode("regression") %>%
  set_engine("ranger") 

rf_wflow <-
  workflow() %>%
  add_model(rf_model) %>%
  add_recipe(rf_recp_prep(train_sub, rf_predictors))



rf_grid <- expand_grid(mtry = 3:5, trees = seq(1000, 2000, 1000))

rf_search <- tune_grid(rf_wflow, grid = rf_grid, resamples = rf_folds, control = control_grid(verbose = TRUE, allow_par = TRUE))

rf_param_final <- select_best(rf_search, mtry, trees,
                                        metric = "rmse")

rf_wflow_final <- finalize_workflow(rf_wflow, rf_param_final)

rf_wflow_final_fit <- fit(rf_wflow_final, data = train_sub)

dia_rec3     <- pull_workflow_prepped_recipe(rf_wflow_final_fit)
rf_final_fit <- pull_workflow_fit(rf_wflow_final_fit)

train_final <- train_sub %>%
  select(meta_sale_price, meta_town_code, cknn_sale_price) %>%
  mutate(
    predicted = exp(predict(
      rf_final_fit,
      new_data = bake(dia_rec3, train_sub))$.pred
    )
  )
train_final %>%
  summarise(
    rmse = rmse_vec(meta_sale_price, predicted),
    cod = cod(predicted / meta_sale_price),
    prd = prd(predicted, meta_sale_price),
    prb = prb(predicted, meta_sale_price)
  )

#############

test_sub <- test %>%
  filter(meta_town_code == "70") 

test_full_set <- test_sub %>%
  cknn_recp_prep(., cknn_predictors) %>%
  prep() %>%
  juice() 

# Use the model to extract the median sale price for obs. in the training set
test_sub <- test_sub %>%
  mutate( 
    cknn_sale_price = map_dbl(
      predict(
        cknn_best_model, 
        newdata = test_full_set %>% 
          select(-meta_sale_price, -geo_longitude, -geo_latitude) %>%
          as.data.frame(),
        lon = test_full_set %>% pull(geo_longitude),
        lat = test_full_set %>% pull(geo_latitude)
      )$knn,
      ~ median(train_sub$meta_sale_price[.x])
    )
  ) 

test_final <- test_sub %>%
  select(meta_sale_price, meta_town_code, cknn_sale_price) %>%
  mutate(
    predicted = exp(predict(
      rf_final_fit,
      new_data = bake(dia_rec3, test_sub))$.pred
    )
  )
test_final %>%
  filter(!is_outlier(predicted / meta_sale_price)) %>%
  summarise(
    rmse = rmse_vec(meta_sale_price, predicted),
    cod = cod(predicted / meta_sale_price),
    prd = prd(predicted, meta_sale_price),
    prb = prb(predicted, meta_sale_price)
  )

# TODO: integrate cknn results with RF

# TODO: Model cknn for whole county
# TODO: try model stacking
# TODO: figure out how to have a single model interface for training/prediction here

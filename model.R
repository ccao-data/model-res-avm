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

# Set seed for reproducibility and set multiprocessing plan
set.seed(27)
plan("multiprocess", workers = availableCores() - 2)



##### Loading Data #####

# Load the full set of training data, keep only good, complete observations
training_data <- read_parquet("data/modeldata.parquet") %>%
  filter(ind_arms_length & ind_complete_predictors & !is.na(geo_longitude)) %>%
  
  # Transform from lat lon to planar coords
  st_as_sf(coords = c("geo_longitude", "geo_latitude"), crs = 4326) %>%
  st_transform(3435) %>%
  mutate(
    geo_longitude = st_coordinates(.)[, 1],
    geo_latitude = st_coordinates(.)[, 2]
  ) %>%
  st_set_geometry(NULL) 


  
##### CkNN #####


### Step 1 - Determine variable weights and which vars to keep
cknn_var_weights <- c("char_bldg_sf" = 8, "char_age" = 4, "char_ext_wall" = 2)

# Allocate the set of variables to keep for clustering
cknn_rm_vars <- c("geo_longitude", "geo_latitude", "meta_sale_price")
cknn_predictors <- vars_dict %>%
  filter(var_is_clustered) %>%
  pivot_longer(var_name_addchars:var_name_standard) %>%
  pull(value) %>%
  unique() %>%
  na.omit()


### Step 2 - Determine the optimal model

# Set number of folds to use for CV
cknn_num_folds <- 5

# Create a grid of possible parameter values to loop through
cknn_param_grid <- expand_grid(
  m = seq(5, 15, 1),
  k = seq(8, 16, 2),
  l = seq(0.3, 0.7, 0.1)
)

# Create the CkNN data as well as the train/CV/test split
cknn_data <- training_data %>%
  select(any_of(cknn_predictors), meta_sale_date, meta_town_code) %>%
  arrange(meta_sale_date) %>%
  select(
    meta_sale_price, geo_longitude, geo_latitude, 
    char_bldg_sf, char_age, char_bsmt, char_bsmt_fin, char_ext_wall,
    char_air, char_heat, char_roof_cnst, meta_sale_date, meta_town_code
  ) %>%
  filter(meta_town_code %in% c("17", "23"))

cknn_split <- initial_time_split(cknn_data, prop = 0.80)
cknn_train <- training(cknn_split)
cknn_test <- testing(cknn_split)
cknn_folds <- vfold_cv(cknn_train, v = cknn_num_folds)

# For each CV fold, calculate the values for all hyperparameters in param_grid
cknn_cv_fits <- cknn_folds %>%
  mutate(
    df_ana = map(splits, analysis),
    df_ass = map(splits, assessment)
  ) %>%
  mutate(
    recipe = map(df_ana, ~ prep(cknn_recp_prep(cknn_train, cknn_predictors), training = .x)),
    df_ana = map(recipe, juice),
    df_ass = map2(recipe, df_ass, ~ bake(.x, new_data = .y))
  ) %>%
  mutate(
    cknn_model = cknn_fit(df_ana, cknn_param_grid, cknn_rm_vars, cknn_var_weights),
    cknn_predict = cknn_predict(df_ana, df_ass, cknn_model, cknn_rm_vars),
    cknn_results = cknn_eval(df_ana, df_ass, cknn_predict)
  )


### Step 3 - Summarize results to find best hyperparams

# Summarize results by param, averaging across folds
cknn_results <- bind_rows(cknn_cv_fits$cknn_results) %>%
  group_by(m, k, l) %>%
  summarize(across(everything(), mean)) %>%
  arrange(cod)

# Print and plot results
cknn_results
cknn_grid_plot(cknn_results, m, k, l, cod)

# Take the best params according to RMSE
cknn_best_model <- cknn_results %>%
  filter(rmse == max(rmse))



##### Random Forest #####

# TODO: Model cknn for whole county
# TODO: integrate cknn results with RF
# TODO: Figure out CV for two-stage model
# TODO: try model stacking
# TODO: figure out how to have a single model interface for training/prediction here

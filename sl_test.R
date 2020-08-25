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
source("superlearner.R")
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




# Models

enet_model <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

enet_grid <- grid_latin_hypercube(penalty(), mixture(), size = 10)

rf_model <- rand_forest(mtry = tune(), trees = 500, min_n = tune()) %>%
  set_mode("regression") %>%
  set_engine("ranger") 

rf_grid <- grid_latin_hypercube(
  mtry(range = c(5, floor(train_p / 3))),
  min_n(),
  size = 5
)

metalearner <- linear_reg(penalty = 0.01, mixture = 0) %>% 
  set_engine("glmnet")


# Create learner

temp <- super_learner(
  models  = list("rf" = rf_model, "enet" = enet_model),
  params  = list("rf" = rf_grid, "enet" = enet_grid),
  recipes = list(
    "rf" = train_recipe %>% dummy_recp_prep(),
    "enet" = train_recipe %>% dummy_recp_prep()
  ),
  train,
  metalearner,
  train_recipe
)


iris <- iris %>%
  mutate(prediction = predict(temp, .)) 

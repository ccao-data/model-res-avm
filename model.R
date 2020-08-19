library(arrow)
library(assessr)
library(ccao)
library(dplyr)
library(tidymodels)
library(furrr)
library(purrr)
library(tictoc)
library(sf)

set.seed(2020)
plan("multiprocess", workers = 4)

# Data prep
cknn_data <- read_parquet("data/modeldata.parquet") %>%
  filter(!is.na(geo_latitude) & !is.na(geo_longitude)) %>%
  st_as_sf(coords = c("geo_longitude", "geo_latitude"), crs = 4326) %>%
  st_transform(3435) %>%
  mutate(
    geo_longitude = st_coordinates(.)[, 1],
    geo_latitude = st_coordinates(.)[, 2]
  ) %>%
  st_set_geometry(NULL)

cknn_split <- initial_time_split(cknn_data, prop = 0.85)
cknn_train <- training(cknn_split)
cknn_test  <- testing(cknn_split)

# parameters
# cknn_param_k <- seq(5, 25, 5)
# cknn_param_m <- seq(2, 12, 1)
# cknn_param_l <- seq(0.1, 0.9, 0.1)
cknn_param_grid <- expand_grid(
  m = seq(8, 10, 1),
  k = 10,
  l = seq(0.8, 0.9, 0.1)
)

# predictors
cknn_var_weights <- c("char_bldg_sf" = 8, "char_age" = 4, "char_ext_wall" = 2)
cknn_predictors <- vars_dict %>%
  filter(var_is_clustered) %>%
  pivot_longer(var_name_addchars:var_name_standard) %>%
  pull(value) %>%
  unique() %>%
  na.omit()

cknn_predictors <- c(cknn_predictors, "geo_latitude", "geo_longitude")


# recipe
cknn_recp <- recipe(meta_sale_price ~ ., data = cknn_train) %>%
  step_rm(-any_of(cknn_predictors), -all_outcomes()) %>%
  step_unknown(all_nominal()) %>%
  step_other(all_nominal(), threshold = 0.05) %>%
  step_naomit(all_predictors()) 


# CV split
cknn_folds <- vfold_cv(cknn_train, v = 5)

# Model fitting/CV
cknn_fit <- cknn_folds %>% 
  
  # Create the split data for CV
  mutate(
    df_ana = map(splits, analysis),
    df_ass = map(splits, assessment)
  ) %>%
  mutate(
    recipe = map(df_ana, ~ prep(cknn_recp, training = .x)),
    df_ana = map(recipe,  juice),
    df_ass = map2(recipe, df_ass, ~ bake(.x, new_data = .y))
  ) %>%
  
  # Actually run the models with different params for each fold
  mutate(
    cknn_model = future_map(df_ana,
      function(fold) future_pmap(cknn_param_grid,
        function(m, k, l) cknn(
          data = fold %>%
            select(-geo_longitude, -geo_latitude, -meta_sale_price) %>%
            as.data.frame(),
          lon = fold %>% pull(geo_longitude),
          lat = fold %>% pull(geo_latitude),
          m = m,
          k = k,
          l = l,
          var_weights = cknn_var_weights
        )
      )
    )
  ) %>%
  
  # Use the generated models to create predictions for each validation set
  mutate(
    cknn_predict = future_map2(df_ass, cknn_model,
      function(newdata, model_objects) future_map(model_objects, 
        function(obj) predict(
          object = obj,
          newdata = newdata %>%
            select(-geo_longitude, -geo_latitude, -meta_sale_price) %>%
            as.data.frame(),
          lon = newdata %>% pull(geo_longitude),
          lat = newdata %>% pull(geo_latitude)
        )
      )
    )
  ) %>%

  # Calculate actual performance stats for each set of params/fold
  mutate(
    cknn_out = future_pmap(list(df_ana, df_ass, cknn_predict),
      function(origdata, newdata, knn_predictions) future_map(knn_predictions,
        function(knn_pred) {
          
          newdata %>%
            select(actual = meta_sale_price) %>%
            mutate(estimate = map_dbl(
              knn_pred$knn, ~ median(origdata$meta_sale_price[.x])
            )) %>%
            summarize(
              rmse = rmse_vec(actual, estimate),
              cod = cod(estimate / actual),
              prd = prd(estimate, actual),
              prb = prb(estimate, actual),
              m = knn_pred$m,
              k = knn_pred$k,
              l = knn_pred$l
            )
        }) 
      )
    )

# Summarize results
bind_rows(cknn_fit4$cknn_out) %>%
  group_by(m, k, l) %>%
  summarize(across(everything(), mean)) %>%
  arrange(rmse)


# TODO: add meta_sale_price, geo_lat, geo_lon to clustering

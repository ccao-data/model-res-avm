# Setup
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
cknn_predictors <- vars_dict %>%
  filter(var_is_clustered) %>%
  pivot_longer(var_name_addchars:var_name_standard) %>%
  pull(value) %>%
  unique() %>%
  na.omit()

cknn_predictors <- c(cknn_predictors, "geo_latitude", "geo_longitude", "meta_sale_price")

cknn_data <- read_parquet("data/modeldata.parquet") %>%
  filter(!is.na(geo_latitude) & !is.na(geo_longitude)) %>%
  filter(between(meta_sale_price, 10000, 1e7)) %>%
  st_as_sf(coords = c("geo_longitude", "geo_latitude"), crs = 4326) %>%
  st_transform(3435) %>%
  mutate(
    geo_longitude = st_coordinates(.)[, 1],
    geo_latitude = st_coordinates(.)[, 2]
  ) %>%
  st_set_geometry(NULL) %>%
  select(any_of(cknn_predictors), meta_sale_date, meta_town_code) %>%
  arrange(meta_sale_date) %>%
  filter(meta_town_code %in% c("17", "23"))

cknn_split <- initial_time_split(cknn_data, prop = 0.85)
cknn_train <- training(cknn_split)
cknn_test <- testing(cknn_split)


# Parameters
cknn_var_weights <- c("char_bldg_sf" = 8, "char_age" = 4, "char_ext_wall" = 2)
cknn_param_grid <- expand_grid(
  m = seq(6, 20, 2),
  k = seq(8, 18, 2),
  l = seq(0.3, 0.7, 0.1)
)

# recipe
cknn_recp <- recipe(meta_sale_price ~ ., data = cknn_train) %>%
  step_rm(-any_of(cknn_predictors), -all_outcomes()) %>%
  step_unknown(all_nominal()) %>%
  step_other(all_nominal(), threshold = 0.05) %>%
  step_naomit(all_predictors())


# CV split
cknn_folds <- vfold_cv(cknn_train, v = 5)

# Model fitting/CV
cknn_fit1 <- cknn_folds %>%

  # Create the split data for CV
  mutate(
    df_ana = map(splits, analysis),
    df_ass = map(splits, assessment)
  ) %>%
  mutate(
    recipe = map(df_ana, ~ prep(cknn_recp, training = .x)),
    df_ana = map(recipe, juice),
    df_ass = map2(recipe, df_ass, ~ bake(.x, new_data = .y))
  ) 

# Actually run the models with different params for each fold
tic()
cknn_fit2 <- cknn_fit1 %>%
  mutate(
    cknn_model = map(
      df_ana,
      function(fold) {
        future_pmap(
          cknn_param_grid,
          function(m, k, l) {
            cknn(
              data = fold %>%
                select(-geo_longitude, -geo_latitude, -meta_sale_price) %>%
                as.data.frame(),
              lon = fold %>% pull(geo_longitude),
              lat = fold %>% pull(geo_latitude),
              m = m,
              k = k,
              l = l,
              var_weights = cknn_var_weights,
              keep_data = FALSE
            )
          }
        )
      }
    )
  )
toc()

# Use the generated models to create predictions for each validation set
tic()
cknn_fit2 <- cknn_fit2 %>%
  mutate(
    cknn_predict = pmap(
      list(df_ana, df_ass, cknn_model),
      function(origdata, newdata, model_objects) {
        map(
          model_objects,
          function(obj) {
            predict(
              object = obj,
              newdata = newdata %>%
                select(-geo_longitude, -geo_latitude, -meta_sale_price) %>%
                as.data.frame(),
              lon = newdata %>% pull(geo_longitude),
              lat = newdata %>% pull(geo_latitude),
              data = origdata %>%
                select(-geo_longitude, -geo_latitude, -meta_sale_price) %>%
                as.data.frame()
            )
          }
        )
      }
    )
  )
toc()

# Calculate actual performance stats for each set of params/fold
cknn_fit2 <- cknn_fit2 %>%
  mutate(
    cknn_out = pmap(
      list(df_ana, df_ass, cknn_predict),
      function(origdata, newdata, knn_predictions) {
        map(
          knn_predictions,
          function(knn_pred) {
            newdata %>%
              select(actual = meta_sale_price) %>%
              mutate(estimate = map_dbl(
                knn_pred$knn, ~ median(origdata$meta_sale_price[.x])
              )) %>%
              summarize(
                rmse = rmse_vec(actual, estimate),
                rsq = rsq_trad_vec(actual, estimate),
                cod = cod(estimate / actual),
                prd = prd(estimate, actual),
                prb = prb(estimate, actual),
                m = knn_pred$m,
                k = knn_pred$k,
                l = knn_pred$l
              )
          }
        )
      }
    )
  )


# Summarize results
temp <- bind_rows(cknn_fit2$cknn_out) %>%
  group_by(m, k, l) %>%
  summarize(across(everything(), mean)) %>%
  arrange(rmse) %>%
  filter(!m == 2) %>%
  mutate(m2 = factor(paste("m (num. clusters) =", m))) %>%
  mutate(m2 = forcats::fct_reorder(m2, m)) %>%

temp %>%
  ggplot() +
  geom_tile(aes(x = factor(k), y = factor(l), fill = cod)) +
  viridis::scale_fill_viridis(name = "COD", option = "viridis") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = "k (num. comparables)", y = "l (dist. trade-off)") +
  facet_wrap(vars(m2)) +
  theme_bw() +
  theme(
    legend.title = element_text(margin = margin(b = 10)),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )

# TODO: add meta_sale_price, geo_lat, geo_lon to vars_dict

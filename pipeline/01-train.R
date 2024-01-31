#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Setup ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

library(rules)
library(Cubist)
library(doParallel)

# NOTE: See DESCRIPTION for library dependencies and R/setup.R for
# variables used in each pipeline stage

# Load libraries, helpers, and recipes from files
purrr::walk(list.files("R/", "\\.R$", full.names = TRUE), source)

# Print a run note and type for context at the top of CI logs
message("Run note: ", run_note)
message("Run type: ", run_type)

cl <- makePSOCKcluster(num_threads)
registerDoParallel(cl)



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
  arrange(meta_sale_date) %>%
  mutate(meta_sale_price = log(meta_sale_price))

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
  data = training_data_full,
  pred_vars = params$model$predictor$all,
  cat_vars = params$model$predictor$categorical,
  id_vars = params$model$predictor$id
)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Linear Model --------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Creating and fitting linear baseline model")

# Create a linear model recipe with additional imputation, transformations,
# and feature interactions
lin_recipe <- model_lin_recipe(
  data = training_data_full,
  pred_vars = params$model$predictor$all,
  cat_vars = params$model$predictor$categorical,
  id_vars = params$model$predictor$id
)

# Create a linear model specification and workflow
lin_model <- parsnip::linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")
lin_wflow <- workflow() %>%
  add_model(lin_model) %>%
  add_recipe(
    recipe = lin_recipe,
    blueprint = hardhat::default_recipe_blueprint(allow_novel_levels = TRUE)
  )

# Fit the linear model on the training data
lin_wflow_final_fit <- lin_wflow %>%
  fit(data = train %>% mutate(meta_sale_price = log(meta_sale_price)))




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4. Cubist Model --------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Creating and fitting Cubist model")

cube_model <- parsnip::cubist_rules(
  mode = "regression",
  committees = tune(),
  neighbors = tune(),
  max_rules = tune(),
  engine = "Cubist"
)

cube_wflow <- workflow() %>%
  add_model(cube_model) %>%
  add_recipe(
    recipe = train_recipe,
    blueprint = hardhat::default_recipe_blueprint(allow_novel_levels = TRUE)
  )

train_folds <- create_rolling_origin_splits(
  data = train,
  v = 5,
  overlap_months = 15,
  date_col = meta_sale_date,
  val_prop = params$model$parameter$validation_prop,
  train_includes_val = params$model$parameter$validation_prop > 0,
  cumulative = FALSE
)

cube_params <- cube_wflow %>%
  hardhat::extract_parameter_set_dials() %>%
  update(
    committees = rules::committees(c(10, 100)),
    neighbors = dials::neighbors(c(0, 9)),
    max_rules = rules::max_rules()
  )

cube_search <- tune_grid(
  object = cube_wflow,
  resamples = train_folds,
  metrics = metric_set(rmse, mape, mae),
  param_info = cube_params,
  grid = 15,
  control = control_grid(
    verbose = TRUE,
    allow_par = TRUE,
    parallel_over = "everything"
  )
)

message("Fitting final model on training data")
cube_wflow_final_fit <- cube_wflow %>%
  finalize_workflow(
    select_best(cube_search, metric = params$cv$best_metric)
  ) %>%
  fit(data = train)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 5. Finalize Models -----------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Finalizing and saving trained model")

test %>%
  mutate(
    pred_card_initial_fmv = exp(predict(cube_wflow_final_fit, test)$.pred)
  ) %>%
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
  as_tibble() %>%
  filter(meta_triad_code == "1") %>%
  mutate(town_name = ccao::town_convert(meta_township_code))
filter(is.finite(pred_card_initial_fmv)) %>%
  group_by(town_name) %>%
  summarize(
    count = n(),
    cod = cod(pred_card_initial_fmv / meta_sale_price),
    prd = prd(pred_card_initial_fmv, meta_sale_price),
    prb = prb(pred_card_initial_fmv, meta_sale_price),
    rmse = rmse_vec(meta_sale_price, pred_card_initial_fmv),
    mdape = mdape_vec(meta_sale_price, pred_card_initial_fmv),
    rsq = rsq_vec(meta_sale_price, pred_card_initial_fmv)
  ) %>%
  arrange(cod) %>%
  print(n = Inf)

stop("Cubist run successful")

# Helper to return prediction from a model and recipe
model_fit <- function(data, recipe, model, col_name) {
  data <- data %>%
    mutate(
      {{col_name}} := exp(predict(model, new_data = bake(recipe, data))$.pred)
    )
  
  return(data)
}

# Helper function to prep data for cknn
cknn_recp_prep <- function(data, keep_vars) {
  recipe(meta_sale_price ~ ., data = data) %>%
    step_rm(-any_of(keep_vars), -all_outcomes()) %>%
    step_unknown(all_nominal()) %>%
    step_other(all_nominal(), threshold = 0.005) %>%
    step_naomit(all_predictors())
}


# Helper function to prep data for all non-cknn models
mod_recp_prep <- function(data, keep_vars) {
  recipe(meta_sale_price ~ ., data = data) %>%
    update_role(meta_pin, new_role = "ID") %>%
    step_rm(-any_of(keep_vars), -all_outcomes()) %>%
    step_unknown(all_nominal()) %>%
    step_other(all_nominal(), -any_of("town_nbhd"), threshold = 0.005) %>%
    step_naomit(all_predictors()) %>%
    step_zv(all_numeric(), -all_outcomes()) %>%
    step_log(
      all_outcomes(),
      ends_with("_price"), ends_with("_sf"), ends_with("_amt_paid"),
      offset = 1
    )
}


# Dummy vars specifically for lasso
xgb_recp_prep <- function(recipe) {
  recipe %>%
    step_mutate_at(starts_with("ind_"), fn = as.numeric) %>%
    step_dummy(all_nominal(), -all_outcomes())
}


# Function to create recipe for stacking model
stack_recp_prep <- function(data) {
  recipe(meta_sale_price ~ ., data = data) %>%
    step_rm(-ends_with("_sale_price"), -all_outcomes()) %>%
    step_naomit(all_predictors()) %>%
    step_log(all_predictors(), all_outcomes())
}


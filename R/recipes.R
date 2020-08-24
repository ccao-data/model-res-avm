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
    step_rm(-any_of(keep_vars), -all_outcomes()) %>%
    step_unknown(all_nominal()) %>%
    step_other(all_nominal(), threshold = 0.005) %>%
    step_naomit(all_predictors(), all_outcomes()) %>%
    step_log(
      all_outcomes(),
      ends_with("_price"), ends_with("_sf"), ends_with("_amt_paid"),
      offset = 1
    ) %>%
    step_zv(all_numeric(), -all_outcomes()) %>%
    step_corr(all_numeric(), -all_outcomes()) %>%
    step_poly(ends_with("_age"), ends_with("_sf"), degree = 2)
}


# Dummy vars specifically for xgb and lasso
dummy_recp_prep <- function(recipe) {
  recipe %>%
    step_mutate_at(starts_with("ind_"), fn = as.numeric) %>%
    step_dummy(all_nominal(), -all_outcomes())
}


# Scale vars to between 1 and 0
scale_recp_prep <- function(recipe) {
  recipe %>%
    step_range(all_numeric(), min = 0, max = 1) %>%
    step_zv(all_numeric())
}


# Function to create recipe for stacking model
stack_recp_prep <- function(data) {
  recipe(meta_sale_price ~ ., data = data) %>%
    step_rm(-ends_with("_sale_price"), starts_with("stack_"), -all_outcomes()) %>%
    step_naomit(all_predictors()) %>%
    step_log(all_predictors(), all_outcomes())
}

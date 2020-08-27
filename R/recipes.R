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


# Function to create recipe for stacking model
stack_recp_prep <- function(data, keep_vars = NULL) {
  recipe(meta_sale_price ~ ., data = data) %>%
    step_rm(
      -c(meta_town_code, meta_sale_price), 
      -any_of(keep_vars), 
      -all_outcomes()
    ) %>%
    step_naomit(all_predictors()) %>%
    step_log(all_numeric(), all_outcomes())
    # step_dummy(meta_town_code) %>%
    # step_interact(
    #   ~ any_of(c("enet", "xgb", "rf")):starts_with("meta_town_code")
    # )
}

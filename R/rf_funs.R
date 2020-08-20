# Helper function to prep data for cknn
rf_recp_prep <- function(data, keep_vars) {
  recipe(meta_sale_price ~ ., data = data) %>%
    step_rm(-any_of(keep_vars), -all_outcomes()) %>%
    step_unknown(all_nominal()) %>%
    step_other(all_nominal(), threshold = 0.005) %>%
    step_naomit(all_predictors()) %>%
    step_log(all_outcomes()) %>%
    step_normalize(all_numeric(), -starts_with("geo_"), -starts_with("ind_"), -all_outcomes()) %>%
    step_dummy(all_nominal())
}


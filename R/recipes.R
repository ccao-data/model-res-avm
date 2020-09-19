# Main data prep recipe, removes unnecessary variables, cleans up factors,
# removes low/no variance and highly correlated vars, log + polynomial trans
mod_recp_prep <- function(data, keep_vars, id_vars) {
  recipe(meta_sale_price ~ ., data = data) %>%
    
    # Convert PIN to numeric so it's not set to NA by missing factor levels
    # Set PIN role to "id" so it's not included in the actual fit
    update_role(any_of(id_vars), new_role = "id") %>%
    step_mutate_at(has_role("id"), fn = ~ as.numeric(as.character(.))) %>%
    step_rm(-any_of(keep_vars), -all_outcomes(), -has_role("id")) %>%
    
    # Preprocessing for all predictors and outcome
    step_unknown(all_nominal(), -has_role("id")) %>%
    step_other(
      all_nominal(), -has_role("id"),
      threshold = 0.005
    ) %>%
    step_naomit(
      all_predictors(), all_outcomes()
    ) %>%
    step_log(
      all_outcomes(), ends_with("_sf"), contains("income"),
      offset = 1
    ) %>%
    step_nzv(
      all_predictors(), -all_outcomes(), -has_role("id"),
      unique_cut = 0.05
    ) %>%
    step_corr(
      all_numeric(), -all_outcomes(), -has_role("id"), 
      threshold = 0.95
    ) %>%
    step_poly(
      ends_with("_age"), ends_with("_sf"),
      degree = 2
    )
}


# Add dummy vars specifically for xgb and enet, not necessary for others
# (Converts categorical vars to one-hot encoding)
dummy_recp_prep <- function(recipe) {
  recipe %>%
    step_mutate_at(starts_with("ind_"), fn = ~ as.numeric(.)) %>%
    step_dummy(all_nominal(), -all_outcomes(), -has_role("id"))
}


# Recipe for stacking/meta model, goal here is to interact each vector of fitted
# model values with spatial regimes variable
stack_recp_prep <- function(data, keep_vars = NULL) {
  recipe(meta_sale_price ~ ., data = data) %>%
    add_role(all_numeric(), -all_outcomes(), new_role = "model") %>%
    step_rm(-any_of(keep_vars), -all_outcomes()) %>%
    step_naomit(all_predictors()) %>%
    step_log(all_numeric(), all_outcomes()) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_interact(~ has_role("model") * matches(".X\\d+"))
}

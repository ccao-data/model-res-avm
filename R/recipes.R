# Main data preprocessing recipe. All of these steps are applied to the training
# set and any new data fed to the model (using bake()). Goal here is to create
# a clean matrix with few/no missing values, well-behaved distributions, and
# cleaned up categoricals
mod_recp_prep <- function(data, keep_vars, id_vars) {
  recipe(meta_sale_price ~ ., data = data) %>%

    # Set PIN role to "id" so it's not included in the actual fit
    # Convert PIN to numeric so it's not set to NA for missing factor levels
    update_role(any_of(id_vars), new_role = "id") %>%
    step_mutate_at(has_role("id"), fn = ~ as.numeric(as.character(.))) %>%

    # Remove any variables not an outcome var or in mod_predictors vector
    # created in model.R
    step_rm(-any_of(keep_vars), -all_outcomes(), -has_role("id")) %>%

    # Replace NA in factors with "Unknown" and gather rare factor values into
    # "Other" category
    step_unknown(all_nominal(), -has_role("id")) %>%
    step_other(
      all_nominal(), -has_role("id"),
      -any_of(c("meta_town_code", "meta_nbhd")),
      -starts_with("geo_school_"),
      threshold = 0.005
    ) %>%

    # Drop any remaining rows with missing values in their predictors. Note that
    # this will be skipped when baking on new data, so the # of predicted values
    # will always be equal to the number of rows in the input data, regardless
    # of whether the input data has missing values
    step_naomit(
      all_predictors(), all_outcomes(),
      skip = TRUE
    ) %>%

    # Log transform price and income variables (these are all extremely skewed)
    step_log(
      all_outcomes(),
      ends_with("_sf"),
      contains("income"),
      contains("meta_nbhd_"),
      offset = 1
    ) %>%

    # Remove variables that are highly correlated with one another
    step_corr(
      all_numeric(), -all_outcomes(), -has_role("id"),
      threshold = 0.95
    ) %>%

    # Create polynomial transformation of certain important characteristics
    step_poly(
      ends_with("_age"), ends_with("_sf"),
      degree = 2
    )
}


# Extra recipe step for concerting categoricals to one-hot encoding. Only used
# in cases where categoricals are not handled natively
dummy_recp_prep <- function(recipe) {
  recipe %>%
    step_mutate_at(starts_with("ind_"), fn = ~ as.numeric(.)) %>%
    step_dummy(all_nominal(), -all_outcomes(), -has_role("id"))
}

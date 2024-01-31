#' Create the main data cleaning Tidymodels recipe
#'
#' @description Main data pre-processing recipe. These steps are applied to the
#' training set and any new data fed to the model (using bake()). Goal of this
#' recipe is to create an input matrix with cleaned up categoricals
#'
#' @param data A data frame containing the input data. Can contain extraneous
#'   columns and missing values.
#' @param pred_vars Character vector of column names to keep for modeling. These
#'   will be the right-hand side of the regression AKA predictors.
#' @param cat_vars Character vector of categorical column names. These will be
#'   integer-encoded (base 0).
#' @param id_vars Character vector of ID variables. These can be kept in "baked"
#'   data without being treated as predictors.
#'
#' @return A recipe object that can be used to clean model input data.
#'
model_main_recipe <- function(data, pred_vars, cat_vars, id_vars) {
  recipe(data) %>%
    # Set the role of each variable in the input data
    update_role(meta_sale_price, new_role = "outcome") %>%
    update_role(all_of(!!pred_vars), new_role = "predictor") %>%
    update_role(all_of(!!id_vars), new_role = "ID") %>%
    update_role_requirements("ID", bake = FALSE) %>%
    update_role_requirements("NA", bake = FALSE) %>%
    # Remove any variables not an outcome var or in the pred_vars vector
    step_rm(any_of("time_split")) %>%
    step_rm(-all_outcomes(), -all_predictors(), -has_role("ID")) %>%
    step_mutate_at(where(is.logical), fn = ~ as.integer(.x)) %>%
    step_impute_median(all_numeric_predictors(), -has_role("ID")) %>%
    step_impute_mode(all_nominal_predictors(), -has_role("ID")) %>%
    step_zv(all_predictors()) %>%
    # Replace novel levels with "new"
    step_novel(all_of(!!cat_vars), -has_role("ID")) %>%
    # Replace NA in factors with "unknown"
    step_unknown(all_of(!!cat_vars), -has_role("ID")) %>%
    step_BoxCox(
      acs5_median_income_per_capita_past_year,
      acs5_median_income_household_past_year,
      char_bldg_sf
    ) %>%
    step_normalize(
      acs5_median_household_renter_occupied_gross_rent,
      loc_longitude, loc_latitude
    )
}


#' Create a Tidymodels recipe for linear baseline model
#'
#' @description Transforms categoricals using embeddings, normalizes numeric
#'  predictors, creates interaction terms, and imputes missing values.
#'
#' @param data A data frame containing the input data.
#' @param pred_vars Character vector of column names to keep for modeling. These
#'   will be the right-hand side of the regression AKA predictors.
#' @param cat_vars Character vector of categorical column names. These will be
#'   transformed/encoded using embeddings.
#' @param id_vars Character vector of ID variables. These can be kept in "baked"
#'   data without being treated as predictors.
#'
#' @return A recipe object that can be used to clean model input data.
#'
model_lin_recipe <- function(data, pred_vars, cat_vars, id_vars) {
  recipe(data) %>%
    # Set the role of each variable in the input data
    update_role(meta_sale_price, new_role = "outcome") %>%
    update_role(all_of(pred_vars), new_role = "predictor") %>%
    update_role(all_of(id_vars), new_role = "ID") %>%
    update_role_requirements("ID", bake = FALSE) %>%
    update_role_requirements("NA", bake = FALSE) %>%
    # Remove any variables not an outcome var or in the pred_vars vector
    step_rm(any_of("time_split")) %>%
    step_rm(-all_outcomes(), -all_predictors(), -has_role("ID")) %>%
    # Drop extra location predictors that aren't nbhd or township
    step_rm(starts_with("loc_"), -all_numeric_predictors()) %>%
    # Transforms and imputations
    step_mutate(char_bldg_sf = ifelse(char_bldg_sf == 0, 1, char_bldg_sf)) %>%
    step_mutate_at(
      starts_with("ind_"),
      starts_with("ccao_is"),
      fn = as.numeric
    ) %>%
    step_impute_median(all_numeric_predictors(), -has_role("ID")) %>%
    step_impute_mode(all_nominal_predictors(), -has_role("ID")) %>%
    step_zv(all_predictors()) %>%
    # Replace novel levels with "new"
    step_novel(all_nominal_predictors(), -has_role("ID")) %>%
    # Replace NA in factors with "unknown"
    step_unknown(all_nominal_predictors(), -has_role("ID")) %>%
    # Dummify categorical predictors
    step_dummy(all_nominal_predictors(), -has_role("ID"), one_hot = TRUE) %>%
    # Drop any predictors with near-zero variance, add interactions, and
    # perform transforms
    step_interact(terms = ~ starts_with("meta_township_"):char_bldg_sf) %>%
    step_BoxCox(
      acs5_median_income_per_capita_past_year,
      acs5_median_income_household_past_year,
      char_bldg_sf
    ) %>%
    step_normalize(
      acs5_median_household_renter_occupied_gross_rent,
      loc_longitude, loc_latitude
    )
}

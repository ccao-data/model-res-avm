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
    update_role(all_of(pred_vars), new_role = "predictor") %>%
    update_role(all_of(id_vars), new_role = "ID") %>%
    update_role_requirements("ID", bake = FALSE) %>%
    update_role_requirements("NA", bake = FALSE) %>%
    # Remove any variables not an outcome var or in the pred_vars vector
    step_rm(any_of("time_split")) %>%
    step_rm(-all_outcomes(), -all_predictors(), -has_role("ID")) %>%
    # Convert nbhd to a numeric feature
    step_mutate(meta_nbhd_code = as.numeric(meta_nbhd_code)) %>%
    # Replace novel levels with "new"
    step_novel(all_of(cat_vars), -has_role("ID")) %>%
    # Replace NA in factors with "unknown"
    step_unknown(all_of(cat_vars), -has_role("ID")) %>%
    # Convert factors to 0-indexed integers
    step_integer(
      all_of(cat_vars), -has_role("ID"),
      strict = TRUE, zero_based = TRUE
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
    # Transforms and imputations
    step_mutate(char_bldg_sf = ifelse(char_bldg_sf == 0, 1, char_bldg_sf)) %>%
    step_mutate_at(
      starts_with("ind_"),
      starts_with("ccao_is"),
      fn = as.numeric
    ) %>%
    step_impute_median(all_numeric(), -all_outcomes(), -has_role("ID")) %>%
    # Replace novel levels with "new"
    step_novel(all_of(cat_vars), -has_role("ID")) %>%
    # Replace NA in factors with "unknown"
    step_unknown(all_of(cat_vars), -has_role("ID")) %>%
    # Perform categorical embedding
    embed::step_lencode_glm(
      all_of(cat_vars), -has_role("ID"),
      outcome = vars(meta_sale_price)
    ) %>%
    # Drop any predictors with near-zero variance, add interactions, and
    # perform transforms
    step_nzv(all_predictors()) %>%
    step_interact(terms = ~ meta_township_code * time_sale_day) %>%
    step_interact(terms = ~ meta_township_code * char_bldg_sf) %>%
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

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
#' @param log_outcome Logical. If TRUE, log-transform the outcome
#'   (`meta_sale_price`) inside the recipe via step_log(skip = TRUE), so the
#'   transform runs during prep()/fit() but is skipped at bake()/predict() time.
#'
#' @return A recipe object that can be used to clean model input data.
#'
model_main_recipe <- function(data, pred_vars, cat_vars, id_vars,
                              log_outcome = FALSE) {
  main_recipe <- recipe(data) %>%
    # Set the role of each variable in the input data
    update_role(meta_sale_price, new_role = "outcome") %>%
    update_role(all_of(pred_vars), new_role = "predictor") %>%
    update_role(all_of(id_vars), new_role = "ID") %>%
    update_role_requirements("ID", bake = FALSE) %>%
    update_role_requirements("NA", bake = FALSE) %>%
    # Remove any variables not an outcome var or in the pred_vars vector
    step_rm(any_of("time_split")) %>%
    step_rm(-all_outcomes(), -all_predictors(), -has_role("ID")) %>%
    # Replace novel levels with "new"
    step_novel(all_of(cat_vars), -has_role("ID")) %>%
    # Replace NA in factors with "unknown"
    step_unknown(all_of(cat_vars), -has_role("ID")) %>%
    # Convert factors to 0-indexed integers
    step_integer(
      all_of(cat_vars), -has_role("ID"),
      strict = TRUE, zero_based = TRUE
    )

  # Log-transform the outcome inside the recipe when enabled. skip = TRUE so the
  # step runs during prep()/fit() (the model trains on log price) but is skipped
  # at bake()/predict() time, where new data has no sale price. Predictions
  # therefore come back on the log scale -- see model_main_tailor() for the
  # inverse transform
  if (log_outcome) {
    main_recipe <- main_recipe %>%
      step_log(meta_sale_price, skip = TRUE)
  }

  main_recipe
}


#' Create the main model post-processing tailor
#'
#' @description Post-processing counterpart to model_main_recipe(). When the
#' log transform is enabled, the tailor exponentiates predictions, undoing the
#' step_log() applied to the outcome during training. When disabled, it leaves
#' predictions unchanged. A tailor is always created (even as a passthrough) so
#' that every consumer of the saved model can apply it unconditionally.
#'
#' Attaching the tailor to the workflow before tuning means tune applies it to
#' assessment set predictions before computing CV metrics. Since step_log() is
#' skipped at bake() time, the truth column stays in raw dollars, so metrics
#' compare dollars to dollars. The custom adjustment requires no estimation, so
#' no training data is sacrificed to an internal calibration split.
#'
#' @param log_outcome Logical. If TRUE, exponentiate predictions to convert
#'   them from log dollars back to raw dollars.
#'
#' @return A tailor object that can be added to a workflow with add_tailor().
#'
model_main_tailor <- function(log_outcome = FALSE) {
  post <- tailor::tailor()
  if (log_outcome) {
    post <- post %>%
      tailor::adjust_predictions_custom(.pred = exp(.pred))
  }
  post
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
    # Log-transform the outcome; the linear baseline always fits on log(price).
    # skip = TRUE so it runs during prep()/fit() but not bake()/predict().
    # Placed before step_lencode_glm() so its supervised encodings learn against
    # log(price). Predictions are exp()-ed back to dollars by the caller
    step_log(meta_sale_price, skip = TRUE) %>%
    # Drop extra location predictors that aren't school district
    step_rm(
      starts_with("loc_"),
      -all_numeric_predictors(),
      -starts_with("loc_school_")
    ) %>%
    # Convert logical values to numerics and get rid of 0s
    step_mutate(char_bldg_sf = ifelse(char_bldg_sf == 0, 1, char_bldg_sf)) %>%
    step_mutate_at(
      char_recent_renovation,
      time_sale_post_covid,
      starts_with("ind_"),
      starts_with("ccao_is"),
      fn = as.numeric
    ) %>%
    # Fill missing values with the median/mode
    step_impute_median(all_numeric_predictors(), -has_role("ID")) %>%
    step_impute_mode(all_nominal_predictors(), -has_role("ID")) %>%
    # Replace novel levels with "new"
    step_novel(all_nominal_predictors(), -has_role("ID")) %>%
    # Replace NA in factors with "unknown"
    step_unknown(all_nominal_predictors(), -has_role("ID")) %>%
    # Create linear encodings for certain high cardinality nominal predictors
    embed::step_lencode_glm(
      meta_nbhd_code,
      meta_township_code,
      char_class,
      starts_with("loc_school_"),
      outcome = vars(meta_sale_price)
    ) %>%
    # Dummify (OHE) any remaining nominal predictors
    step_dummy(
      all_nominal_predictors(),
      -meta_nbhd_code,
      -meta_township_code,
      -char_class,
      -starts_with("loc_school_"),
      -has_role("ID"),
      one_hot = TRUE
    ) %>%
    # Normalize/transform skewed numeric predictors. Add a small fudge factor
    # so no values are zero
    step_mutate(
      prox_nearest_vacant_land_dist_ft_1 =
        prox_nearest_vacant_land_dist_ft + 0.001,
      prox_nearest_new_construction_dist_ft_1 =
        prox_nearest_new_construction_dist_ft + 0.001,
      acs5_percent_employment_unemployed_1 =
        acs5_percent_employment_unemployed + 0.001
    ) %>%
    step_BoxCox(
      acs5_median_income_per_capita_past_year,
      acs5_median_income_household_past_year,
      char_bldg_sf, char_land_sf,
      prox_nearest_vacant_land_dist_ft_1,
      prox_nearest_new_construction_dist_ft_1,
      acs5_percent_employment_unemployed_1,
      acs5_median_household_renter_occupied_gross_rent
    ) %>%
    # Winsorize some extreme values in important numeric vars
    step_mutate_at(
      char_land_sf, char_bldg_sf,
      fn = \(x) pmin(pmax(x, quantile(x, 0.01)), quantile(x, 0.99))
    ) %>%
    step_poly(
      char_yrblt, char_bldg_sf, char_land_sf,
      degree = 2
    ) %>%
    # Normalize basically all numeric predictors
    step_normalize(
      meta_nbhd_code, meta_township_code, char_class,
      starts_with("char_yrblt"), starts_with("char_bldg_sf"),
      starts_with("char_land_sf"), starts_with("loc_"), starts_with("prox_"),
      starts_with("shp_"), starts_with("acs5_"), starts_with("other_"),
      -has_role("ID")
    ) %>%
    step_nzv(all_predictors())
}

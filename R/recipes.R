#' Create the main data cleaning Tidymodels recipe
#'
#' @description Main data pre-processing recipe. These steps are applied to the
#' training set and any new data fed to the model (using bake()). Goal of this
#' recipe is to create a clean matrix with few/no missing values, well-behaved
#' distributions, and cleaned up categorical variables
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
  recipe(meta_sale_price ~ ., data = data) %>%

    # Set some vars to role to "ID" so they're not included in the actual fit
    update_role(all_of(id_vars), new_role = "ID") %>%

    # Remove any variables not an outcome var or in the pred_vars vector
    step_rm(-all_of(pred_vars), -all_outcomes(), -has_role("ID")) %>%
    
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

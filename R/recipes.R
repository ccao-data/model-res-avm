#' Create the main data cleaning Tidymodels recipe
#'
#' @description Main data pre-processing recipe. These steps are applied to the
#' training set and any new data fed to the model (using bake()). Goal of this
#' recipe is to create a clean matrix with few/no missing values, well-behaved
#' distributions, and cleaned up categorical variables
#'
#' @param data A data frame containing the input data. Can contain extraneous
#'   columns and missing values.
#' @param keep_vars Character vector of column names to keep for modeling. These
#'   will be the right-hand side of the regression AKA predictors.
#' @param id_vars Character vector of ID variables. These can be kept in "baked"
#'   data without being treated as predictors.
#'
#' @return A recipe object that can be used to clean model input data.
#'
model_main_recipe <- function(data, keep_vars, id_vars) {
  recipe(meta_sale_price ~ ., data = data) %>%

    # Set some vars to role to "ID" so they're not included in the actual fit
    update_role(any_of(id_vars), new_role = "ID") %>%

    # Remove any variables not an outcome var or in the keep_vars vector
    step_rm(-any_of(keep_vars), -all_outcomes(), -has_role("ID")) %>%

    # Replace NA in factors with "unknown" 
    step_unknown(all_nominal(), -has_role("ID")) %>%
    
    # Condense rare factor levels into "other"
    step_other(all_nominal(), -has_role("ID"), threshold = 0.01) %>%
    
    step_novel(all_nominal(), -has_role("ID")) 
}


# Extra recipe step for converting categoricals to one-hot encoding. Only used
# in cases where categoricals are not handled natively such as linear models
dummy_recp_prep <- function(recipe) {
  recipe %>%
    step_mutate_at(starts_with("ind_"), fn = ~ as.numeric(.)) %>%
    step_dummy(all_nominal(), -all_outcomes(), -has_role("ID"))
}

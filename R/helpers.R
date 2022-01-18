#' Predict values using a trained model and recipe
#'
#' @description Simple helper function to return predictions from a new data set
#' given a parsnip specification and recipe. Will exponentiate predictions by
#' default.
#'
#' @param spec A parsnip model specification object. Must be trained.
#' @param recipe A prepped recipe object. Must be trained.
#' @param data New data to get predictions from. Will be pre-processed by the
#'   specified \code{recipe}.
#' @param exp Exponentiate the returned prediction (assumed to be natural log).
#'   Default TRUE.
#'
#' @return A vector of predictions from the model given the data and recipe
#'   specified.
#'
model_predict <- function(spec, recipe, data, exp = TRUE) {
  pred <- parsnip::predict.model_fit(
    object = spec,
    new_data = recipes::bake(recipe, data, recipes::all_predictors())
  )$.pred
  
  if (exp) pred <- exp(pred)
  
  return(pred)
}

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
model_predict <- function(spec, recipe, data, exp = FALSE) {
  pred <- parsnip::predict.model_fit(
    object = spec,
    new_data = recipes::bake(recipe, data, recipes::all_predictors())
  )$.pred
  
  if (exp) pred <- exp(pred)
  
  return(pred)
}


# Function to generate a dictionary of file names, local paths, and mirrored
# S3 location URIs
model_file_dict <- function(model_s3_bucket = NULL,
                            model_run_id = NULL,
                            model_assessment_year = NULL) {
  wd <- here::here("output")
  mpq <- paste0(model_run_id, ".parquet")
  list(
    "input" = list(
      "training" = list(
        "local" = here::here("input", "training_data.parquet"),
        "dvc" = here::here("input", "training_data.parquet.dvc")
      ),
      "assessment" = list(
        "local" = here::here("input", "training_data.parquet"),
        "dvc" = here::here("input", "training_data.parquet.dvc")
      )
    ),
    "output" = list(
      "assessment" = list(
        "local" = here::here(wd, "assessment", "model_assessment.parquet"),
        "s3" = file.path(
          model_s3_bucket, "assessment",
          paste0("year=", model_assessment_year), mpq
        )
      ),
      "metadata" = list(
        "local" = here::here(wd, "metadata", "model_metadata.parquet"),
        "s3" = file.path(
          model_s3_bucket, "metadata", 
          paste0("year=", model_assessment_year), mpq
        )
      ),
      "parameter" = list(
        "local" = here::here(wd, "parameter", "model_parameter.parquet"),
        "s3" = file.path(
          model_s3_bucket, "parameter", 
          paste0("year=", model_assessment_year), mpq
        ),
        "s3_raw" = file.path(
          model_s3_bucket, "parameter_raw", 
          paste0("year=", model_assessment_year), mpq
        )
      ),
      "performance" = list(
        "local" = here::here(wd, "performance", "model_performance.parquet"),
        "s3" = file.path(
          model_s3_bucket, "performance", 
          paste0("year=", model_assessment_year), mpq
        )
      ),
      "test" = list(
        "local" = here::here(wd, "performance", "test_data.parquet")
      ),
      "timing" = list(
        "local" = here::here(wd, "timing", "model_timing.parquet"),
        "s3" = file.path(
          model_s3_bucket, "timing", 
          paste0("year=", model_assessment_year), mpq
        )
      ),
      "workflow" = list(
        "fit" = list(
          "local" = here::here(wd, "workflow", "fit", "fit.zip"),
          "s3" = file.path(
            model_s3_bucket, "workflow", "fit",
            paste0("year=", model_assessment_year), paste0(model_run_id, ".zip")
          )
        ),
        "recipe" = list(
          "local" = here::here(wd, "workflow", "recipe", "recipe.rds"),
          "s3" = file.path(
            model_s3_bucket, "workflow", "recipe",
            paste0("year=", model_assessment_year), paste0(model_run_id, ".rds")
          )
        )
      )
    )
  )
}

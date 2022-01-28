# Function to translate the ratio study year and stage into a column name within
# used in the input data
get_rs_col_name <- function(data_year,  study_year,  study_stage) {
  stopifnot(study_stage %in% c("board",  "mailed",  "certified"))
  year_diff <- as.numeric(data_year) - as.numeric(study_year)
  time_prefix <- ifelse(
    year_diff > 0, 
    paste0(as.integer(year_diff),  "yr_pri"), 
    NA_character_
  )
  
  paste(na.omit(c("meta",  time_prefix,  study_stage,  "tot")),  collapse = "_")
}


# Function to generate a dictionary of file names,  local paths,  and mirrored
# S3 location URIs
model_file_dict <- function(model_s3_bucket = NULL, 
                            model_run_id = NULL, 
                            model_assessment_year = NULL) {
  wd <- here::here("output")
  mpq <- paste0(model_run_id,  ".parquet")
  list(
    "input" = list(
      "training" = list(
        "local" = here::here("input",  "training_data.parquet"), 
        "dvc" = here::here("input",  "training_data.parquet.dvc")
      ), 
      "assessment" = list(
        "local" = here::here("input",  "assessment_data.parquet"), 
        "dvc" = here::here("input",  "assessment_data.parquet.dvc")
      )
    ), 
    "output" = list(
      "metadata" = list(
        "local" = here::here(wd,  "metadata",  "model_metadata.parquet"), 
        "s3" = file.path(
          model_s3_bucket,  "metadata",  
          paste0("year=",  model_assessment_year),  mpq
        )
      ), 
      "parameter_search" = list(
        "local" = here::here(
          wd,  "parameter_search",  "model_parameter_search.parquet"
        ), 
        "s3" = file.path(
          model_s3_bucket,  "parameter_search",  
          paste0("year=",  model_assessment_year),  mpq
        ), 
        "s3_raw" = file.path(
          model_s3_bucket,  "parameter_raw",  
          paste0("year=",  model_assessment_year),  mpq
        )
      ),
      "parameter_range" = list(
        "local" = here::here(
          wd, "parameter_range", "model_parameter_range.parquet"
        ),
        "s3" = file.path(
          model_s3_bucket,  "parameter_range",  
          paste0("year=",  model_assessment_year),  mpq
        )
      ),
      "parameter_final" = list(
        "local" = here::here(
          wd, "parameter_final",  "model_parameter_final.parquet"
        ), 
        "s3" = file.path(
          model_s3_bucket,  "parameter_final",  
          paste0("year=",  model_assessment_year),  mpq
        )
      ), 
      "performance" = list(
        "test" = list(
          "local" = here::here(
            wd,  "performance",  "test_performance.parquet"
          ), 
          "s3" = file.path(
            model_s3_bucket,  "performance",  
            paste0("year=",  model_assessment_year),  "stage=test",  mpq
          )
        ), 
        "assessment" = list(
          "local" = here::here(
            wd,  "performance",  "assessment_performance.parquet"
          ), 
          "s3" = file.path(
            model_s3_bucket,  "performance",  
            paste0("year=",  model_assessment_year),  "stage=assessment",  mpq
          )
        )
      ), 
      "data" = list(
        "test" = list(
          "local" = here::here(wd,  "data",  "test_data.parquet")
        ),
        "assessment" = list(
          "local" = here::here(wd,  "data",  "assessment_data.parquet")
        )
      ),
      "timing" = list(
        "local" = here::here(wd,  "timing",  "model_timing.parquet"), 
        "s3" = file.path(
          model_s3_bucket,  "timing",  
          paste0("year=",  model_assessment_year),  mpq
        )
      ), 
      "workflow" = list(
        "fit" = list(
          "local" = here::here(wd,  "workflow",  "fit",  "fit.zip"), 
          "s3" = file.path(
            model_s3_bucket,  "workflow",  "fit", 
            paste0("year=",  model_assessment_year),  paste0(model_run_id,  ".zip")
          )
        ), 
        "recipe" = list(
          "local" = here::here(wd,  "workflow",  "recipe",  "recipe.rds"), 
          "s3" = file.path(
            model_s3_bucket,  "workflow",  "recipe", 
            paste0("year=",  model_assessment_year),  paste0(model_run_id,  ".rds")
          )
        )
      )
    )
  )
}


# Used to delete erroneous, incomplete, or otherwise broken runs
# Use with caution!
model_delete_run <- function(model_s3_bucket, 
                             model_run_id, 
                             model_assessment_year) {
  
  paths <- model_file_dict(
    file.path(model_s3_bucket, "model"),
    model_run_id,
    model_assessment_year
  )
  
  s3_objs <- grep("s3://", unlist(paths), value = TRUE)
  purrr::walk(s3_objs, aws.s3::delete_object)
}

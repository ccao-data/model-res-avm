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
model_file_dict <- function(s3_bucket = NULL, run_id = NULL, year = NULL) {
  wd <- here::here("output")

  readr::
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

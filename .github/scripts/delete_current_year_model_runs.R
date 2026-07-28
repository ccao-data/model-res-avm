# Script to delete a list of model runs by ID from AWS.
#
# Accepts one argument, a comma-delimited list of run IDs for model runs
# whose artifacts should be deleted.
#
# Assumes that model runs are restricted to the current assessment cycle, where
# each assessment cycle starts in May. Raises an error if no objects matching
# a given ID for the current year could be located in S3. This error will get
# raised before any deletion occurs, so if one or more IDs are invalid then
# no objects will be deleted.
#
# Example usage (delete the runs 123, 456, and 789 in the current year):
#
#   delete_current_year_model_runs.R 123,456,789

suppressPackageStartupMessages({
  library(glue)
  library(here)
  library(magrittr)
})
source(here("R", "helpers.R"))

current_date <- as.POSIXct(Sys.Date())
current_month <- current_date %>% format("%m")
current_year <- current_date %>% format("%Y")

# The following heuristic determines the current upcoming assessment cycle year:
#
#   * From May to December (post assessment), `year` = next year
#   * From January to April (during assessment), `year` = current year
year <- if (current_month < "05") {
  current_year
} else {
  as.character(as.numeric(current_year) + 1)
}

# Convert the comma-delimited input to a vector of run IDs. Accepting one or
# more positional arguments would be a cleaner UX, but since this script is
# intended to be called from a dispatched GitHub workflow, it's easier to parse
# one comma-delimited string than convert a space-separated string passed as a
# workflow input to an array of function arguments
raw_run_ids <- commandArgs(trailingOnly = TRUE)
run_ids <- raw_run_ids %>%
  strsplit(split = ",", fixed = TRUE) %>%
  unlist()

"Confirming artifacts exist for run IDs in year {year}: {raw_run_ids}" %>%
  glue::glue() %>%
  print()

# We consider a run ID to be valid if it has any matching data in S3 for
# the current year
run_id_is_valid <- function(run_id, year) {
  return(
    model_get_s3_artifacts_for_run(run_id, year) %>%
      sapply(aws.s3::object_exists) %>%
      any()
  )
}

# We check for validity separate from the deletion operation for two reasons:
#
#   1. The aws.s3::delete_object API does not raise an error if an object does
#      not exist, so a delete operation alone won't alert us for an incorrect
#      ID
#   2. Even if aws.s3::delete_object could raise an error for missing objects,
#      we want to alert the caller that one or more of the IDs were incorrect
#      before deleting any objects so that this script is nondestructive
#      in the case of a malformed ID
valid_run_ids <- run_ids %>% sapply(run_id_is_valid, year = year)

if (!all(valid_run_ids)) {
  invalid_run_ids <- run_ids[which(valid_run_ids == FALSE)] %>%
    paste(collapse = ", ")

  "Some run IDs are missing all S3 artifacts for {year}: {invalid_run_ids}" %>%
    glue::glue() %>%
    stop()
}

"Deleting S3 artifacts run IDs in year {year}: {run_ids}" %>%
  glue::glue() %>%
  print()

run_ids %>%
  purrr::walk(~ model_delete_run(run_id = .x, year = year))

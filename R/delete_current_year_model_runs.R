# Script to delete a list of model runs by ID from AWS.
#
# Accepts an arbitrary number of arguments, each of which should be the run ID
# of a model run whose artifacts should be deleted.
#
# Assumes that model runs are restricted to the current assessment cycle, where
# each assessment cycle starts in April. Raises an error if no objects matching
# a given ID for the current year could be located in S3. This error will get
# raised before any deletion occurs, so if one or more IDs are invalid then
# no objects will be deleted.
#
# Example usage:
#
#   delete_current_year_model_runs.R 123 456 789

library(glue)
library(here)
library(magrittr)
source(here("R", "helpers.R"))

# Function to check whether S3 artifacts exist for a given model run.
# Defining this as a separate check from the deletion operation is helpful for
# two reasons:
#
#   1. The aws.s3::delete_object API does not raise an error if an object does
#      not exist, so a delete operation alone won't alert us for an incorrect
#      ID
#   2. Even if aws.s3::delete_object could raise an error for missing objects,
#      we want to alert the caller that one or more of the IDs were incorrect
#      before deleting any objects so that this script is nondestructive
#      in the case of a malformed ID
raise_if_run_id_is_invalid <- function(run_id, year) {
  artifacts_exist <- model_get_s3_artifacts_for_run(run_id, year) %>%
    sapply(aws.s3::object_exists)

  if (!any(artifacts_exist)) {
    "Model run {run_id} for year {year} is missing all S3 artifacts" %>%
      glue::glue() %>%
      stop()
  }
}

current_date <- as.POSIXct(Sys.Date())
current_month <- current_date %>% format("%m")
current_year <- current_date %>% format("%Y")

# The following heuristic determines the current upcoming assessment cycle year:
#
#   * From April to December (post assessment), `year` = next year
#   * From January to March (during assessment), `year` = current year
year <- if (current_month < "03") {
  current_year
} else {
  as.character(as.numeric(current_year) + 1)
}

run_ids <- commandArgs(trailingOnly = TRUE)

"Confirming artifacts exist for run IDs in year {year}: {run_ids}" %>%
  glue::glue() %>%
  print()

# For a future improvement, it would probably be more user friendly to catch
# the missing artifact errors raised by raise_if_run_id_is_invalid and compile
# a list of all invalid run IDs before raising
run_ids %>% sapply(raise_if_run_id_is_invalid, year = year)

"Deleting S3 artifacts run IDs in year {year}: {run_ids}" %>%
  glue::glue() %>%
  print()

run_ids %>% sapply(model_delete_run, year = year)

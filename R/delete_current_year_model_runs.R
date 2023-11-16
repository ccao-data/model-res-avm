# Script to delete a list of model runs by ID from AWS.
#
# Assumes that model runs are restricted to the current assessment cycle, where
# each assessment cycle starts in April.
#
# Raises an error if no objects matching the given ID were deleted.

library(glue)
library(here)
library(magrittr)
source(here("R", "helpers.R"))

# Slightly altered version of model_delete_run from helpers.R that raises an
# error if no objects were deleted
delete_run <- function(run_id, year) {
  deleted_objs <- model_delete_run(run_id, year)
  if (length(deleted_objs) == 0) {
    error_msg <- "No objects match the run ID '{run_id}' for year {year}"
    error_msg %>%
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

log_msg <- "Deleting run IDs for year {year}: {run_ids}"
log_msg %>%
  glue::glue() %>%
  print()

run_ids %>% sapply(delete_run, year = year)

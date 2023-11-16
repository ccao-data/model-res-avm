# Script to delete a list of model runs by ID from AWS.
#
# Assumes that model runs are restricted to the current assessment cycle, where
# each assessment cycle starts in April.

library(here)
library(magrittr)
source(here("R", "helpers.R"))

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

sprintf("Deleting run IDs for year %s: %s", year, run_ids)

run_ids %>% sapply(model_delete_run, year = year)

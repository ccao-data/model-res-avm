# Script to tag a list of model runs by ID from AWS.
#
# Accepts two arguments, a comma-delimited list of run IDs for model runs and
# a new run_type which to apply to those runs. See delete script for more
# details on the script implementation

suppressPackageStartupMessages({
  library(glue)
  library(here)
  library(magrittr)
})
source(here("R", "helpers.R"))

current_date <- as.POSIXct(Sys.Date())
current_month <- current_date %>% format("%m")
current_year <- current_date %>% format("%Y")

year <- if (current_month < "05") {
  current_year
} else {
  as.character(as.numeric(current_year) + 1)
}

# Convert the comma-delimited input to a vector of run IDs
raw_args <- commandArgs(trailingOnly = TRUE)
run_type <- raw_args[[2]]
run_ids <- raw_args[[1]] %>%
  strsplit(split = ",", fixed = TRUE) %>%
  unlist()

"Confirming artifacts exist for run IDs in year {year}: {run_ids}" %>%
  glue::glue() %>%
  print()

# We consider a run ID to be valid if it has any matching data in S3 for
# the current year
run_id_is_valid <- function(run_id, year) {
  artifacts <- model_get_s3_artifacts_for_run(run_id, year)
  artifacts <- artifacts[stringr::str_detect(artifacts, "^.*metadata.*$")]
  output <- artifacts %>%
    sapply(aws.s3::object_exists) %>%
    all()
  return(output)
}

# Check for validity of the tag operation
valid_run_ids <- run_ids %>% sapply(run_id_is_valid, year = year)

if (!all(valid_run_ids)) {
  invalid_run_ids <- run_ids[which(valid_run_ids == FALSE)] %>%
    paste(collapse = ", ")

  "Some run IDs are missing all S3 artifacts for {year}: {invalid_run_ids}" %>%
    glue::glue() %>%
    stop()
}

"Tagging runs with type \"{run_type}\" in year {year}: {run_ids}" %>%
  glue::glue() %>%
  print()

run_ids %>%
  purrr::walk(~ model_tag_run(run_id = .x, year = year, run_type = run_type))

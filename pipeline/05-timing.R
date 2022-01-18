# Load the necessary libraries
library(arrow)
library(dplyr)
library(stringr)
library(tidyr)
library(tictoc)
source(here("R", "helpers.R"))

# Initialize a dictionary of file paths and URIs. See R/helpers.R
paths <- model_file_dict()

# Convert input timing logs to data frame, then save to file
if (exists("model_run_id") & exists("model_run_start_timestamp")) {
  dplyr::bind_rows(tictoc::tic.log(format = FALSE)) %>%
    dplyr::mutate(
      run_id = model_run_id,
      run_start_timestamp = model_run_start_timestamp,
      elapsed = toc - tic,
      stage = paste0(tolower(word(msg, 1)), "_sec_elapsed")
    ) %>%
    dplyr::select(-c(tic:toc, msg)) %>%
    tidyr::pivot_wider(
      id_cols = c(run_id, run_start_timestamp),
      names_from = stage,
      values_from = elapsed
    ) %>%
    arrow::write_parquet(paths$output$timing$local)
  
  # Clear any logs from tictoc
  tictoc::tic.clearlog()
}

# Load the necessary libraries
library(arrow)
library(dplyr)
library(here)
library(stringr)
library(tidyr)
library(tictoc)
source(here("R", "helpers.R"))

# Initialize a dictionary of file paths and URIs. See R/helpers.R
paths <- model_file_dict()

# Convert input timing logs to data frame, then save to file
if (file.exists(paths$output$metadata$local) &
    file.exists(paths$output$timing$local)) {
  
  # Load info from the saved metadata file to append run ID and start time
  metadata <- read_parquet(paths$output$metadata$local)
  
  # Load the built timing file and munge it into a more useful format
  read_parquet(paths$output$timing$local) %>%
    mutate(
      run_id = metadata$run_id[1],
      run_start_timestamp = metadata$run_start_timestamp[1],
      elapsed = round(toc - tic, 2),
      stage = paste0(tolower(word(msg, 1)), "_sec_elapsed")
    ) %>%
    select(-c(tic:toc, msg)) %>%
    tidyr::pivot_wider(
      id_cols = c(run_id, run_start_timestamp),
      names_from = stage,
      values_from = elapsed
    ) %>%
    mutate(overall_sec_elapsed = rowSums(across(ends_with("_sec_elapsed")))) %>%
    write_parquet(paths$output$timing$local)
  
  # Clear any remaining logs from tictoc
  tictoc::tic.clearlog()
}

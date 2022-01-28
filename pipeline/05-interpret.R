# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Setup ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Start the script timer and clear logs from prior script
tictoc::tic.clearlog()
tictoc::tic("Interpret")

# Load the necessary libraries
options(scipen = 99)
library(arrow)
library(dplyr)
library(fastshap)
library(here)
library(recipes)
library(stringr)
library(tidyr)
library(tictoc)
source(here("R", "helpers.R"))

# Initialize a dictionary of file paths and URIs. See R/helpers.R
paths <- model_file_dict()

# Load the final lightgbm model object and recipe from file
lgbm_final_full_fit <- lightsnip::lgbm_load(paths$output$workflow$fit$local)
lgbm_final_full_recipe <- readRDS(paths$output$workflow$recipe$local)


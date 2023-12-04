# Set various global options for each pipeline stage
options(
  tidymodels.dark = TRUE, # Tidymodels CV output compatible with dark theme
  dplyr.summarise.inform = FALSE, # Disable messages when aggregating
  scipen = 99, # Disable scientific notation
  knitr.kable.NA = "", # Replace NA with empty string in knitr::kable
  java.parameters = "-Xmx10g" # Pre-allocate memory for Java JDBC driver
)

# Load R pipeline dependencies quietly (no startup messages). This gets called
# by purrr::walk for each pipeline stage
suppressPackageStartupMessages({
  library(arrow)
  library(assessr)
  library(aws.s3)
  library(aws.ec2metadata)
  library(butcher)
  library(ccao)
  library(conflicted)
  library(dplyr)
  library(furrr)
  library(git2r)
  library(glue)
  library(here)
  library(knitr)
  library(parsnip)
  library(purrr)
  library(lightgbm)
  library(lightsnip)
  library(lubridate)
  library(paws.analytics)
  library(paws.application.integration)
  library(recipes)
  library(rlang)
  library(rsample)
  library(stringr)
  library(tictoc)
  library(tidyr)
  library(tune)
  library(workflows)
  library(yaml)
  library(yardstick)
})

# Resolve package namespace conflicts, preferring the library::function pair
# shown over other functions with the same name from different libraries
conflicts_prefer(
  dplyr::filter,
  dplyr::lag,
  dplyr::pull,
  dplyr::slice,
  lubridate::duration,
  purrr::flatten,
  purrr::set_names,
  purrr::splice,
  purrr::when,
  recipes::step,
  rlang::is_empty,
  tictoc::push,
  utils::timestamp,
  .quiet = TRUE
)

# Load helpers for model dictionary, data loading, and other misc functions
source(here::here("R", "helpers.R"))

# Initialize a dictionary of file paths. See misc/file_dict.csv for details
paths <- model_file_dict()

# Load the parameters file containing the run settings
params <- read_yaml("params.yaml")

# Get the number of available physical cores to use for multi-threading
# Lightgbm docs recommend using only real cores, not logical
# https://lightgbm.readthedocs.io/en/latest/Parameters.html#num_threads
num_threads <- parallel::detectCores(logical = FALSE)

# Set the overall model seed
set.seed(params$model$seed)

# Override CV toggle, SHAP toggle, and run_type set in params.yaml.
# Used to disable certain features for CI or limited runs
cv_enable <- as.logical(
  Sys.getenv("CV_ENABLE_OVERRIDE", unset = params$toggle$cv_enable)
)
shap_enable <- as.logical(
  Sys.getenv("SHAP_ENABLE_OVERRIDE", unset = params$toggle$shap_enable)
)
run_type <- as.character(
  Sys.getenv("RUN_TYPE_OVERRIDE", unset = params$run_type)
)

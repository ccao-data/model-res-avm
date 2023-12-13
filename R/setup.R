# Set various global options for each pipeline stage
options(
  tidymodels.dark = TRUE, # Tidymodels CV output compatible with dark theme
  dplyr.summarise.inform = FALSE, # Disable messages when aggregating
  scipen = 99, # Disable scientific notation
  knitr.kable.NA = "", # Replace NA with empty string in knitr::kable
  java.parameters = "-Xmx10g" # Pre-allocate memory for Java JDBC driver
)

# Load R pipeline dependencies from the Depends: key in the DESCRIPTION file.
# Any new pipeline dependencies should be added there
suppressPackageStartupMessages({
  purrr::walk(
    strsplit(yaml::read_yaml("DESCRIPTION")$Depends, ", ")[[1]],
    library,
    character.only = TRUE
  )
})

# Resolve package namespace conflicts, preferring the library::function pair
# shown over other functions with the same name from different libraries
conflicts_prefer(
  dplyr::filter,
  dplyr::first,
  dplyr::lag,
  dplyr::pull,
  dplyr::slice,
  lubridate::duration,
  purrr::flatten,
  purrr::is_empty,
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

# Override CV toggle, SHAP toggle, and S3 upload set in params.yaml.
# Used to disable certain features for CI or limited runs
cv_enable <- as.logical(
  Sys.getenv("CV_ENABLE_OVERRIDE", unset = params$toggle$cv_enable)
)
shap_enable <- as.logical(
  Sys.getenv("SHAP_ENABLE_OVERRIDE", unset = params$toggle$shap_enable)
)
upload_enable <- as.logical(
  Sys.getenv("UPLOAD_ENABLE_OVERRIDE", unset = params$toggle$upload_enable)
)

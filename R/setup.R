# Set various global options for each pipeline stage
options(
  tidymodels.dark = TRUE, # Tidymodels CV output compatible with dark theme
  dplyr.summarise.inform = FALSE, # Disable messages when aggregating
  scipen = 99, # Disable scientific notation
  knitr.kable.NA = "" # Replace NA with empty string in knitr::kable
)

# Load R pipeline dependencies from the Depends: key in the DESCRIPTION file.
# Any new pipeline dependencies should be added there
suppressPackageStartupMessages({
  purrr::walk(
    strsplit(yaml::read_yaml(here::here("DESCRIPTION"))$Depends, ", ")[[1]],
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
  glue::glue,
  lubridate::duration,
  purrr::discard,
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

# Load the parameters file containing the run settings. The conditional here
# detects rendering in a Quarto/RMarkdown document and assigns to `model_params`
# instead, since `params` is a reserved variable name in Quarto
if (knitr::is_html_output() || knitr::is_latex_output()) {
  params_obj_name <- "model_params"
} else if (exists("params")) {
  if (exists("run_id", where = params)) params_obj_name <- "model_params"
} else {
  params_obj_name <- "params"
}
assign(params_obj_name, read_yaml(here("params.yaml")))

# Set the random seed for reproducibility. This is currently used in two places:
# 1. Within Tidymodels workflows whenever sampling is used e.g. for KNN impute
# 2. Within the CV loop to make random folds that are consistent across runs
# Note that LightGBM uses its own set of seeds i.e. this seed doesn't affect
# whether the model itself is deterministic
set.seed(get(params_obj_name)$model$seed)

# Get the number of available physical cores to use for multi-threading
# Lightgbm docs recommend using only real cores, not logical
# https://lightgbm.readthedocs.io/en/latest/Parameters.html#num_threads
num_threads <- parallel::detectCores(logical = FALSE)

# Override CV toggle, SHAP toggle, and S3 upload set in params.yaml.
# Used to disable certain features for CI or limited runs
cv_enable <- as.logical(Sys.getenv(
  "CV_ENABLE_OVERRIDE",
  unset = get(params_obj_name)$toggle$cv_enable
))
shap_enable <- as.logical(Sys.getenv(
  "SHAP_ENABLE_OVERRIDE",
  unset = get(params_obj_name)$toggle$shap_enable
))
comp_enable <- as.logical(Sys.getenv(
  "COMP_ENABLE_OVERRIDE",
  unset = get(params_obj_name)$toggle$comp_enable
))
upload_enable <- as.logical(Sys.getenv(
  "UPLOAD_ENABLE_OVERRIDE",
  unset = get(params_obj_name)$toggle$upload_enable
))

feature_report_enable <- as.logical(Sys.getenv(
  "FEATURE_REPORT_ENABLE_OVERRIDE",
  unset = get(params_obj_name)$toggle$feature_report_enable
))

# If in a CI context, use the run params passed to the workflow. Otherwise, use
# the note included in params.yaml
run_note <- as.character(
  Sys.getenv("WORKFLOW_RUN_NOTE", unset = get(params_obj_name)$run_note)
)
run_type <- as.character(
  Sys.getenv("WORKFLOW_RUN_TYPE", unset = get(params_obj_name)$run_type)
)

# Must be one of the dedicated run types
possible_run_types <- c(
  "junk", "rejected", "test",
  "baseline", "candidate", "final",
  "comps"
)
if (!run_type %in% possible_run_types) {
  stop(
    "Invalid run type '", run_type, "'. Must be one of: ",
    paste0(possible_run_types, collapse = ", ")
  )
}
rm(possible_run_types)

# Whether to log-transform the sale price target before fitting. Defines model
# structure (see `model.log_sale_price` in params.yaml), not a pipeline toggle
log_transform_enable <- as.logical(
  get(params_obj_name)$model$log_sale_price
)

# The "mse_cov" objective is only built for logged sale prices. An un-logged
# configuration exists in the reference Python implementation (the "div"
# ratio mode) but is not built into our codebase; if there is interest, see
# the author's repo or working paper:
# https://github.com/nicacevedo/soft-vertical-equity-constrained-mass-appraissal
if (identical(get(params_obj_name)$model$objective, "mse_cov") &&
  !log_transform_enable) { # nolint
  stop(
    "model.objective = 'mse_cov' requires model.log_sale_price = true, ",
    "since the objective is only built for logged sale prices"
  )
}

# Check to see if LightGBM early stopping is enabled based on engine parameters
early_stopping_enable <-
  get(params_obj_name)$model$parameter$validation_prop > 0 &&
    !is.null(get(params_obj_name)$model$parameter$stop_iter) && # nolint
    get(params_obj_name)$model$parameter$stop_iter > 0 # nolint

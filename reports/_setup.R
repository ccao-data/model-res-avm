# This setup script is run at the top of each Quarto report subsection to load
# libraries, data, and other objects needed for the report. It only loads
# objects if they don't already exist in the environment, so it can be run
# idempotently to avoid reloading data sets and libraries

## Libraries and paths ---------------------------------------------------------
library(purrr)
library(here)

# Load list of helper files and main libraries
purrr::walk(list.files(here::here("R"), "\\.R$", full.names = TRUE), source)

# Load reporting-only R libraries
suppressPackageStartupMessages({
  reporting_libs <- "Config/renv/profiles/reporting/dependencies"
  purrr::walk(
    strsplit(read_yaml(here::here("DESCRIPTION"))[[reporting_libs]], ", ")[[1]],
    library,
    character.only = TRUE
  )
})

# TODO: Catch for weird Arrow bug with SIGPIPE. Need to permanently fix later
# https://github.com/apache/arrow/issues/32026
cpp11::cpp_source(code = "
#include <csignal>
#include <cpp11.hpp>

[[cpp11::register]] void ignore_sigpipes() {
  signal(SIGPIPE, SIG_IGN);
}
")

ignore_sigpipes()

# Initialize a dictionary of file paths. See misc/file_dict.csv for details
paths <- model_file_dict(model_params$run_id, model_params$year)


## Input data ------------------------------------------------------------------

# Ingest training set used for this run from DVC bucket (if not local)
if (!exists("training_data")) {
  training_data <- read_parquet(paths$input$training$local)
}

# Load assessment set used for this run
if (!exists("assessment_data")) {
  assessment_data <- read_parquet(paths$input$assessment$local)
}

# Load Home Improvement Exemption data
if (!exists("hie_data")) {
  hie_data <- read_parquet(paths$input$hie$local)
}

# Load characteristics data
if (!exists("chars_data")) {
  chars_data <- read_parquet(paths$input$char$local)
}

# Load characteristics data
if (!exists("complex_id_data")) {
  complex_id_data <- read_parquet(paths$input$complex_id$local)
}


## Output data -----------------------------------------------------------------

# Grab metadata to check output data <> params alignment
metadata <- read_parquet(paths$output$metadata$local)
if (metadata$run_id != params$run_id) {
  stop(
    "Local run outputs are NOT equal to the requested run_id. You ",
    "should run model_fetch_run() to fetch model outputs from S3"
  )
}

# Extract hash values from dvc.lock file
dvc_lock_values <- sapply(
  read_yaml(here::here("dvc.lock"))$stages$ingest$outs,
  function(x) x$md5
)

metadata_dvc_md5 <- metadata %>%
  select(starts_with("dvc_md5"))

# Compare hash for each metadata value with the corresponding dvc.lock value
comparison_results <- purrr::map2_lgl(
  metadata_dvc_md5,
  dvc_lock_values,
  ~ .x == .y
)

# Check if all hashes match between metadata and dvc.lock
if (!all(comparison_results)) {
  stop(
    "Hash values between the dvc.lock file and the metadata ",
    "do not match. Potential mismatch in input and output data."
  )
}

# Get the triad of the run to use for filtering
run_triad <- tools::toTitleCase(metadata$assessment_triad)
run_triad_code <- ccao::town_dict %>%
  filter(triad_name == run_triad) %>%
  distinct(triad_code) %>%
  pull(triad_code)

# Load model object and recipe
model_fit <- lightsnip::lgbm_load(paths$output$workflow_fit$local)
model_recipe <- readRDS(paths$output$workflow_recipe$local)

# Load model-generated output data sets
if (!exists("assessment_card")) {
  assessment_card <- read_parquet(paths$output$assessment_card$local)
}
if (!exists("assessment_pin")) {
  assessment_pin <- read_parquet(paths$output$assessment_pin$local)
}
if (!exists("test_card")) {
  test_card <- read_parquet(paths$output$test_card$local)
}
if (!exists("model_performance_test")) {
  model_performance_test <-
    arrow::read_parquet(paths$output$performance_test$local)
}
if (!exists("model_performance_test_linear")) {
  model_performance_test_linear <-
    arrow::read_parquet(paths$output$performance_test_linear$local)
}
if (!exists("model_performance_quantile_test")) {
  # nolint start: object_length_linter
  model_performance_quantile_test <-
    arrow::read_parquet(paths$output$performance_quantile_test$local)
}
if (!exists("model_performance_quantile_test_linear")) {
  model_performance_quantile_test_linear <-
    arrow::read_parquet(paths$output$performance_quantile_test_linear$local)
  # nolint end
}
if (!exists("model_performance_assessment")) {
  model_performance_assessment <-
    arrow::read_parquet(paths$output$performance_assessment$local)
}
if (!exists("model_parameter_final")) {
  model_parameter_final <-
    arrow::read_parquet(paths$output$parameter_final$local)
}
if (!exists("feat_imp_df")) {
  feat_imp_df <- read_parquet(paths$output$feature_importance$local)
}

# Load SHAP data if it exists (only exists for important runs)
if (file.exists(paths$output$shap$local) && metadata$shap_enable) {
  shap_df <- read_parquet(paths$output$shap$local)
  shap_exists <- nrow(shap_df) > 0
} else {
  shap_exists <- FALSE
}

# Load comp data if it exists
if (file.exists(paths$output$comp$local) && metadata$comp_enable) {
  comp_df <- read_parquet(paths$output$comp$local)
  comp_exists <- nrow(comp_df) > 0
} else {
  comp_exists <- FALSE
}

# Add colors to re-use across plots
plot_colors <- list(
  "sales" = "#66c2a5",
  "main" = "#5773b3",
  "linear" = "#fc8d62",
  "met" = "#d3f2c2",
  "bg_line" = "#2C3E50"
)

# Helper function to help with x-axis tick labels
shorten_number <- function(x) {
  scales::dollar(x, accuracy = 1, scale = 1 / 1000, suffix = "K")
}

# Chunk to populate the metadata / dataset summaries in the text of each module
# Anything prefixed with m_ is a variable that will be used directly in the text
m_test_min_date <- min(test_card$meta_sale_date)
m_test_max_date <- max(test_card$meta_sale_date)
m_test_n_sales <- test_card %>%
  nrow() %>%
  scales::comma()
m_test_n_sales_triad <- test_card %>%
  filter(meta_triad_code == run_triad_code) %>%
  nrow() %>%
  scales::comma()
m_test_n_sales_prop <- (
  nrow(filter(test_card, meta_triad_code == run_triad_code)) /
    nrow(test_card)
) %>%
  scales::percent(accuracy = 0.01)
m_test_med_sp <- test_card$meta_sale_price %>%
  median() %>%
  scales::dollar()
m_test_split_prop <- scales::percent(
  1 - metadata$cv_split_prop,
  accuracy = 0.01
)

m_train_min_date <- min(training_data$meta_sale_date)
m_train_max_date <- max(training_data$meta_sale_date)
m_train_n_sales <- training_data %>%
  nrow() %>%
  scales::comma()
m_train_n_sales_triad <- training_data %>%
  filter(meta_triad_code == run_triad_code) %>%
  nrow() %>%
  scales::comma()
m_train_n_sales_prop <- (
  nrow(filter(training_data, meta_triad_code == run_triad_code)) /
    nrow(training_data)
) %>%
  scales::percent(accuracy = 0.01)
m_train_med_sp <- training_data$meta_sale_price %>%
  median() %>%
  scales::dollar()
m_train_n_outliers <- training_data$sv_is_outlier %>%
  sum() %>%
  scales::comma()
m_train_n_outliers_prop <-
  ((sum(training_data$sv_is_outlier) / nrow(training_data))) %>%
  scales::percent(accuracy = 0.01)

m_assess_min_date <- min(assessment_pin$sale_ratio_study_date, na.rm = TRUE)
m_assess_max_date <- max(assessment_pin$sale_ratio_study_date, na.rm = TRUE)
m_assess_n_sales <- table(is.na(assessment_pin$sale_ratio_study_price))[1] %>%
  scales::comma()
m_assess_med_sp <- assessment_pin$sale_ratio_study_price %>%
  median(na.rm = TRUE) %>%
  scales::dollar()
m_assess_stage_far <- paste(
  metadata$ratio_study_far_year,
  metadata$ratio_study_far_stage
)
m_assess_stage_near <- paste(
  metadata$ratio_study_near_year,
  metadata$ratio_study_near_stage
)

options(knitr.kable.NA = "", scipen = 99, width = 150)
noctua::noctua_options(cache_size = 10)

# Load necessary libraries
library(arrow)
library(ccao)
library(brio)
library(DBI)
library(decor)
library(desc)
library(dplyr)
library(DT)
library(ggplot2)
library(glue)
library(grid)
library(gridExtra)
library(gtools)
library(here)
library(htmltools)
library(kableExtra)
library(leaflet)
library(noctua)
library(plotly)
library(purrr)
library(recipes)
library(scales)
library(sf)
library(skimr)
library(stringr)
library(tableone)
library(tidyr)
library(tools)
source(here("R", "helpers.R"))

# TODO: Catch for weird arrow bug with SIGPIPE. Need to permanently fix later
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
paths <- model_file_dict(params$run_id, params$year)

# Grab metadata to check input alignment
metadata <- read_parquet(paths$output$metadata$local)
if (metadata$run_id != params$run_id) {
  stop(
    "Local run outputs are NOT equal to the requested run_id. You ",
    "should run model_fetch_run() to fetch model outputs from S3"
  )
}

# Get the triad of the run to use for filtering
run_triad <- tools::toTitleCase(metadata$assessment_triad)
run_triad_code <- ccao::town_dict %>%
  filter(triad_name == run_triad) %>%
  distinct(triad_code) %>%
  pull(triad_code)

# Ingest training set used for this run from DVC bucket (if not local)
training_data <- read_parquet(paths$input$training$local)

# Load model-generated output data sets
assessment_card <- read_parquet(paths$output$assessment_card$local)
assessment_pin <- read_parquet(paths$output$assessment_pin$local)

# Load SHAP and feature importance data
shap_exists <- file.exists(paths$output$shap$local)
if (shap_exists) {
  shap_df <- read_parquet(paths$output$shap$local)
}
feat_imp_df <- read_parquet(paths$output$feature_importance$local)

# This script runs balance tests comparing the universe of residential parcels
# to the sample of residential parcels with sales in the previous 8 years
# that are used to train the data department's CAMA.

# Connect to Athena
AWS_ATHENA_CONN_NOCTUA <- dbConnect(noctua::athena())

# Declare time frame for study
time_frame <-
  c("min" = as.character(as.numeric(params$year) - 1), "max" = params$year)

# GATHER DATA ----

sales <- arrow::read_parquet(paths$input$training$local)

# All residential parcel characteristics from time frame, with a sale indicator
res_chars <- dbGetQuery(
  conn = AWS_ATHENA_CONN_NOCTUA,
  glue(
    "
  SELECT * FROM model.vw_card_res_input vcri
  WHERE vcri.meta_year BETWEEN '{time_frame['min']}' AND '{time_frame['max']}'
  AND meta_class NOT IN ('211', '212')
"
  )
)

sf_parcels <- res_chars %>%
  # Join on sales indicator for most recent sale in the last two years -
  # if there is one
  left_join(
    sales %>%
      filter(
        dplyr::between(meta_year, time_frame["min"], time_frame["max"]) &
          sv_is_outlier == FALSE
      ) %>%
      select(meta_pin, meta_sale_date, meta_year) %>%
      group_by(meta_pin) %>%
      filter(meta_sale_date == max(meta_sale_date, na.rm = TRUE)) %>%
      ungroup()
  ) %>%
  mutate(sale = as.numeric(!is.na(meta_sale_date)))


# Vars to test across
vars <-
  grep("char_|3435|acs5_median|township_name",
       names(sf_parcels),
       value = TRUE
  ) %>%
  grep("percentile|_apts|_ncu|_use|_qlty",
       .,
       value = TRUE,
       invert = TRUE
  )

sf_parcels <- sf_parcels %>%
  mutate(char_recent_renovation = case_when(
    char_recent_renovation == 0 ~ "No",
    TRUE ~ "Yes"
  )) %>%
  mutate(
    sale = factor(
      sale,
      levels = c(0, 1),
      labels = c("No Sale", "Sale")
    ),
    across(starts_with("char_") &
             where(is.character), ~ as.factor(.x)),
    across(
      c(
        loc_tax_municipality_name,
        loc_ward_num,
        char_recent_renovation,
        char_type_resd
      ),
      as.factor
    )
  ) %>%
  select(sale, all_of(vars)) %>%
  vars_recode(
    type = "long",
    dict = ccao::vars_dict_legacy
  ) %>%
  vars_rename(names_from = "athena", names_to = "pretty") %>%
  rename_with(~ gsub("loc_", "", .x)) %>%
  rename_with(~ gsub("_", " ", .x)) %>%
  rename_with(toTitleCase) %>%
  rename_with(~ gsub("Acs5", "ACS5", .x))

logit_parcels <- sf_parcels %>%
  mutate(across(where(is.factor), ~ addNA(.x))) %>%
  drop_na()

vars <- names(sf_parcels) %>%
  grep("Sale|Township", ., value = TRUE, invert = TRUE)



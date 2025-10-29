library(arrow)
library(data.table)
library(dplyr)
library(DT)
library(ggplot2)
library(glue)
library(leaflet)
library(noctua)
library(stringr)
library(tidyr)

noctua_options(cache_size = 10, unload = TRUE)
conn <- dbConnect(noctua::athena(), rstudio_conn_tab = FALSE)

# Model metadata
baseline_metadata <- dbGetQuery(
  conn,
  # This query should only ever return one row, but limit the results to 1
  # just to be defensive
  glue(
    "
    select
      dvc_md5_assessment_data,
      dvc_md5_training_data,
      model_predictor_all_name,
      model_predictor_categorical_name
    from model.metadata
    where run_id = '{params$baseline_run_id}'
    limit 1
    "
  )
)
model_predictor_all_name <-
  baseline_metadata$model_predictor_all_name %>%
  unlist()
model_predictor_categorical_name <-
  baseline_metadata$model_predictor_categorical_name %>%
  unlist()
dvc_md5_assessment_data <- baseline_metadata$dvc_md5_assessment_data
dvc_md5_training_data <- baseline_metadata$dvc_md5_training_data


# Model metadata
comp_metadata <- dbGetQuery(
  conn,
  # This query should only ever return one row, but limit the results to 1
  # just to be defensive
  glue(
    "
    select
      dvc_md5_assessment_data,
      dvc_md5_training_data,
      model_predictor_all_name,
      model_predictor_categorical_name
    from model.metadata
    where run_id = '{params$comp_run_id}'
    limit 1
    "
  )
)

dvc_md5_assessment_data_comp <- comp_metadata$dvc_md5_assessment_data

# Split categorical and continuous predictors since we need to plot them
# differently (e.g. count vs. bin histograms, respectively)
categorical_preds <- model_predictor_categorical_name
continuous_preds <- setdiff(
  model_predictor_all_name,
  model_predictor_categorical_name
)

categorical_shaps <- paste0(categorical_preds, "_shap")
continuous_shaps <- paste0(continuous_preds, "_shap")

# Text sizes for small multiples
axis_title_size <- 6
strip_text_size <- 4
axis_text_size <- 3

# Number of small multiples per line for each type of chart
ncol_histogram <- 6
ncol_violin <- 3
ncol_line <- 6

# Function to plot a set of small multiple histograms of char values
plot_small_multiple_histograms <- function(df, stat = "bin") {
  df %>%
    ggplot(aes(x = value)) +
    geom_histogram(fill = "steelblue", stat = stat) +
    facet_wrap(
      ~predictor,
      scales = "free",
      ncol = ncol_histogram
    ) +
    scale_y_continuous(labels = scales::comma) +
    theme(
      strip.text = element_text(size = strip_text_size),
      axis.text = element_text(size = axis_text_size),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title = element_text(size = axis_title_size)
    )
}

# Base function for plotting small multiple violins and lines
plot_small_multiple_base <- function(
    df,
    y,
    ncol,
    y_axis_label = "FMV",
    range = NULL) {
  df %>%
    ggplot(aes(x = value, y = .data[[y]])) +
    geom_violin(fill = "steelblue", alpha = 0.3) +
    facet_wrap(
      ~predictor,
      scales = "free",
      ncol = ncol
    ) +
    scale_y_continuous(
      limits = range,
      labels = scales::label_currency()
    ) +
    labs(x = "Value", y = y_axis_label) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = strip_text_size),
      axis.text = element_text(size = axis_text_size),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title = element_text(size = axis_title_size)
    )
}

plot_small_multiple_violins <- function(
    df,
    y,
    y_axis_label = "FMV",
    range = NULL) {
  plot_small_multiple_base(df, y, ncol_violin, y_axis_label, range) +
    geom_violin(fill = "steelblue", alpha = 0.3)
}

# Same as above, but produce smoothed regression lines
plot_small_multiple_lines <- function(
    df,
    y,
    y_axis_label = "FMV",
    range = NULL) {
  plot_small_multiple_base(df, y, ncol_line, y_axis_label, range) +
    geom_smooth(fill = "steelblue", linewidth = 0.5)
}

# Function to compute figure height for a code chunk that is using a dataframe
# to produce small multiples based on a `predictor` X axis. This is important
# to allow the small multiple container to flex vertically as much as is
# necessary to display all of the plots
fig_height <- function(df, ncol = ncol_histogram) {
  return(1.5 * ceiling(length(unique(df$predictor)) / ncol))
}

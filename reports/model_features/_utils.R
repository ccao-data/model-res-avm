library(arrow)
library(data.table)
library(dplyr)
library(DT)
library(ggplot2)
library(glue)
library(kableExtra)
library(knitr)
library(leaflet)
library(noctua)
library(stringr)
library(tidyr)

# We want sub-reports to be able to be run on their own. This ensures
# that if `model_features.qmd` isn't the report run and no param is created,
# we create the params object from the frontmatter of the main report file.
#
# WARNING: This function definition is a duplicate of a function with the same
# name in `reports/_setup.R`, so if you change this function, you should
# change that one too
parse_params_from_frontmatter <- function(path, defaults = NULL) {
  fm <- rmarkdown::yaml_front_matter(path)

  p <- fm$params

  # Ensure a regular named list
  p <- as.list(p)

  if (!is.null(defaults)) {
    defaults <- as.list(defaults)
    # params override defaults
    p <- utils::modifyList(defaults, p)
  }

  p
}

# We only want to parse the params if they are not-defined
if (!exists("params", inherits = TRUE)) {
  params <- parse_params_from_frontmatter("model_features.qmd")
}

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

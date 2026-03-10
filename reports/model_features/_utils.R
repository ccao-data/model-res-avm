library(arrow)
library(data.table)
library(dplyr)
library(DT)
library(ggplot2)
library(glue)
library(here)
library(kableExtra)
library(knitr)
library(leaflet)
library(noctua)
library(stringr)
library(tidyr)
library(yaml)

conflicted::conflicts_prefer(glue::glue, dplyr::filter)

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
if (!exists("params")) {
  params <- parse_params_from_frontmatter(
    here::here("reports", "model_features", "model_features.qmd")
  )
}

# list of file names, local paths,
# and mirrored S3 location URIs from file_dict.csv
model_file_dict <- function(run_id = NULL, year = NULL) {
  env <- environment()
  wd <- here::here()
  suppressPackageStartupMessages(library(magrittr))

  if (!is.null(run_id)) {
    if (run_id == "") {
      stop("run_id cannot be an empty string")
    } else if (!stringr::str_detect(run_id, "^[a-z0-9]+(?:[-][a-z0-9]+)*$")) {
      stop("run_id must contain only alphanumeric characters and hyphens")
    } else if (!stringr::str_detect(run_id, "^[0-9]{4}-[0-9]{2}-[0-9]{2}-[a-z]*-[a-z]*")) { # nolint
      stop("run_id must be in the format YYYY-MM-DD-<adjective>-<person>")
    }
  }

  if (!is.null(year)) {
    if (year == "") {
      stop("year cannot be an empty string")
    } else if (!stringr::str_detect(year, "^[0-9]{4}$")) {
      stop("year must be a four-digit number")
    } else if (is.numeric(year)) {
      stop("year must be a string")
    }
  }

  # Convert flat dictionary file to nested list
  dict <- read.csv(
    here::here("misc", "file_dict.csv"),
    colClasses = c("character", "character", "numeric", rep("character", 9)),
    na.strings = ""
  ) %>%
    dplyr::mutate(
      s3 = as.character(purrr::map_if(
        path_s3, ~ !is.na(.x), glue::glue,
        .envir = env, .na = NULL, .null = NA_character_
      )),
      s3 = ifelse(!is.na(s3), file.path(paste0("s3://", s3_bucket), s3), NA),
      local = ifelse(!is.na(path_local), file.path(wd, path_local), NA)
    ) %>%
    dplyr::select(type, name, s3, local) %>%
    split(., .$type) %>%
    purrr::map(., ~ split(.x, .x$name, drop = TRUE)) %>%
    purrr::map(., ~ purrr::map(.x, function(x) {
      as.list(x)[!is.na(x) & names(x) %in% c("s3", "local")]
    }))

  return(dict)
}
paths <- model_file_dict()

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

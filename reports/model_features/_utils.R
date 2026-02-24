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
if (!exists("params")) {
  params <- parse_params_from_frontmatter(
    here::here("reports", "model_features", "model_features.qmd")
  )
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
plot_small_multiple_histograms <- function(comp, baseline, stat = "bin") {
  # Add grouping labels
  comp <- comp %>% mutate(group = "comp")
  baseline <- baseline %>% mutate(group = "baseline")

  # Combine
  df_all <- bind_rows(comp, baseline)

  # Plot
  df_all %>%
    ggplot(aes(x = value, fill = group)) +
    geom_histogram(
      stat = stat,
      position = "identity",
      alpha = 0.5
    ) +
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
    comp_df,
    baseline_df,
    y,
    ncol,
    y_axis_label = "FMV",
    range = NULL) {
  comp_df$dataset <- "comp"
  baseline_df$dataset <- "baseline"

  df <- dplyr::bind_rows(comp_df, baseline_df)

  ggplot(df, aes(x = value, y = .data[[y]], fill = dataset)) +
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


# Function to compute figure height for a code chunk that is using a dataframe
# to produce small multiples based on a `predictor` X axis. This is important
# to allow the small multiple container to flex vertically as much as is
# necessary to display all of the plots
fig_height <- function(df, ncol = ncol_histogram) {
  return(1.5 * ceiling(length(unique(df$predictor)) / ncol))
}
plot_small_multiple_lines <- function(comp_df,
                                      baseline_df,
                                      y,
                                      y_axis_label = "FMV",
                                      range = NULL) {
  plot_small_multiple_base(
    comp_df,
    baseline_df,
    y,
    col_line,
    y_axis_label,
    range
  ) +
    geom_smooth(linewidth = 0.5, se = TRUE, aes(color = dataset), fill = NA) +
    guides(fill = "none")
}
plot_small_multiple_violins <- function(comp_df,
                                        baseline_df,
                                        y,
                                        y_axis_label = "FMV",
                                        range = NULL) {
  plot_small_multiple_base(
    comp_df,
    baseline_df,
    y,
    ncol_violin,
    y_axis_label,
    range
  ) +
    geom_violin(alpha = 0.3, position = "identity") +
    guides(color = "none")
}

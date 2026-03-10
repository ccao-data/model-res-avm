#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Setup ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Start the stage timer
tictoc::tic.clearlog()
tictoc::tic("Subset")

# Load libraries, helpers, and recipes from files
purrr::walk(list.files("R/", "\\.R$", full.names = TRUE), source)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. Create Subset -------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

subset_enable <- as.logical(
  Sys.getenv("SUBSET_ENABLE_OVERRIDE", unset = params$input$subset$enable)
)

if (subset_enable) {
  message(
    "Creating stratified training data subset ",
    "(fraction: ", params$input$subset$fraction, ")"
  )

  training_data <- read_parquet(paths$input$training$local)
  set.seed(params$input$subset$seed)

  # Stratified sample: within each year x township x class group, keep at
  # least 1 row and otherwise sample the configured fraction
  subset_data <- training_data %>%
    group_by(time_sale_year, meta_township_code, meta_class) %>%
    group_modify(~ {
      n_sample <- max(1L, ceiling(nrow(.x) * params$input$subset$fraction))
      slice_sample(.x, n = n_sample)
    }) %>%
    ungroup()

  message(
    "Subset: ", nrow(subset_data), " / ", nrow(training_data),
    " rows (", round(nrow(subset_data) / nrow(training_data) * 100, 1), "%)"
  )

  subset_data %>%
    write_parquet(paths$input$training_subset$local)
} else {
  message("Subset mode disabled, writing schema-only stub")

  # Write an empty parquet with the same schema so DVC outputs are satisfied
  training_data <- read_parquet(paths$input$training$local)
  training_data[0, ] %>%
    write_parquet(paths$input$training_subset$local)
}

# End the stage timer
tictoc::toc(log = FALSE)

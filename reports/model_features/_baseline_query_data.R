noctua_options(cache_size = 10, unload = TRUE)

AWS_ATHENA_CONN_NOCTUA <- dbConnect(
  noctua::athena(),
  s3_staging_dir   = "s3://ccao-athena-results-us-east-1/",
  region_name      = "us-east-1",
  rstudio_conn_tab = FALSE
)

# Metadata and predictor naming ------------------------------------------------

# We use an new vs old nomenclature to differentiate data from the current
# model run (new) to a model run that we want to compare it to (old).

# Grab metadata to check output data <> params alignment
metadata <- read_parquet(paths$output$metadata$local)

if (metadata$run_id != params$run_id) {
  stop(
    "Local run outputs are NOT equal to the requested run_id. You ",
    "should run model_fetch_run() to fetch model outputs from S3"
  )
}

model_params <- read_yaml(here("params.yaml"))

paths <- model_file_dict(model_params$run_id, model_params$year)

if (!exists("model_predictor_all_name")) {
  model_predictor_all_name <- model_params$model$predictor$all %>%
    unlist()
}

if (!exists("model_predictor_categorical_name")) {
  model_predictor_categorical_name <-
    model_params$model$predictor$categorical %>%
    unlist()
}


# Split categorical and continuous predictors since we need to plot them
# differently (e.g. count vs. bin histograms, respectively)
if (!exists("categorical_preds")) {
  categorical_preds <- model_predictor_categorical_name
}

if (!exists("continuous_preds")) {
  continuous_preds <- setdiff(
    model_predictor_all_name,
    model_predictor_categorical_name
  )
}

# Model metadata
if (!exists("metadata_old")) {
  # Fetch the metadata for the most recent final model, which should be the one
  # we use for the comparison analysis.
  #
  # Since it's possible for there to be multiple final models in a given year,
  # each one for a different set of townships, this query needs to decide how
  # to choose between multiple final models. We follow the naive heuristic of
  # ordering by model finalization date and selecting the model that we
  # finalized most recently
  metadata_old <- dbGetQuery(
    conn = AWS_ATHENA_CONN_NOCTUA,
    statement = glue::glue("
    select
      final.run_id
    from model.final_model final
    where final.type = 'res'
      and CAST(final.year AS INTEGER) = {model_params$assessment$year} - 1
    order by final.date_finalized desc
    limit 1
  ")
  )
}

# Include an error if for some reason we do not get valid old metadata
if (is.null(metadata_old) || nrow(metadata_old) == 0) {
  stop(
    "Missing prior year data for comparison."
  )
}

# Assessment Data --------------------------------------------------------------

# Get assessment set chars for new and old data
if (!exists("assessment_data_new")) {
  assessment_data_new <- ccao_download_model_input_data(
    params$run_id,
    "assessment"
  ) %>%
    select(
      meta_pin,
      meta_card_num,
      meta_year,
      meta_class,
      # We use any of since predictors can change year over year
      any_of(model_predictor_all_name)
    ) %>%
    collect()
}

if (!exists("assessment_data_old")) {
  assessment_data_old <- ccao_download_model_input_data(
    metadata_old$run_id,
    "assessment"
  ) %>%
    select(
      meta_pin,
      meta_card_num,
      meta_year,
      meta_class,
      any_of(model_predictor_all_name)
    ) %>%
    collect()
}

# SHAPs ------------------------------------------------------------------------

# Get SHAPs for new data (we don't use old SHAPs)
if (!exists("shaps_new")) {
  if (
    file.exists(paths$output$shap$local) &&
      metadata$shap_enable
  ) {
    shap_df <- read_parquet(paths$output$shap$local)
    shap_exists <- nrow(shap_df) > 0

    if (shap_exists) {
      shaps_new <- shap_df %>%
        collect() %>%
        left_join(
          assessment_data_new,
          by = c("meta_pin", "meta_card_num"),
          suffix = c("_shap", "")
        ) %>%
        select(-meta_year_shap)
    }
  } else {
    shap_exists <- FALSE
  }
}

# We use a different naming nomenclature for our columns in later joins
if (!exists("categorical_shaps")) {
  categorical_shaps <- paste0(categorical_preds, "_shap")
}

if (!exists("continuous_shaps")) {
  continuous_shaps <- paste0(continuous_preds, "_shap")
}

# Training Data ----------------------------------------------------------------

# Get new and old training data
if (!exists("training_data_old")) {
  training_data_old <- ccao_download_model_input_data(
    metadata_old$run_id,
    "training"
  ) %>%
    select(
      meta_pin,
      meta_card_num,
      meta_year,
      meta_sale_price,
      meta_sale_date,
      meta_class,
      any_of(model_predictor_all_name)
    ) %>%
    collect()
}

if (!exists("training_data_new")) {
  training_data_new <- read_parquet(paths$input$training$local) %>%
    select(
      meta_pin,
      meta_card_num,
      meta_year,
      meta_sale_price,
      meta_sale_date,
      meta_class,
      all_of(model_predictor_all_name)
    ) %>%
    collect()
}

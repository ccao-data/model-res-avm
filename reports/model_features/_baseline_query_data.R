noctua_options(cache_size = 10, unload = TRUE)

AWS_ATHENA_CONN_NOCTUA <- dbConnect(
  noctua::athena(),
  s3_staging_dir   = "s3://ccao-athena-results-us-east-1/",
  region_name      = "us-east-1",
  rstudio_conn_tab = FALSE
)

base_dvc_url <- "s3://ccao-data-dvc-us-east-1"
base_model_results_url <- "s3://ccao-model-results-us-east-1"

# Grab metadata to check output data <> params alignment
metadata <- read_parquet(paths$output$metadata$local)

model_params <- read_yaml(here("params.yaml"))

if (!exists("model_predictor_all_name")) {
  model_predictor_all_name <- model_params$model$predictor$all %>%
    unlist()
}

if (!exists("model_predictor_categorical_name")) {
  model_predictor_categorical_name <-
    model_params$model$predictor$categorical %>%
    unlist()
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
      final.run_id,
      model.dvc_md5_assessment_data,
      model.dvc_md5_training_data,
      model.model_predictor_all_name,
      model.assessment_year,
      model.model_predictor_categorical_name
    from model.metadata model
    join model.final_model final
      on model.run_id = final.run_id
    where final.type = 'res'
      and CAST(final.year AS INTEGER) = {model_params$assessment$year} - 1
    order by final.date_finalized desc
    limit 1
  ")
  )
}

# Get assessment data for both old and new datasets

if (!exists("assessment_year_old")) {
  assessment_year_old <- metadata_old$assessment_year
}

if (!exists("dvc_md5_assessment_data_old")) {
  dvc_md5_assessment_data_old <- metadata_old$dvc_md5_assessment_data
}


if (!exists("assessment_data_old")) {
  assessment_data_old <- open_dataset(
    paste0(
      glue("{base_dvc_url}/files/md5/"),
      substr(dvc_md5_assessment_data_old, 1, 2), "/",
      substr(dvc_md5_assessment_data_old, 3, 32)
    )
  ) %>%
    select(
      meta_pin,
      meta_card_num,
      meta_year,
      meta_class,
      all_of(model_predictor_all_name)
    ) %>%
    collect()
}

if (!exists("year_new")) {
  year_new <- model_params$assessment$year
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

if (!exists("categorical_shaps")) {
  categorical_shaps <- paste0(categorical_preds, "_shap")
}

if (!exists("continuous_shaps")) {
  continuous_shaps <- paste0(continuous_preds, "_shap")
}

# Get assessment set chars for new and old data
if (!exists("assessment_data_new")) {
  assessment_data_new <- read_parquet(paths$input$assessment$local) %>%
    select(
      meta_pin,
      meta_card_num,
      meta_year,
      meta_class,
      all_of(model_predictor_all_name)
    ) %>%
    collect()
}

assessment_data_old <- open_dataset(
  paste0(
    glue("{base_dvc_url}/files/md5/"),
    substr(dvc_md5_assessment_data_old, 1, 2), "/",
    substr(dvc_md5_assessment_data_old, 3, 32)
  )
) %>%
  select(
    meta_pin,
    meta_card_num,
    meta_year,
    meta_class,
    all_of(model_predictor_all_name)
  ) %>%
  collect()

# Get SHAPs for new and old data
if (
  !exists("shaps_new") &&
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
      # This column isn't a real SHAP, it's just an artifact of the join
      select(-meta_year_shap)
  }
} else {
  shap_exists <- FALSE
}

if (!exists("shaps_old")) {
  shap_old_df <- open_dataset(
    paste0(
      glue("{base_model_results_url}/shap/"),
      glue("year={assessment_year_old}/"),
      glue("run_id={metadata_old$run_id}")
    )
  ) %>%
    collect()
  shap_old_exists <- nrow(shap_old_df) > 0
  if (shap_old_exists) {
    shaps_old <- shap_old_df %>%
      left_join(
        assessment_data_old,
        by = c("meta_pin", "meta_card_num"),
        suffix = c("_shap", "")
      ) %>%
      select(-meta_year_shap)
  }
} else {
  shap_old_exists <- FALSE
}


# Get training data for new and old data
# Training data
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

if (!exists("training_data_old")) {
  training_data_old <- open_dataset(
    paste0(
      glue("{base_dvc_url}/files/md5/"),
      substr(metadata_old$dvc_md5_training_data, 1, 2), "/",
      substr(metadata_old$dvc_md5_training_data, 3, 32)
    )
  ) %>%
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

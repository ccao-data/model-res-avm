noctua_options(cache_size = 10, unload = TRUE)

AWS_ATHENA_CONN_NOCTUA <- dbConnect(
  noctua::athena(),
  s3_staging_dir   = "s3://ccao-athena-results-us-east-1/",
  region_name      = "us-east-1",
  rstudio_conn_tab = FALSE
)

base_dvc_url <- "s3://ccao-data-dvc-us-east-1"
base_model_results_url <- "s3://ccao-model-results-us-east-1"

if (knitr::is_html_output() || knitr::is_latex_output()) {
  params_obj_name <- "model_params"
} else if (exists("params")) {
  if (exists("run_id", where = params)) params_obj_name <- "model_params"
} else {
  params_obj_name <- "params"
}

# Grab metadata to check output data <> params alignment
metadata <- read_parquet(paths$output$metadata$local)

assign(params_obj_name, read_yaml(here("params.yaml")))

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

# Model metadata
if (!exists("old_metadata")) {
  # This query should only ever return one row, but limit the results to 1
  # just to be defensive. It fetches the metadata for the most recent final
  # model, which should be the one used for the comparison analysis
  old_metadata <- dbGetQuery(
    conn = AWS_ATHENA_CONN_NOCTUA,
    statement = glue::glue("
    select
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
      limit 1
  ")
  )
}

if (!exists("assessment_year_old")) {
  assessment_year_old <- old_metadata$assessment_year
}

if (!exists("dvc_md5_assessment_data_old")) {
  dvc_md5_assessment_data_old <- old_metadata$dvc_md5_assessment_data
}

if (!exists("old_assessment_data")) {
  old_assessment_data <- open_dataset(
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
      any_of(model_predictor_all_name)
    ) %>%
    collect()
}

if (!exists("new_year")) {
  new_year <- model_params$assessment$working_year
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

# Assessment set chars
if (!exists("new_assessment_data")) {
  new_assessment_data <- read_parquet(paths$input$assessment$local) %>%
    select(
      meta_pin,
      meta_card_num,
      meta_year,
      meta_class,
      all_of(model_predictor_all_name)
    ) %>%
    collect()
}

# SHAPs
if (file.exists(paths$output$shap$local) && metadata$shap_enable) {
  shap_df <- read_parquet(paths$output$shap$local)
  shap_exists <- nrow(shap_df) > 0

  if (shap_exists) {
    new_shaps <- shap_df %>%
      collect() %>%
      left_join(
        new_assessment_data,
        by = c("meta_pin", "meta_card_num"),
        suffix = c("_shap", "")
      ) %>%
      # This column isn't a real SHAP, it's just an artifact of the join
      select(-meta_year_shap)
  }
} else {
  shap_exists <- FALSE
}

# Training data
if (!exists("new_training_data")) {
  new_training_data <- read_parquet(paths$input$training$local) %>%
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

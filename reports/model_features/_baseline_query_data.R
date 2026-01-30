noctua_options(cache_size = 10, unload = TRUE)
conn <- dbConnect(noctua::athena(), rstudio_conn_tab = FALSE)

base_model_results_url <- "s3://ccao-model-results-us-east-1"

# Model metadata
if (!exists("baseline_metadata")) {
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
      model_predictor_categorical_name,
      assessment_year
    from model.metadata
    where run_id = '{params$baseline_run_id}'
    limit 1
    "
    )
  )
}


if (!exists("model_predictor_all_name")) {
  model_predictor_all_name <-
    baseline_metadata$model_predictor_all_name %>%
    unlist()
}

if (!exists("model_predictor_categorical_name")) {
  model_predictor_categorical_name <-
    baseline_metadata$model_predictor_categorical_name %>%
    unlist()
}

if (!exists("dvc_md5_assessment_data")) {
  dvc_md5_assessment_data <- baseline_metadata$dvc_md5_assessment_data
}

if (!exists("dvc_md5_training_data")) {
  dvc_md5_training_data <- baseline_metadata$dvc_md5_training_data
}

# Model metadata
if (!exists("comp_metadata")) {
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
      assessment_year,
      model_predictor_categorical_name
    from model.metadata
    where run_id = '{params$comp_run_id}'
    limit 1
    "
    )
  )
}

if (!exists("assessment_year_comp")) {
  assessment_year_comp <- comp_metadata$assessment_year
}

if (!exists("dvc_md5_assessment_data_comp")) {
  dvc_md5_assessment_data_comp <- comp_metadata$dvc_md5_assessment_data
}


if (!exists("comp_chars")) {
  comp_chars <- ccao_download_model_input_data(
    {
      params$comp_run_id
    },
    "char"
  ) %>%
    select(
      meta_pin,
      meta_card_num,
      meta_year,
      meta_class,
      any_of(model_predictor_all_name)
    )
}

if (!exists("baseline_year")) {
  baseline_year <- baseline_metadata$assessment_year
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
if (!exists("baseline_assessment_data")) {
  baseline_assessment_data <- ccao_download_model_input_data(
    {
      params$baseline_run_id
    },
    "assessment"
  ) %>%
    select(
      meta_pin,
      meta_card_num,
      meta_year,
      meta_class,
      all_of(model_predictor_all_name)
    )
}

# SHAPs
if (!exists("baseline_shaps")) {
  baseline_shaps <- open_dataset(
    paste0(
      glue("{base_model_results_url}/shap/"),
      glue("year={baseline_year}/"),
      glue("run_id={params$baseline_run_id}")
    )
  ) %>%
    collect() %>%
    left_join(
      baseline_assessment_data,
      by = c("meta_pin", "meta_card_num"),
      suffix = c("_shap", "")
    ) %>%
    # This column isn't a real SHAP, it's just an artifact of the join
    select(-meta_year_shap)
}

# Training data
if (!exists("baseline_training_data")) {
  ccao_download_model_input_data(
    {
      params$baseline_run_id
    },
    "training"
  ) %>%
    select(
      meta_pin,
      meta_card_num,
      meta_year,
      meta_sale_price,
      meta_sale_date,
      meta_class,
      all_of(model_predictor_all_name)
    )
}

source("_utils.R")

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
      model_predictor_categorical_name,
      assessment_year
    from model.metadata
    where run_id = '{params$baseline_run_id}'
    limit 1
    "
  )
)

baseline_year <- baseline_metadata$assessment_year

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
      assessment_year,
      model_predictor_categorical_name
    from model.metadata
    where run_id = '{params$comp_run_id}'
    limit 1
    "
  )
)

assessment_year_comp <- comp_metadata$assessment_year

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

base_dvc_url <- "s3://ccao-data-dvc-us-east-1"
base_model_results_url <- "s3://ccao-model-results-us-east-1"

# Assessment set chars
baseline_chars <- open_dataset(
  paste0(
    glue("{base_dvc_url}/files/md5/"),
    substr(dvc_md5_assessment_data, 1, 2), "/",
    substr(dvc_md5_assessment_data, 3, 32)
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

comp_chars <- open_dataset(
  paste0(
    glue("{base_dvc_url}/files/md5/"),
    substr(dvc_md5_assessment_data_comp, 1, 2), "/",
    substr(dvc_md5_assessment_data_comp, 3, 32)
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

# Assessment set predictions
baseline_assess_card <- open_dataset(
  paste0(
    glue("{base_model_results_url}/assessment_card/"),
    glue("year={params$baseline_year}/"),
    glue("run_id={params$baseline_run_id}")
  )
) %>%
  collect()

# Grab PIN predictions so we can get the final rounded value
baseline_assess_pin <- open_dataset(
  paste0(
    glue("{base_model_results_url}/assessment_pin/"),
    glue("year={params$baseline_year}/"),
    glue("run_id={params$baseline_run_id}")
  )
) %>%
  collect()

# SHAPs
baseline_shaps <- open_dataset(
  paste0(
    glue("{base_model_results_url}/shap/"),
    glue("year={params$baseline_year}/"),
    glue("run_id={params$baseline_run_id}")
  )
) %>%
  collect() %>%
  left_join(
    baseline_chars,
    by = c("meta_pin", "meta_card_num"),
    suffix = c("_shap", "")
  ) %>%
  # This column isn't a real SHAP, it's just an artifact of the join
  select(-meta_year_shap)

# Training data
baseline_training_data <- open_dataset(
  paste0(
    glue("{base_dvc_url}/files/md5/"),
    substr(dvc_md5_training_data, 1, 2), "/",
    substr(dvc_md5_training_data, 3, 32)
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

# Clean up large objects that we no longer need
rm(baseline_assess_card, baseline_assess_pin)

source("_utils.R")


base_dvc_url <- "s3://ccao-data-dvc-us-east-1"
base_model_results_url <- "s3://ccao-model-results-us-east-1"

# Assessment set chars
baseline_assessment_data <- open_dataset(
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
    baseline_assessment_data,
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

gc()

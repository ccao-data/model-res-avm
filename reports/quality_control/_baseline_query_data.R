source("_utils.R")


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

baseline_preds <- baseline_assess_card %>%
  left_join(baseline_assess_pin, by = c("meta_pin")) %>%
  select(
    meta_pin, meta_card_num,
    pred_card_initial_fmv,
    pred_pin_final_fmv_round
  )

# Full assessment set
baseline_assessment_data <- baseline_chars %>%
  left_join(baseline_preds, by = c("meta_pin", "meta_card_num"))

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
rm(baseline_preds, baseline_assess_card, baseline_assess_pin)
gc()

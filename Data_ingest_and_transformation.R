base_paths <- model_file_dict(params$run_id, params$run_id_year)
comparison_paths <- model_file_dict(
  params$comparison_run_id,
  params$comparison_run_id_year
)
run_id <- params$run_id
comparison_run_id <- params$comparison_run_id


analyses_paths <- list(
  output = list(
    list(
      s3 = base_paths$output$assessment_card$s3,
      key = "assessment_card"
    ),
    list(
      s3 = base_paths$output$assessment_pin$s3,
      key = "assessment_pin"
    ),
    list(
      s3 = base_paths$output$metadata$s3,
      key = "metadata"
    ),
    list(
      s3 = base_paths$output$performance_test$s3,
      key = "performance_test"
    ),
    list(
      s3 = base_paths$output$shap$s3,
      key = "shap"
    )
  )
)

source("analyses/Ingest_script.qmd")

data_new <- model_fetch_run_subset(
  params$run_id,
  params$run_id_year, analyses_paths, TRUE
)

list2env(data, envir = .GlobalEnv)

rm(data_new)

comparison_paths <- list(
  output = list(
    list(
      s3 = base_paths$output$assessment_card$s3,
      key = "assessment_card"
    ),
    list(
      s3 = base_paths$output$assessment_pin$s3,
      key = "assessment_pin"
    ),
    list(
      s3 = base_paths$output$metadata$s3,
      key = "metadata"
    ),
    list(
      s3 = base_paths$output$performance_test$s3,
      key = "performance_test"
    ),
    list(
      s3 = base_paths$output$shap$s3,
      key = "shap"
    )
  )
)

data_comparison <- model_fetch_run_subset(
  params$comparison_run_id,
  params$comparison_run_id_year,
  comparison_paths, TRUE
)

list2env(data_comparison, envir = .GlobalEnv)

rm(data_comparison)

all_vars <- ls()

# Loop through the variables and rename those that match the patterns
for (var_name in all_vars) {
  # Check if the variable is a dataframe and ends with _run_id
  if (exists(var_name) && is.data.frame(get(var_name)) &&
    grepl(paste0("_", params$run_id, "$"), var_name)) {
    new_name <- sub(paste0("_", params$run_id, "$"), "_new", var_name)
    assign(new_name, get(var_name), envir = .GlobalEnv)
    rm(list = var_name, envir = .GlobalEnv)
  }

  # Check if the variable is a dataframe and ends with _comp_run_id
  if (exists(var_name) && is.data.frame(get(var_name)) &&
    grepl(paste0("_", params$comp_run_id, "$"), var_name)) {
    new_name <- sub(paste0("_", params$comp_run_id, "$"), "_comparison", var_name)
    assign(new_name, get(var_name), envir = .GlobalEnv)
    rm(list = var_name, envir = .GlobalEnv)
  }
}

# Loop through the variables and rename those that match the patterns
for (var_name in all_vars) {
  # Check if the variable is a dataframe and ends with _run_id
  if (exists(var_name) && is.data.frame(get(var_name)) &&
    grepl(paste0("_", params$run_id, "$"), var_name)) {
    new_name <- sub(paste0("_", params$run_id, "$"), "_new", var_name)
    assign(new_name, get(var_name), envir = .GlobalEnv)
    rm(list = var_name, envir = .GlobalEnv)
  }

  # Check if the variable is a dataframe and ends with _comp_run_id
  if (exists(var_name) && is.data.frame(get(var_name)) &&
    grepl(paste0("_", params$comp_run_id, "$"), var_name)) {
    new_name <- sub(paste0("_", params$comp_run_id, "$"), "_comparison", var_name)
    assign(new_name, get(var_name), envir = .GlobalEnv)
    rm(list = var_name, envir = .GlobalEnv)
  }
}

target_feature_value <- params$added_feature
target_feature_shap <- params$added_feature_shap
nbhd <- ccao::nbhd_shp



# Selecting and joining relevant data
card_individual <- shap_new %>%
  select(
    meta_pin, meta_card_num, pred_card_shap_baseline_fmv,
    {{ target_feature_value }}
  ) %>%
  rename(!!target_feature_shap := !!target_feature_value) %>%
  inner_join(assessment_card_new, by = c("meta_pin", "meta_card_num")) %>%
  mutate(
    relative_shap =
      round(!!sym({{ target_feature_shap }}) / pred_card_initial_fmv, 2)
  )



# Summarizing data by neighborhood code
card_nbhd <- card_individual %>%
  group_by(meta_nbhd_code) %>%
  summarize(
    avg_target_feature_shap =
      mean(!!sym({{ target_feature_shap }}), na.rm = TRUE),
    avg_target_feature_shap_abs =
      mean(abs(!!sym({{ target_feature_shap }})), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  inner_join(
    nbhd,
    by = c("meta_nbhd_code" = "town_nbhd")
  ) %>%
  st_as_sf()

pin_individual <- assessment_pin_new %>%
  select(meta_pin, pred_pin_final_fmv, pred_pin_initial_fmv) %>%
  rename(
    pred_pin_final_fmv_new = pred_pin_final_fmv,
    pred_pin_initial_fmv_new = pred_pin_initial_fmv
  ) %>%
  inner_join(
    assessment_pin_comparison %>%
      select(meta_pin, pred_pin_final_fmv, pred_pin_initial_fmv),
    by = "meta_pin"
  ) %>%
  rename(
    pred_pin_final_fmv_comp = pred_pin_final_fmv,
    pred_pin_initial_fmv_comp = pred_pin_initial_fmv
  ) %>%
  mutate(
    diff_pred_pin_final_fmv = round(((
      pred_pin_final_fmv_new -
        pred_pin_final_fmv_comp) /
      pred_pin_final_fmv_comp), 4),
    pred_pin_final_fmv_new = dollar(pred_pin_final_fmv_new),
    pred_pin_final_fmv_comp = dollar(pred_pin_final_fmv_comp),
    diff_pred_pin_initial_fmv = round(((
      pred_pin_initial_fmv_new -
        pred_pin_initial_fmv_comp) /
      pred_pin_initial_fmv_comp), 4),
    pred_pin_initial_fmv_new = dollar(pred_pin_initial_fmv_new),
    pred_pin_initial_fmv_comp = dollar(pred_pin_initial_fmv_comp)
  ) %>%
  inner_join(
    assessment_data %>%
      distinct(meta_pin, .keep_all = TRUE) %>%
      select(
        meta_pin, meta_nbhd_code, loc_longitude,
        loc_latitude, meta_township_name, {{ target_feature_value }}
      ),
    by = "meta_pin"
  )

pin_nbhd <- pin_individual_new %>%
  group_by(meta_nbhd_code) %>%
  summarize(
    !!paste0({{ target_feature_value }}, "_neighborhood_mean") :=
      mean(!!sym({{ target_feature_value }}), na.rm = TRUE),
    !!paste0({{ target_feature_value }}, "_neighborhood_median") :=
      median(!!sym({{ target_feature_value }}), na.rm = TRUE),
    !!paste0({{ target_feature_value }}, "_neighborhood_90th") :=
      quantile(!!sym({{ target_feature_value }}), 0.9, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  inner_join(
    nbhd,
    by = c("meta_nbhd_code" = "town_nbhd")
  ) %>%
  st_as_sf()

leaflet_data <- card_individual %>%
  select(meta_pin, {{ target_feature_shap }}) %>%
  group_by(meta_pin) %>%
  mutate(row = row_number()) %>%
  pivot_wider(names_from = row, values_from = {{ target_feature_shap }}, names_prefix = "target_feature_shap_") %>%
  right_join(pin_individual, by = c("meta_pin" = "meta_pin"))

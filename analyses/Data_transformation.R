target_feature_value <- params$added_feature
target_feature_shap <- params$added_feature_shap
nbhd <- ccao::nbhd_shp

# Create a individual card level dataset
card_individual <- shap_new %>%
  select(
    meta_pin, meta_card_num, pred_card_shap_baseline_fmv,
    {{ target_feature_value }}
  ) %>%
  rename(!!sym(target_feature_shap) := !!sym(target_feature_value)) %>%
  inner_join(
    assessment_card_new %>%
      select(
        meta_pin, meta_nbhd_code,
        meta_card_num,
        pred_card_initial_fmv,
        {{ target_feature_value }}
      ),
    by = c("meta_pin", "meta_card_num")
  )

# Summarizing data by neighborhood code
card_nbhd <- card_individual %>%
  group_by(meta_nbhd_code) %>%
  summarize(
    !!paste0({{ target_feature_shap }}, "_mean") :=
      mean(!!sym({{ target_feature_shap }}), na.rm = TRUE),
    !!paste0({{ target_feature_shap }}, "_90th") :=
      quantile(!!sym({{ target_feature_shap }}), probs = 0.9, na.rm = TRUE),
    !!paste0({{ target_feature_shap }}, "_mean_abs") :=
      mean(abs(!!sym({{ target_feature_shap }})), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  inner_join(
    nbhd,
    by = c("meta_nbhd_code" = "town_nbhd")
  ) %>%
  st_as_sf()

## Create a pin level dataset
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
    diff_pred_pin_final_fmv =
      round(((pred_pin_final_fmv_new - pred_pin_final_fmv_comp) /
        pred_pin_final_fmv_comp), 4),
    pred_pin_final_fmv_new = dollar(pred_pin_final_fmv_new),
    pred_pin_final_fmv_comp = dollar(pred_pin_final_fmv_comp),
    diff_pred_pin_initial_fmv =
      round(((pred_pin_initial_fmv_new - pred_pin_initial_fmv_comp) /
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

# Aggregate to neighborhood level
pin_nbhd <- pin_individual %>%
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

# Pivot wider for leaflet maps to allow multiple shap values
leaflet_data <- card_individual %>%
  select(meta_pin, relative_shap, {{ target_feature_shap }}) %>%
  group_by(meta_pin) %>%
  mutate(
    shap_total = sum(!!sym({{ target_feature_shap }})),
    variable_index = row_number(),
    name_col = paste0(
      deparse(substitute(
        target_feature_shap
      )), "_",
      variable_index
    )
  ) %>%
  pivot_wider(
    id_cols = c("meta_pin", "shap_total"),
    names_from = name_col,
    values_from = !!sym({{ target_feature_shap }})
  ) %>%
  ungroup() %>%
  right_join(pin_individual, by = c("meta_pin" = "meta_pin")) %>%
  mutate(
    pred_pin_initial_fmv_new_numeric =
      as.numeric(gsub("[$,]", "", pred_pin_initial_fmv_new)),
    relative_shap =
      round(as.numeric(shap_total) / pred_pin_initial_fmv_new_numeric, 2)
  ) %>%
  distinct(meta_pin, .keep_all = TRUE)

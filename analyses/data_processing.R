target_feature_value <- params$added_feature
target_feature_shap <- params$added_feature_shap
nbhd <- ccao::nbhd_shp

# Selecting and joining relevant data
card_individual <- shap %>%
  select(meta_pin, meta_card_num, pred_card_shap_baseline_fmv,
         {{ target_feature_value }}) %>%
  rename(!!target_feature_shap := !!target_feature_value) %>%
  inner_join(assessment_card, by = c("meta_pin", "meta_card_num")) %>%
  inner_join(
    assessment_data %>%
      select(meta_pin, meta_card_num, meta_nbhd_code,
             loc_longitude, loc_latitude, meta_township_name),
    by = c("meta_pin", "meta_card_num")
  )


# Summarizing data by neighborhood code
card_nbhd <- card_individual %>%
  group_by(meta_nbhd_code) %>%
  summarize(
    avg_target_feature_shap =
      mean(!!sym({{target_feature_shap}}), na.rm = TRUE),
    avg_pred_card_shap_baseline_fmv =
      mean(pred_card_shap_baseline_fmv, na.rm = TRUE)
  ) %>%
  ungroup()


pin_individual <- assessment_pin %>%
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
      pred_pin_final_fmv_comp) * 100, 2),
    pred_pin_final_fmv_new = dollar(pred_pin_final_fmv_new),
    pred_pin_final_fmv_comp = dollar(pred_pin_final_fmv_comp),
    diff_pred_pin_initial_fmv = round(((
                                        pred_pin_initial_fmv_new -
                                          pred_pin_initial_fmv_comp) /
      pred_pin_initial_fmv_comp) * 100, 2),
    pred_pin_initial_fmv_new = dollar(pred_pin_initial_fmv_new),
    pred_pin_initial_fmv_comp = dollar(pred_pin_initial_fmv_comp)
  ) %>%
  inner_join(
    assessment_data %>%
      select(meta_pin, meta_nbhd_code, loc_longitude,
             loc_latitude, meta_township_name, {{ target_feature_value }}),
    by = "meta_pin"
  )

pin_nbhd <- pin_individual %>%
  group_by(meta_nbhd_code) %>%
  summarize(
    !!paste0({{target_feature_value}}, "_neighborhood_mean") :=
      mean(!!sym({{target_feature_value}}), na.rm = TRUE),
    !!paste0({{target_feature_value}}, "_neighborhood_median") :=
      median(!!sym({{target_feature_value}}), na.rm = TRUE),
    !!paste0({{target_feature_value}}, "_neighborhood_90th") :=
      quantile(!!sym({{target_feature_value}}), 0.9, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  inner_join(
    nbhd,
    by = c("meta_nbhd_code" = "town_nbhd")
  )

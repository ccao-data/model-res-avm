shap_predictors <- unlist(metadata$model_predictor_all_name)

shap_df_filtered <- shap %>%
  arrange(meta_pin, meta_card_num)

shap_df_filtered_long <- shap_df_filtered %>%
  select(all_of(shap_predictors)) %>%
  pivot_longer(
    cols = all_of(shap_predictors),
    names_to = "feature",
    values_to = "shap"
  )


create_shapviz <- function(shap_df, assessment_df, idx) {
  shapviz::shapviz(
    object = shap_df_filtered %>%
      select(all_of(shap_predictors)) %>%
      slice(idx) %>%
      as.matrix(),
    X = assessment_df %>%
      select(all_of(shap_predictors)) %>%
      slice(idx),
    baseline = shap_df_filtered$pred_card_shap_baseline_fmv[1]
  )
}

shapviz::shapviz(
  object = shap_df_filtered %>%
    select(all_of(shap_predictors)) %>%
    as.matrix(),
  X = assessment_card %>%
    select(all_of(shap_predictors)),
  baseline = shap_df_filtered$pred_card_shap_baseline_fmv[1]
) %>%
  shapviz::sv_importance(
    kind = "bar",
    max_display = 25L
  ) +
  labs(x = "Mean Abs. SHAP Value") +
  scale_x_continuous(
    labels = scales::dollar,
    expand = expansion(mult = c(0, 0.1))
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(margin = margin(t = 10))
  )

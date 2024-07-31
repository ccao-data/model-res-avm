pin_individual %>%
  filter(
    diff_pred_pin_initial_fmv >= quantile(diff_pred_pin_initial_fmv, 0.025, na.rm = TRUE) &
      diff_pred_pin_initial_fmv <= quantile(diff_pred_pin_initial_fmv, 0.975, na.rm = TRUE)
  ) %>%
  mutate(
    mean_value = mean(diff_pred_pin_initial_fmv, na.rm = TRUE),
    median_value = median(diff_pred_pin_initial_fmv, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = 1, y = diff_pred_pin_initial_fmv)) +
  geom_violin(fill = "blue", color = "black", alpha = 0.7) +
  geom_hline(aes(yintercept = mean_value, color = "Mean"), linetype = "dashed", linewidth = 1, show.legend = TRUE) +
  geom_hline(aes(yintercept = median_value, color = "Median"), linetype = "dashed", linewidth = 1, show.legend = TRUE) +
  scale_y_continuous(labels = label_percent()) +
  scale_color_manual(
    name = "Statistics",
    values = c(Mean = "red", Median = "green"),
    labels = c("Mean", "Median")
  ) +
  labs(
    x = "Change in FMV",
    y = "Value Distribution"
  ) +
  theme_minimal()

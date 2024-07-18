test1 <- assessment_pin %>%
  distinct(meta_pin, keep_all) %>%
  select(meta_pin, pred_pin_initial_fmv, pred_pin_final_fmv) %>%
  rename(pred_pin_initial_fmv_new = pred_pin_initial_fmv)

test2 <- assessment_pin_comparison %>%
  select(meta_pin, pred_pin_initial_fmv, pred_pin_final_fmv) %>%
  rename(pred_pin_initial_fmv_comp = pred_pin_initial_fmv)


test3 <- inner_join(test1, test2, by = "meta_pin") %>%
  mutate(fmv_change_comp = pred_pin_initial_fmv_new - pred_pin_initial_fmv_comp)

test4 <- test3 %>%
  filter(
    fmv_change_comp >= quantile(fmv_change_comp, 0.025, na.rm = TRUE) &
      fmv_change_comp <= quantile(fmv_change_comp, 0.975, na.rm = TRUE)
  ) %>%
  mutate(
    mean_value = mean(fmv_change_comp, na.rm = TRUE),
    median_value = median(fmv_change_comp, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = fmv_change_comp)) +
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = mean_value, color = "Mean"),
    linetype = "dashed", linewidth = 1, show.legend = TRUE
  ) +
  geom_vline(aes(xintercept = median_value, color = "Median"),
    linetype = "dashed", linewidth = 1, show.legend = TRUE
  ) +
  scale_color_manual(
    name = "Statistics",
    values = c(Mean = "red", Median = "green"),
    labels = c("Mean", "Median")
  ) +
  labs(
    x = "Change in FMV",
    y = "Frequency"
  ) +
  theme_minimal()


plot(test4)

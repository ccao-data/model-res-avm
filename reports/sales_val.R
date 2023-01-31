library(arrow)
library(dplyr)
library(reticulate)
library(sf)
library(ggplot2)

use_virtualenv("pipenv/")
source_python("R/flagging.py")

sv_group_cols <- list("meta_township_code", "meta_class")
iso_forest_cols <- c(
  "meta_sale_price", "sv_price_per_sqft", "sv_days_since_last_transaction",
  "sv_cgdr", "sv_sale_dup_counts"
)

training_data <- read_parquet("input/training_data_temp.parquet") %>%
  mutate(meta_sale_price = as.numeric(meta_sale_price)) %>%
  create_stats(sv_group_cols) %>%
  string_processing() %>%
  iso_forest(sv_group_cols, iso_forest_cols) %>%
  outlier_taxonomy(list(2, 3), sv_group_cols)




# Exploratory plots
ggplot(training_data) +
  geom_density(
    aes(x = meta_sale_price, fill = sv_is_outlier),
    alpha = 0.5
  ) +
  scale_x_log10(
    name = "Sale Price (Log10)",
    labels = scales::label_dollar()
  ) +
  labs(y = "Density", fill = "") +
  theme_bw()

ggplot(training_data) +
  geom_density(
    aes(x = meta_sale_price, fill = sv_is_outlier),
    alpha = 0.5
  ) +
  scale_x_log10(
    name = "Sale Price (Log10)",
    labels = scales::label_dollar(scale = 1e-6, suffix = "M")
  ) +
  labs(y = "Density", fill = "") +
  facet_wrap(vars(sv_outlier_type), nrow = 3) +
  theme_bw() +
  theme(legend.position = "none")

training_data %>%
  filter(!is.na(loc_x_3435)) %>%
  st_as_sf(coords = c("loc_x_3435", "loc_y_3435"), crs = 3435) %>% 
ggplot() +
  geom_sf(alpha = 0.3, size = 0.9) +
  facet_wrap(vars(sv_outlier_type), nrow = 3) +
  theme_void()

training_data %>%
  group_by(sv_outlier_type) %>%
  summarize(
    count = n(),
    med_sale = median(meta_sale_price),
    min = mean(meta_sale_price),
    avg_sale = mean(meta_sale_price),
    max = max(meta_sale_price),
    sd = sd(meta_sale_price) 
  ) %>%
  arrange(desc(count))

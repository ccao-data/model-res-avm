# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Setup ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load R libraries
library(arrow)
library(ccao)
library(dplyr)
library(ggplot2)
library(glue)
library(here)
library(purrr)
library(recipes)
library(sf)
library(stringr)
library(tidymodels)
library(treesnip)

## Flagging variables

# Factor by which ratios must deviate from 1 in order to be flagged. For
# example, setting this to 4 will flag ratios higher than 4 and less than 0.25
flag_ratio_factor <- 4

# Factor by which predicted value deviates from the neighborhood median. For
# example, if a neighborhood's median sale price is $100K and the model predicts
# a value of $400K, that observation's nbhd factor will be 4
flag_nbhd_factor <- 4

# Factor by which sales for the same PIN deviate year-over-year. The higher the
# factor the larger the deviation
flag_yoy_pct_change_factor <- 1.6




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Valuation ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load the final lightgbm model object and recipe from file
lgbm_final_full_fit <- ccao::model_lgbm_load(
  here("output", "models", "lgbm_model.zip")
)
lgbm_final_full_recipe <- readRDS(
  here("output", "models", "lgbm_recipe.rds")
)

# Get all arms-length sales and clean up some string columns
sales_data <- read_parquet(here("input", "modeldata.parquet")) %>%
  filter(ind_arms_length)

# Generate predicted values for all sales using the saved model and recipe
sales_data <- sales_data %>%
  mutate(
    lgbm = model_predict(
      spec = lgbm_final_full_fit,
      recipe = lgbm_final_full_recipe,
      data = .
    )
  )

# Select relevant output columns and calculate sales ratio
sales_filtered <- sales_data %>%
  select(
    meta_pin, meta_document_num, meta_class, meta_year, meta_town_code, meta_nbhd,
    geo_property_address, geo_property_apt_no, geo_property_city, geo_property_zip,
    geo_longitude, geo_latitude, model_value = lgbm, meta_sale_price
  ) %>%
  mutate(sales_ratio = model_value / meta_sale_price) %>%
  group_by(meta_nbhd) %>%
  mutate(
    sales_ratio_from_nbhd_med =
      model_value / median(meta_sale_price, na.rm = TRUE)
  ) %>% 
  ungroup()




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Flagging ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# The goal here is to flag sales which deviate significantly from their
# predicted value, neighborhood, or previous sale price. These sales are
# hand-reviewed by the CCAO and excluded from the model if they are atypical or
# not arms-length. Not all sales flagged here will be excluded

# Flag very high and very low ratios (anything that deviates from its prediction
# be a factor of flag_ratio_factor or more)
ratio_factor <- sales_filtered %>%
  filter(
    sales_ratio < (1 / flag_ratio_factor) |
    sales_ratio > flag_ratio_factor
  ) %>%
  # Invert ratios in order to rank by magnitude of error
  mutate(
    adj_sales_ratio = ifelse(
      sales_ratio < 1,
      1 / sales_ratio,
      sales_ratio
    ),
    reason = glue(
      "Estimate differs from sale price",
      "by factor of {flag_ratio_factor} or greater"
    )
  ) %>%
  arrange(desc(adj_sales_ratio)) %>%
  mutate(rank = row_number()) %>%
  select(-adj_sales_ratio)

# Flag large deviations from the neighborhood median sale price
nbhd_factor <- sales_filtered %>%
  filter(
    (sales_ratio_from_nbhd_med < (1 / flag_nbhd_factor) |
    sales_ratio_from_nbhd_med > flag_nbhd_factor),
    !meta_pin %in% ratio_factor
  ) %>%
  # Invert ratios in order to rank by magnitude of error
  mutate(
    avg_nbhd_ratio = ifelse(
      sales_ratio_from_nbhd_med < 1,
      1 / sales_ratio_from_nbhd_med,
      sales_ratio_from_nbhd_med
    ),
    reason = glue(
      "Estimate differs from neighborhood average",
      "sale price by factor of {flag_nbhd_factor} or greater"
    )
  ) %>%
  arrange(desc(avg_nbhd_ratio)) %>%
  mutate(rank = row_number()) %>%
  select(-avg_nbhd_ratio)

# Flag properties with a high year-over-year change in sale price
yoy_change_factor <- sales_filtered %>%
  group_by(meta_pin) %>%
  # Calculate ratio between range of sale prices and their mean for each PIN
  mutate(
    dev = (max(meta_sale_price) - min(meta_sale_price)) / mean(meta_sale_price)
  ) %>%
  ungroup() %>%
  mutate(reason = "High year-over-year change in sale price within 5 years") %>%
  # Filter for large YoY changes / large deviations from the mean sale price
  filter(
    dev > flag_yoy_pct_change_factor,
    !meta_pin %in% ratio_factor$meta_pin,
    !meta_pin %in% nbhd_factor$meta_pin
  ) %>%
  arrange(desc(dev)) %>%
  mutate(rank = row_number()) %>%
  select(-dev)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Cleanup and Save ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Combine flagged values into a single data frame and clean up the output
# for export to CSV/Excel
flagged_sales <- bind_rows(
  ratio_factor,
  nbhd_factor,
  yoy_change_factor
) %>%
  mutate(
    meta_pin = pin_format_pretty(meta_pin),
    `Township Name` = town_convert(meta_town_code)
  ) %>%
  select(-sales_ratio_from_nbhd_med) %>%
  relocate(`Township Name`, .after = "meta_year") %>%
  vars_recode(type = "long") %>%
  vars_rename(names_from = "standard", names_to = "pretty") %>%
  rename(
    `Predicted 2021 Value` = model_value,
    `Reason for Flag` = reason,
    `Sale Ratio` = sales_ratio
  ) %>%
  arrange(rank) %>%
  mutate(`Severity Rank` = row_number()) %>%
  select(-rank)

# Export final data frame to CSV shared with Valuation dept.
write.csv(
  x = flagged_sales,
  file = here("output", "data", "flagged_sales.csv"),
  row.names = FALSE,
  na = ""
)

# Plot spatial distribution of largest outliers
flagged_sales %>%
  filter(!is.na(Latitude) & `Severity Rank` <= 1000) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
ggplot() +
  geom_sf(data = town_shp) +
  geom_sf(aes(geometry = geometry), color = "blue", alpha = 0.2) +
  theme_void() +
  theme(
    strip.text = element_text(size = 12)
  ) +
  ggtitle("Spatial Distribution of Sales Flagged as Outliers (Worst 1000)")

# Export plot image to PNG
ggsave(here("output", "reports", "outlier_map.png"), plot = last_plot())

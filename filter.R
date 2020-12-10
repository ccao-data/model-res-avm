# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Setup ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load R libraries
library(arrow) 
library(ccao)
library(dplyr)
library(ggplot2)
library(here)
library(purrr)
library(recipes)
library(sf)
library(stringr)
library(tidymodels)
library(treesnip)
source("R/model_funs.R")
source("R/valuation_funs.R")




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Valuation ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load the final stacked model object from file
sm_final_full_fit <- model_load(here("output", "models", "stacked_model.zip"))

# Get all arms-length sales
sales_data <- read_parquet(here("input", "modeldata.parquet")) %>%
  filter(ind_arms_length) %>%
  mutate(
    meta_pin = str_pad(meta_pin, 14, "left", "0"),
    meta_class = str_pad(meta_class, 3, "left", "0"),
    meta_multi_code = as.character(meta_multi_code)
  )

# Get predicted values for all sales
sales_preds <- bind_cols(
  bake(sm_final_full_fit$recipes$lgbm, sales_data, has_role("id")), 
  predict(
    object = sm_final_full_fit,
    new_data = sales_data,
    prepped_recipe = sm_final_full_fit$recipes$lgbm
  )
) %>%
  mutate(
    meta_pin = str_pad(meta_pin, 14, "left", "0"),
    meta_class = str_pad(meta_class, 3, "left", "0"),
    meta_multi_code = as.character(meta_multi_code),
    meta_document_num = str_pad(meta_document_num, 11, "left", "0")
  )

# Join predicted values back onto the sales data and cleanup
sales_data <- sales_data %>%
  left_join(
    sales_preds,
    by = c("meta_pin", "meta_class", "meta_multi_code", "meta_document_num")
  ) %>%
  mutate(
    meta_nbhd = str_pad(meta_nbhd, 3, "left", "0"),
    meta_sale_price = na_if(meta_sale_price, 0)
  )




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Flagging ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Select relevant columns for output
sales_filtered <- sales_data %>%
  select(
    meta_pin, meta_document_num, meta_class, meta_year, meta_town_code, meta_nbhd, 
    geo_property_address, geo_property_apt_no, geo_property_city, geo_property_zip,
    geo_longitude, geo_latitude,
    model_value = stack, meta_sale_price, meta_nbhd_avg
  ) %>%
  
  # Calculate ratios
  mutate(
    sales_ratio = model_value / meta_sale_price,
    nbhd_avg_pred_ratio = model_value / meta_nbhd_avg
  )

# Filter for very high and very low ratios
ratio_factor <- sales_filtered %>% 
  filter(sales_ratio < 0.33 | sales_ratio > 3) %>%
  
  # Invert ratios in order to rank by magnitude
  mutate(
    adj_sales_ratio = ifelse(sales_ratio < 1, 1 / sales_ratio, sales_ratio),
    reason = "Estimate differs from sale price by factor of 3 or greater"
  ) %>%
  arrange(desc(adj_sales_ratio)) %>%
  mutate(rank = row_number()) %>%
  select(-adj_sales_ratio)

# Filter for large devations from the neighborhood average sale price
nbhd_avg_factor <- sales_filtered %>% 
  filter(
    (nbhd_avg_pred_ratio < 0.33 | nbhd_avg_pred_ratio > 3),
    !meta_pin %in% ratio_factor
  ) %>%
  
  # Invert ratios in order to rank by magnitude
  mutate(
    avg_nbhd_ratio = ifelse(nbhd_avg_pred_ratio < 1, 1 / nbhd_avg_pred_ratio, nbhd_avg_pred_ratio),
    reason = "Estimate differs from neighborhood average sale price by factor of 3 or greater") %>%
  arrange(desc(avg_nbhd_ratio)) %>%
  mutate(rank = row_number()) %>%
  select(-avg_nbhd_ratio)

# Find properties with high year-over-year change in sale price
yoy_change_factor <- sales_filtered %>% 
  group_by(meta_pin) %>% 
  
  # Calculate statistic as ratio between range of sale prices and their mean
  mutate(
    r = max(meta_sale_price) - min(meta_sale_price), 
    m = mean(meta_sale_price),
    dev = r / m
  ) %>%
  select(-r, -m) %>%
  ungroup() %>%
  mutate(reason = "High year-over-year change in sale price within 5 years") %>%
  
  # Filter for large YoY changes
  filter(
    dev > 1.6,
    !meta_pin %in% ratio_factor$meta_pin,
    !meta_pin %in% nbhd_avg_factor$meta_pin
  ) %>%
  arrange(desc(dev)) %>%
  mutate(rank = row_number()) %>%
  select(-dev)

# Combine flagged values into a single data frame
flagged_sales <- bind_rows(
    ratio_factor,
    nbhd_avg_factor,
    yoy_change_factor
  ) %>%
  mutate(
    meta_pin = pin_format_pretty(meta_pin),
    `Township Name` = town_convert(meta_town_code)
  ) %>%
  select(-nbhd_avg_pred_ratio) %>%
  relocate(`Township Name`, .after = "meta_year") %>%
  relocate(meta_nbhd_avg, .after = "sales_ratio") %>%
  vars_recode(type = "long") %>%
  vars_rename(names_from = "standard", names_to = "pretty") %>%
  rename(
    `Predicted 2021 Value` = model_value,
    `Reason for Flag` = reason,
    `Sale Ratio` = sales_ratio,
    PIN = `Property Identification Number`,
    `Docket #` = `Sale Deed Number`
  ) %>%
  arrange(rank) %>%
  mutate(`Severity Rank` = row_number()) %>%
  select(-rank)

# Plot spatial distribution of outliers
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

# Export image to PNG
ggsave(here("output", "reports", "outlier_map.png"), plot = last_plot())

# Export table to CSV
write.csv(
  x = flagged_sales,
  file = here("output", "data", "flagged_sales.csv"),
  row.names = FALSE,
  na = ""
)

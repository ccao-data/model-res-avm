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

# Load helper functions from file
source("R/model_funs.R")




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Valuation ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load the final lightgbm model object and recipe from file
lgbm_final_full_fit <- model_load(here("output", "models", "lgbm_model.zip"))
lgbm_final_full_recipe <- readRDS(here("output", "models", "lgbm_recipe.rds"))

# Get all arms-length sales and clean up some string columns
sales_data <- read_parquet(here("input", "modeldata.parquet")) %>%
  filter(ind_arms_length) %>%
  mutate(
    meta_pin = str_pad(meta_pin, 14, "left", "0"),
    meta_class = str_pad(meta_class, 3, "left", "0"),
    meta_multi_code = as.character(meta_multi_code),
    meta_document_num = str_pad(meta_document_num, 11, "left", "0")
  )

# Generate predicted values for all sales using the saved model and recipe
sales_data <- sales_data %>%
  mutate(
    lgbm = model_predict(
      spec = lgbm_final_full_fit,
      recipe = lgbm_final_full_recipe,
      data = .
    )
  )




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Flagging ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# The goal here is to flag sales which deviate significantly from their
# predicted value, neighborhood, or previous sale price. These sales are 
# hand-reviewed by the CCAO and excluded from the model if they are atypical or
# not arms-length. Not all sales flagged here will be excluded

# Select relevant output columns
sales_filtered <- sales_data %>%
  select(
    meta_pin, meta_document_num, meta_class, meta_year, meta_town_code, meta_nbhd, 
    geo_property_address, geo_property_apt_no, geo_property_city, geo_property_zip,
    geo_longitude, geo_latitude,
    model_value = lgbm, meta_sale_price, meta_nbhd_avg
  ) %>%
  
  # Calculate sales ratio and sale ratio based on neighborhood average sale price
  mutate(
    sales_ratio = model_value / meta_sale_price,
    nbhd_avg_pred_ratio = model_value / meta_nbhd_avg
  )

# Flag very high and very low ratios (anything that deviates from its prediction
# be a factor of 3 or more)
ratio_factor <- sales_filtered %>% 
  filter(sales_ratio < 0.33 | sales_ratio > 3) %>%
  
  # Invert ratios in order to rank by magnitude of error
  mutate(
    adj_sales_ratio = ifelse(sales_ratio < 1, 1 / sales_ratio, sales_ratio),
    reason = "Estimate differs from sale price by factor of 3 or greater"
  ) %>%
  arrange(desc(adj_sales_ratio)) %>%
  mutate(rank = row_number()) %>%
  select(-adj_sales_ratio)

# Flag large deviations from the neighborhood average sale price
nbhd_avg_factor <- sales_filtered %>% 
  filter(
    (nbhd_avg_pred_ratio < 0.33 | nbhd_avg_pred_ratio > 3),
    !meta_pin %in% ratio_factor
  ) %>%
  
  # Invert ratios in order to rank by magnitude of error
  mutate(
    avg_nbhd_ratio = ifelse(nbhd_avg_pred_ratio < 1, 1 / nbhd_avg_pred_ratio, nbhd_avg_pred_ratio),
    reason = "Estimate differs from neighborhood average sale price by factor of 3 or greater") %>%
  arrange(desc(avg_nbhd_ratio)) %>%
  mutate(rank = row_number()) %>%
  select(-avg_nbhd_ratio)

# Flag properties with a high year-over-year change in sale price
yoy_change_factor <- sales_filtered %>% 
  group_by(meta_pin) %>% 
  
  # Calculate ratio between range of sale prices and their mean for each PIN
  mutate(
    r = max(meta_sale_price) - min(meta_sale_price), 
    m = mean(meta_sale_price),
    dev = r / m
  ) %>%
  select(-r, -m) %>%
  ungroup() %>%
  mutate(reason = "High year-over-year change in sale price within 5 years") %>%
  
  # Filter for large YoY changes / large deviations from the mean sale price
  filter(
    dev > 1.6,
    !meta_pin %in% ratio_factor$meta_pin,
    !meta_pin %in% nbhd_avg_factor$meta_pin
  ) %>%
  arrange(desc(dev)) %>%
  mutate(rank = row_number()) %>%
  select(-dev)

# Combine flagged values into a single data frame and clean up the output
# for export to CSV/Excel
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


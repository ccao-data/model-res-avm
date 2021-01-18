# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Setup ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load R libraries
library(arrow)
library(assessr)
library(beepr)
library(ccao)
library(dplyr)
library(here)
library(purrr)
library(recipes)
library(stringr)
library(tictoc)
library(tidymodels)
library(treesnip)

# Load helper functions from file
source(here("R", "model_funs.R"))
source(here("R", "valuation_funs.R"))

# Start full script timer
tictoc::tic(msg = "Full Valuation Complete!")




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Valuation ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load the final lightgbm model object and recipe from file
lgbm_final_full_fit <- model_load(here("output", "models", "lgbm_model.zip"))
lgbm_final_full_recipe <- readRDS(here("output", "models", "lgbm_recipe.rds"))

# Post-modeling adjustments (such as ratio capping) require sales to work.
# As such, we need to append the most recent sale (closest to the assessment
# date within 2 years) to each PIN. Not all PINs will have sales, as our sales
# sample is limited to 7 years prior to the assessment date
sales_data_prev_2_years <- read_parquet(here("input", "modeldata.parquet")) %>%
  filter(ind_arms_length, meta_year >= max(meta_year) - 1) %>%
  group_by(meta_pin) %>%
  filter(meta_sale_date == max(meta_sale_date)) %>%
  distinct(meta_pin, meta_sale_price, meta_document_num) %>%
  ungroup()

# Load the full set of residential properties that need values, join the sales
# where available
assmntdata <- read_parquet(here("input", "assmntdata.parquet")) %>%
  left_join(sales_data_prev_2_years, by = "meta_pin")

# Generate predictions for all assessment data using the lightgbm model
assmntdata <- assmntdata %>%
  mutate(
    lgbm = model_predict(
      spec = lgbm_final_full_fit,
      recipe = lgbm_final_full_recipe,
      data = .
    )
  ) 




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Valuation for HIEs (288s) ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# For the subset of properties with Home Improvement Exemptions (288s), we value
# their improved version separately, then compare to the unimproved version to
# see if the improvement adds more than $75K value. If it does, then then
# remainder is added to the original unimproved value
hiedata <- read_parquet(here("input", "hiedata.parquet")) %>%
  left_join(sales_data_prev_2_years, by = "meta_pin") %>%
  mutate(
    lgbm_w_addchars = model_predict(
      spec = lgbm_final_full_fit,
      recipe = lgbm_final_full_recipe,
      data = .
    )
  ) %>%
  select(meta_pin, meta_year, meta_class, meta_multi_code, lgbm_w_addchars)

# Calculate difference between improved and non-improved property, if the
# difference exceeds the cap, add the difference minus the cap to the original
# property value
assmntdata <- assmntdata %>%
  left_join(hiedata) %>%
  mutate(
    diff = replace_na(lgbm_w_addchars - lgbm, 0),
    meta_288_exceeds_cap = diff > 75000,
    lgbm = ifelse(meta_288_exceeds_cap, lgbm + (diff - 75000), lgbm)
  ) %>%
  select(-diff, -lgbm_w_addchars)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Post-Valuation Model ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# To correct for model and data bias, we implement an additional post-modeling
# process which attempts to correct skew, fix outliers, etc. The adjustments
# generate an additional model object which must also be applied to new data
# in order to get final, first-pass model values

# Create post-valuation model object to save aggregate adjustments. Here we are
# capping large sales ratios, shifting distributions within neighborhood and
# modeling group, and averaging values for identical townhomes
pv_model <- postval_model(
  data = assmntdata,
  truth = meta_sale_price,
  estimate = lgbm,
  class = meta_class,
  ntile_group_cols = c("meta_town_code", "meta_nbhd", "meta_modeling_group"),
  ntile_probs = c(0.2, 0.4, 0.6, 0.8),
  ntile_min_sales = 15,
  ntile_min_turnover = 0.09,
  townhome_group_cols = c(
    "meta_town_code", "meta_class", "char_age", "char_bsmt", "char_rooms",
    "char_gar1_size", "char_attic_fnsh", "char_bldg_sf", "char_beds"
  ),
  townhome_min_sales = 5
)

# Save postval model to file so it can be used for any future predictions
pv_model %>%
  saveRDS(here("output", "models", "postval_model.rds"))

# Applied the postval model to initial lightgbm predictions to get final
# predicted values. Keep only the columns needed for final output
pv_final_values <- assmntdata %>%
  mutate(final_value = predict(pv_model, ., meta_sale_price, lgbm)) %>%
  rename(lgbm_value = lgbm) %>%
  group_by(meta_pin) %>% 
  mutate(ind_multi_pin = n() > 1) %>%
  ungroup()

# Load most recent year of sales, taking the most recent sale within the year
sales_data <- read_parquet(here("input", "modeldata.parquet")) %>%
  filter(ind_arms_length, meta_year == max(meta_year)) %>%
  group_by(meta_pin) %>%
  filter(meta_sale_date == max(meta_sale_date)) %>%
  select(meta_pin, meta_year, meta_class, meta_sale_price) %>%
  ungroup()

# Attach only the most recent year of sales to the final values for the
# purpose of reporting and sales ratio study. The 2 years of sales for the post-
# modeling adjustment are tossed out here
pv_final_values <- pv_final_values %>%
  select(-meta_sale_price) %>%
  left_join(sales_data, by = c("meta_pin", "meta_year", "meta_class")) %>%
  ccao::recp_clean_relocate()

# Save final values to file so they can be uploaded to AS/400 or Tyler iasWorld
pv_final_values %>%
  write_parquet(here("output", "data", "finalvalues.parquet"))


### Generate reports

# Generate valuation performance/diagnostic report
rmarkdown::render(
  input = here("reports", "valuation_report.Rmd"),
  output_file = here("output", "reports", "valuation_report.html")
)

# Stop full script timer
tictoc::toc()

# BIG BEEP
beepr::beep(8)

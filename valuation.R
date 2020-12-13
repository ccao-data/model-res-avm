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
source("R/model_funs.R")
source("R/valuation_funs.R")

# Start full script timer
tictoc::tic(msg = "Full Valuation Complete!")




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Valuation ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load the final lightgbm model object and recipe from file
lgbm_final_full_fit <- model_load(here("output", "models", "lgbm_model.zip"))
lgbm_final_full_recipe <- readRDS(here("output", "models", "lgbm_recipe.rds"))

# Some post-modeling adjustments (such as ratio capping) require sales to work.
# As such, we need to append the most recent sale (closest to the assessment
# date) to each PIN. Not all PINs will have sales, as our sales sample is
# limited to 5 years prior to the assessment date
sales_data <- read_parquet(here("input", "modeldata.parquet")) %>%
  filter(ind_arms_length) %>%
  group_by(meta_pin) %>%
  filter(meta_sale_date == max(meta_sale_date)) %>%
  select(meta_pin, meta_sale_price, meta_document_num)

# Load the full set of residential properties that need values, join the sales
# where available
assmntdata <- read_parquet(here("input", "assmntdata.parquet")) %>%
  left_join(sales_data, by = "meta_pin")

# Generate predictions for all assessment data using the lightgbm model
assmntdata <- assmntdata %>%
  mutate(
    lgbm = model_predict(
      spec = lgbm_final_full_fit,
      recipe = lgbm_final_full_recipe,
      data = .
    )
  ) %>%
  mutate(
    meta_pin = str_pad(meta_pin, 14, "left", "0"),
    meta_nbhd = str_pad(meta_nbhd, 5, "left", "0"),
    meta_sale_price = na_if(meta_sale_price, 0)
  )




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Post-Valuation Model ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# To correct for model and data bias, we implement and additional post-modeling
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
  med_adj_cols = c("meta_town_code", "meta_nbhd", "meta_modeling_group"),
  townhome_adj_cols = c(
    "meta_town_code", "char_age", "char_bsmt", "char_rooms",
    "char_attic_fnsh", "char_bldg_sf", "char_beds"
  )
)

# Save postval model to file so it can be used for any future predictions
pv_model %>%
  saveRDS(here("output", "models", "postval_model.rds"))

# Applied the postval model to initial lightgbm predictions to get final
# predicted values. Keep only the columns needed for final output
pv_final_values <- assmntdata %>%
  mutate(
    meta_town_name = town_convert(meta_town_code),
    final_value = predict(pv_model, ., meta_sale_price, lgbm),
    prior_value = meta_est_land + meta_est_bldg
  ) %>%
  select(
    meta_pin, meta_year, meta_class, meta_town_code, meta_nbhd,
    meta_modeling_group, meta_multi_code, geo_latitude, geo_longitude,
    meta_sale_price, meta_sale_date, meta_document_num,
    prior_value,
    lgbm_value = lgbm, final_value
  )

# Save final values to file so they can be uploaded to AS/400 or Tyler iasWorld
pv_final_values %>%
  write_parquet(here("output", "data", "finalvalues.parquet"))

# Generate valuation diagnostic/performance report
rmarkdown::render(
  input = here("reports", "valuation_report.Rmd"),
  output_file = here("output", "reports", "valuation_report.html")
)

# Stop full script timer
tictoc::toc()

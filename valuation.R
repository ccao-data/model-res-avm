# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Setup ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load R libraries
options(tidymodels.dark = TRUE)
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
source("R/model_funs.R")
source("R/valuation_funs.R")

# Start full script timer
tictoc::tic(msg = "Full Valuation Complete!")

# Set seed for reproducibility
set.seed(27)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Valuation ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load the final LightGBM model object and recipe from file
lgbm_final_full_fit <- model_load(here("output", "models", "lgbm_model.zip"))
lgbm_final_full_recipe <- readRDS(here("output", "models", "lgbm_recipe.rds"))

# Get the most recent arms length sale for each property in the training dataset
sales_data <- read_parquet(here("input", "modeldata.parquet")) %>%
  filter(ind_arms_length) %>%
  group_by(meta_pin) %>%
  filter(meta_sale_date == max(meta_sale_date)) %>%
  select(meta_pin, meta_sale_price, meta_document_num)

# Load the full set of residential properties that need values, join the most
# recent sale and fill all missing sale prices with 0. This is a workaround 
# to prevent NA sales values from being removed when creating predictions
assmntdata <- read_parquet(here("input", "assmntdata.parquet")) %>%
  left_join(sales_data, by = "meta_pin")

# Generate predictions for all assessment data
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
    meta_nbhd = str_pad(meta_nbhd, 3, "left", "0"),
    meta_sale_price = na_if(meta_sale_price, 0)
  )




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Post-Valuation Model ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Create post-valuation model object that saves aggregate adjustments
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

# Save postval model to file so it can be loaded later for prediction
pv_model %>%
  saveRDS(here("output", "models", "postval_model.rds"))

# Get adjusted values for assessment data 
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
    prior_value, lgbm_value = lgbm, final_value
  )

# Save final values to file so they can be uploaded to AS/400
pv_final_values %>%
  write_parquet(here("output", "data", "finalvalues.parquet"))

# Generate valuation diagnostic/performance report
rmarkdown::render(
  input = here("reports", "valuation_report.Rmd"),
  output_file = here("output", "reports", "valuation_report.html")
)

# Stop full script timer
tictoc::toc()

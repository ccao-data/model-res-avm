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
library(purrr)
library(stringr)
library(tictoc)
library(recipes)

# Start full script timer
tictoc::tic(msg = "Full Valuation Complete!")

# Set seed for reproducibility
set.seed(27)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Valuation ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load the full set of residential properties that need values, dummy var for 
# sale price is added because it's necessary to work with recipes
assmntdata <- read_parquet(here("input", "assmntdata.parquet")) %>%
  mutate(meta_sale_price = 0)

# Load the final stacked model object if not already loaded
if (!exists("sm_final_full_fit")) {
  sm_final_full_fit <- readRDS(here("output", "models", "stacked_model.rds"))
}

# Get the subset of PINs that actually can be valued by the model (no missing
# or strange data). PINs that can't be modeled are removed via the prepped 
# recipe
assmntdata_preds <- bind_cols(
  bake(sm_final_full_fit$recipes$lgbm, assmntdata, has_role("id")),
  predict(
    object = sm_final_full_fit,
    new_data = assmntdata,
    prepped_recipe = sm_final_full_fit$recipes$lgbm
  )
) %>%
  mutate(meta_pin = str_pad(meta_pin, 14, "left", "0"))


# Join predicted values back onto the original data
assmntdata <- assmntdata %>%
  left_join(assmntdata_preds, by = "meta_pin")




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Post-Modeling ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -




### Step 2 - Adjust townhomes to ensure identical townhomes have same value


### Step 3 - Adjust median ratio of group by neighborhood


### Step 4 - Regressivity adjustment?


### Step 5 - Limit excessive ratios


### Step 6 - Identify historic/affordable housing


# TODO: Fix missing data in predictions and from meta model
# TODO: Adjust multi-property PINS
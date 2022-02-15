#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Setup ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Start the stage timer and clear logs from prior stage
tictoc::tic.clearlog()
tictoc::tic("Assess")

# Load libraries and scripts
library(arrow)
library(assessr)
library(ccao)
library(dplyr)
library(here)
library(lightsnip)
library(purrr)
library(recipes)
library(stringr)
library(tictoc)
library(tidyr)

# Load helpers and recipes from files
walk(list.files("R/", "\\.R$", full.names = TRUE), source)

# Initialize a dictionary of file paths and S3 URIs. See R/file_dict.csv
paths <- model_file_dict()

# Load the metadata file containing the run settings
metadata <- read_parquet(paths$output$metadata$local)

# Column prefixes to use for ratio study comparison
rsf_columns <- gsub("_tot", "", metadata$model_ratio_study_far_column)
rsn_columns <- gsub("_tot", "",metadata$model_ratio_study_near_column)

# Load the training data to use as a source of sales. These will be attached to
# PIN-level output (for comparison) and used as the basis for a sales ratio
# analysis on the assessment data
sales_data <- read_parquet(paths$input$training$local)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. Predict Values ------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load the final lightgbm model object and recipe from file
lgbm_final_full_fit <- lightsnip::lgbm_load(paths$output$workflow_fit$local)
lgbm_final_full_recipe <- readRDS(paths$output$workflow_recipe$local)

# Load the data for assessment. This is the universe of CARDs (not
# PINs) that needs values. Use the trained lightgbm model to estimate a single
# fair-market value for each card
assessment_data_pred <- read_parquet(paths$input$assessment$local) %>%
  as_tibble() %>%
  mutate(
    pred_card_initial_fmv = predict(
      lgbm_final_full_fit,
      new_data = bake(
        lgbm_final_full_recipe,
        new_data = .,
        all_predictors()
      )
    )$.pred
  )




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Post-Modeling Adjustments -------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

## 3.1. Multicards -------------------------------------------------------------

# Cards represent buildings/improvements. A PIN can have multiple cards, and
# the total taxable value of the PIN is (usually) the sum of all cards
assessment_data_mc <- assessment_data_pred %>%
  select(
    meta_pin, meta_class, char_bldg_sf, meta_card_num,
    meta_tieback_key_pin, meta_tieback_proration_rate,
    meta_1yr_pri_board_tot, pred_card_initial_fmv
  ) %>%
  # For prorated PINs with multiple cards, take the average of the
  # card (building) across PINs
  group_by(meta_tieback_key_pin, meta_card_num) %>%
  mutate(
    pred_card_intermediate_fmv = ifelse(
      is.na(meta_tieback_key_pin),
      pred_card_initial_fmv,
      mean(pred_card_initial_fmv)
    )
  ) %>%
  # Aggregate multi-cards to the PIN-level by summing the predictions
  # of all cards. We use a heuristic here to limit the PIN-level total
  # value, this is to prevent super-high-value back-buildings/ADUs from
  # blowing up the PIN-level AV
  group_by(meta_pin) %>%
  mutate(
    pred_pin_final_fmv = ifelse(
      sum(pred_card_intermediate_fmv) * meta_tieback_proration_rate <=
        metadata$model_pv_multicard_yoy_cap * first(meta_1yr_pri_board_tot * 10) |
        is.na(meta_1yr_pri_board_tot),
      sum(pred_card_intermediate_fmv),
      max(pred_card_intermediate_fmv)
    )
  )


## 3.2. Townhomes --------------------------------------------------------------

# For class 210 and 295s, we want all units in the same complex to
# have the same value (assuming they are identical)

# Load townhome/rowhome complex IDs
complex_id_data <- read_parquet(paths$input$complex_id$local) %>%
  select(meta_pin, meta_complex_id)

# Join complex IDs to the predictions, then for each complex, set the
# prediction to the average prediction of the complex. Also, multiply
# the PIN-level value by the PIN's proration rate
assessment_data_cid <- assessment_data_mc %>%
  left_join(complex_id_data, by = "meta_pin") %>%
  group_by(meta_complex_id, meta_tieback_proration_rate) %>%
  mutate(
    pred_pin_final_fmv = ifelse(
      is.na(meta_complex_id),
      pred_pin_final_fmv * meta_tieback_proration_rate,
      mean(pred_pin_final_fmv) * meta_tieback_proration_rate
    )
  ) %>%
  ungroup()


## 3.3. Prorate/Round ----------------------------------------------------------

# Round PIN-level predictions using the breaks and amounts specified in setup
assessment_data_final <- assessment_data_cid %>%
  mutate(
    pred_pin_final_fmv_round = ccao::val_round_fmv(
      pred_pin_final_fmv,
      breaks = metadata$model_pv_round_break[[1]],
      round_to = metadata$model_pv_round_to_nearest[[1]],
      type = metadata$model_pv_round_type
    )
  ) %>%
  # Apportion the final PIN-level value back out to the card-level using
  # the square footage of each improvement
  group_by(meta_pin) %>%
  mutate(
    meta_card_pct_total_fmv = char_bldg_sf / sum(char_bldg_sf),
    pred_card_final_fmv = pred_pin_final_fmv_round * meta_card_pct_total_fmv
  ) %>%
  ungroup()

# Merge the finalized card-level data back to the main tibble of predictions
assessment_data_merged <- assessment_data_pred %>%
  left_join(
    assessment_data_final %>%
      select(
        meta_pin, meta_card_num, meta_card_pct_total_fmv, meta_complex_id,
        pred_card_final_fmv, pred_pin_final_fmv, pred_pin_final_fmv_round
      ),
    by = c("meta_pin", "meta_card_num")
  ) %>%
  relocate(
    c(meta_card_pct_total_fmv, meta_complex_id),
    .after = "meta_card_num"
  ) %>%
  mutate(
    township_code = meta_township_code,
    meta_year = as.character(meta_year)
  )

# The test PINs below can be used to ensure that the order of operations
# for the adjustments above results in a sensible outcome:
# 17321110470000 05174150240000 05213220250000 08121220400000 06334030310000




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4. Card-Level Data -----------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Keep only card-level variables of interest, including: ID vars
# (run_id, pin, card), location (lat/lon), and predictions
assessment_data_merged %>%
  select(
    meta_pin, meta_class, meta_card_num, meta_card_pct_total_fmv,
    meta_complex_id, loc_longitude, loc_latitude,
    pred_card_initial_fmv, pred_card_final_fmv, township_code
  ) %>%
  write_parquet(paths$output$assessment_card$local)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 5. PIN-Level Data ------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Generate PIN-level stats for each run. These are used for desktop review,
# looking at YoY changes, comparing to sales, etc.

## 5.1. Load Sales/Land --------------------------------------------------------

# Keep the two most recent sales for each PIN. These are just for review, not
# for ratio studies
sales_data_two_most_recent <- sales_data %>%
  group_by(meta_pin) %>%
  slice_max(meta_sale_date, n = 2) %>%
  distinct(
    meta_pin, meta_year,
    meta_sale_price, meta_sale_date, meta_sale_document_num
  ) %>%
  mutate(mr = paste0("sale_recent_", row_number())) %>%
  tidyr::pivot_wider(
    id_cols = meta_pin,
    names_from = mr,
    values_from = c(meta_sale_date, meta_sale_price, meta_sale_document_num),
    names_glue = "{mr}_{gsub('meta_sale_', '', .value)}"
  ) %>%
  select(meta_pin, contains("1"), contains("2")) %>%
  ungroup()

# Load land rates from file
land_site_rate <- read_parquet(
  paths$input$land_site_rate$local,
  c("meta_pin", "land_rate_per_pin")
)
land_nbhd_rate <- read_parquet(
  paths$input$land_nbhd_rate$local
)


## 5.2. Collapse to PIN Level --------------------------------------------------

# Collapse card-level data to the PIN level, keeping the largest building on
# each PIN but summing the total square footage of all buildings
assessment_data_pin <- assessment_data_merged %>%
  group_by(meta_year, meta_pin) %>%
  arrange(desc(char_bldg_sf)) %>%
  # Rename prior year comparison columns to near/far to maintain consistent
  # column names in Athena
  rename_with(
    .fn = ~ gsub(paste0(rsn_columns, "_"), "prior_near_", .x),
    .cols = starts_with(rsn_columns)
  ) %>%
  rename_with(
    .fn = ~ gsub(paste0(rsf_columns, "_"), "prior_far_", .x),
    .cols = starts_with(rsf_columns)
  ) %>%
  summarize(
    across(
      c(
        # Keep ID and meta variables
        meta_triad_code, meta_township_code, meta_nbhd_code, meta_tax_code,
        meta_class, meta_tieback_key_pin, meta_tieback_proration_rate,
        meta_cdu, meta_pin_num_cards, meta_pin_num_landlines, meta_complex_id,

        # Keep certain vital characteristics for the largest card on the PIN
        char_yrblt, char_land_sf, char_ext_wall, char_type_resd,

        # Keep locations, prior year values, and indicators
        starts_with(c(
          "loc_property_", "loc_cook_", "loc_chicago_",
          "loc_census", "loc_school_", "prior_", "ind_"
        )),

        # Keep PIN-level predicted values
        pred_pin_final_fmv, pred_pin_final_fmv_round, township_code
      ),
      first
    ),

    # Keep the sum of the initial card level values
    pred_pin_initial_fmv = sum(pred_card_initial_fmv),
    char_total_bldg_sf = sum(char_bldg_sf)
  ) %>%
  ungroup() %>%
  # Make a flag for any vital missing characteristics
  bind_cols(
    assessment_data_merged %>%
      select(
        meta_year, meta_pin,
        char_yrblt, char_bldg_sf, char_land_sf, char_beds,
        char_fbath, char_bsmt, char_ext_wall, char_apts
      ) %>%
      mutate(ind_char_missing_critical_value = rowSums(is.na(.))) %>%
      group_by(meta_year, meta_pin) %>%
      summarize(
        ind_char_missing_critical_value =
          sum(ind_char_missing_critical_value) > 0
      ) %>%
      ungroup() %>%
      select(ind_char_missing_critical_value)
  )


## 5.3. Value Land -------------------------------------------------------------

# Attach land and sales data to the PIN-level data, then calculate land and
# building values for each PIN
assessment_data_pin_2 <- assessment_data_pin %>%
  left_join(land_site_rate, by = "meta_pin") %>%
  left_join(land_nbhd_rate, by = c("meta_nbhd_code" = "meta_nbhd")) %>%
  left_join(sales_data_two_most_recent, by = "meta_pin") %>%
  # Land values are provided by Valuations and are capped at a percentage of the
  # total FMV for the PIN. For 210 and 295s (townhomes), there's a pre-
  # calculated land total value, for all other classes, there's a $/sqft rate
  mutate(
    pred_pin_final_fmv_land = case_when(
      !is.na(land_rate_per_pin) &
        (land_rate_per_pin > pred_pin_final_fmv_round *
           metadata$model_pv_land_pct_of_total_cap) ~
      pred_pin_final_fmv_round * metadata$model_pv_land_pct_of_total_cap,
      
      !is.na(land_rate_per_pin) ~ land_rate_per_pin,
      
      char_land_sf * land_rate_per_sqft >= pred_pin_final_fmv_round *
        metadata$model_pv_land_pct_of_total_cap ~
      pred_pin_final_fmv_round * metadata$model_pv_land_pct_of_total_cap,
      
      TRUE ~ char_land_sf * land_rate_per_sqft
    ),
    pred_pin_uncapped_fmv_land = case_when(
      !is.na(land_rate_per_pin) ~ land_rate_per_pin,
      TRUE ~ char_land_sf * land_rate_per_sqft
    ),
    pred_pin_final_fmv_bldg =
      pred_pin_final_fmv_round - pred_pin_final_fmv_land
  ) %>%
  # Calculate effective rates (rate with 50% cap) + the % of the PIN value
  # dedicated to the building
  mutate(
    pred_pin_land_rate_effective = pred_pin_final_fmv_land / char_land_sf,
    pred_pin_bldg_rate_effective = pred_pin_final_fmv_bldg / char_total_bldg_sf,
    pred_pin_land_pct_total = pred_pin_final_fmv_land / pred_pin_final_fmv_round
  ) %>%
  # Convert prior values to FMV from AV, then calculate year-over-year
  # percent and nominal changes
  mutate(
    across(starts_with("prior_"), ~ .x * 10),
    prior_far_yoy_change_nom = pred_pin_final_fmv_round - prior_far_tot,
    prior_far_yoy_change_pct = prior_far_yoy_change_nom / prior_far_tot,
    prior_near_yoy_change_nom = pred_pin_final_fmv_round - prior_near_tot,
    prior_near_yoy_change_pct = prior_near_yoy_change_nom / prior_near_tot
  )


## 5.4. Add Flags --------------------------------------------------------------

# Flags are used for identifying PINs for potential desktop review
assessment_data_pin_final <- assessment_data_pin_2 %>%
  # Rename existing indicators to flags
  rename_with(~ gsub("ind_", "flag_", .x), starts_with("ind_")) %>%
  # Add flag for potential proration issues (rates don't sum to 1)
  group_by(meta_tieback_key_pin) %>%
  mutate(flag_proration_sum_not_1 = ifelse(
    !is.na(meta_tieback_key_pin),
    sum(meta_tieback_proration_rate) != 1,
    FALSE
  )) %>%
  ungroup() %>%
  # Flag for capped land value
  mutate(
    flag_land_value_capped = pred_pin_final_fmv_round *
      metadata$model_pv_land_pct_of_total_cap == pred_pin_final_fmv_land
  ) %>%
  # Flags for changes in values
  mutate(
    flag_prior_near_to_pred_unchanged =
      prior_near_tot >= pred_pin_final_fmv_round - 100 &
        prior_near_tot <= pred_pin_final_fmv_round + 100,
    flag_pred_initial_to_final_changed = ccao::val_round_fmv(
      pred_pin_initial_fmv,
      breaks = metadata$model_pv_round_break[[1]],
      round_to = metadata$model_pv_round_to_nearest[[1]],
      type = metadata$model_pv_round_type
    ) != pred_pin_final_fmv_round,
    flag_prior_near_yoy_inc_gt_50_pct = prior_near_yoy_change_pct > 0.5,
    flag_prior_near_yoy_dec_gt_5_pct = prior_near_yoy_change_pct < -0.05,
  ) %>%
  # Flag high-value properties from prior years
  group_by(meta_township_code) %>%
  mutate(flag_prior_near_fmv_top_decile = ntile(prior_near_tot, 10) == 10) %>%
  ungroup() %>%
  # Flags for HIEs / 288s (placeholder until 288 data is integrated)
  mutate(
    flag_hie_num_active = 0,
    flag_hie_num_expired = 0,
    meta_pin_num_landlines = tidyr::replace_na(meta_pin_num_landlines, 1),
    flag_pin_is_multiland = tidyr::replace_na(flag_pin_is_multiland, FALSE)
  )


## 5.5. Clean/Reorder/Save -----------------------------------------------------

# Recode characteristics from numeric encodings to human-readable strings
assessment_data_pin_final %>%
  ccao::vars_recode(
    cols = starts_with("char_"),
    type = "short",
    as_factor = FALSE
  ) %>%
  # Reorder columns into groups by prefix
  select(
    starts_with(c("meta_", "loc_")), char_yrblt, char_total_bldg_sf,
    char_ext_wall, char_type_resd, char_land_sf,
    starts_with(c("land", "prior_far_", "prior_near_")),
    pred_pin_initial_fmv, pred_pin_final_fmv, pred_pin_final_fmv_bldg,
    pred_pin_final_fmv_land, pred_pin_final_fmv_round,
    pred_pin_bldg_rate_effective, pred_pin_land_rate_effective,
    pred_pin_land_pct_total, starts_with(c("sale_", "flag_")), township_code
  ) %>%
  write_parquet(paths$output$assessment_pin$local)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 6. Performance Data ----------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Data saved temporarily for use in performance evaluation. The most recent sale
# in the assessment year is used for the ratio study/performance eval

# Load the MOST RECENT sale per PIN for the same year as the assessment data.
# We want our assessed value to be as close as possible to the most recent sale
sales_data_most_recent <- sales_data %>%
  filter(meta_year == metadata$model_assessment_data_year) %>%
  group_by(meta_pin) %>%
  filter(meta_sale_date == max(meta_sale_date)) %>%
  distinct(
    meta_pin, meta_year, meta_sale_price,
    meta_sale_date, meta_sale_document_num
  ) %>%
  ungroup()

# Join sales data to each PIN, then collapse the card-level assessment
# data to the PIN level, summing the predicted value for multicard PINs. Keep
# only columns needed for performance calculations. This data is used for
# performance measurement only
assessment_data_merged %>%
  group_by(meta_year, meta_pin) %>%
  summarize(
    char_bldg_sf = sum(char_bldg_sf),
    pred_pin_final_fmv_round = first(pred_pin_final_fmv_round),
    across(
      c(
        meta_triad_code, meta_township_code, meta_nbhd_code,
        starts_with(c(rsf_columns, rsn_columns)),
        starts_with(c("loc_cook_", "loc_chicago_", "loc_census", "loc_school_"))
      ),
      first
    )
  ) %>%
  ungroup() %>%
  left_join(sales_data_most_recent, by = c("meta_year", "meta_pin")) %>%
  write_parquet(paths$intermediate$assessment$local)

# End the stage timer and append the time elapsed to a temporary file
tictoc::toc(log = TRUE)
arrow::read_parquet(paths$intermediate$timing$local) %>%
  bind_rows(., tictoc::tic.log(format = FALSE)) %>%
  arrow::write_parquet(paths$intermediate$timing$local)

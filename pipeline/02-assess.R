#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Setup ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# NOTE: See DESCRIPTION for library dependencies and R/setup.R for
# variables used in each pipeline stage

# NOTE: This script uses the data.table package for some transformations instead
# of dplyr for performance reasons (~10x speed up vs dplyr for group operations)

# Start the stage timer and clear logs from prior stage
tictoc::tic.clearlog()
tictoc::tic("Assess")

# Load libraries, helpers, and recipes from files
purrr::walk(list.files("R/", "\\.R$", full.names = TRUE), source)

# Columns to use for ratio study comparison (by prefix)
rsf_prefix <- gsub("_tot", "", params$ratio_study$far_column)
rsn_prefix <- gsub("_tot", "", params$ratio_study$near_column)

# Load the training data to use as a source of sales. These will be attached to
# PIN-level output (for comparison) and used as the basis for a sales ratio
# analysis on the assessment data
sales_data <- read_parquet(paths$input$training$local) %>%
  filter(!sv_is_outlier) %>%
  setDT()

# Load land rates from file
land_site_rate <- read_parquet(
  paths$input$land_site_rate$local,
  c("meta_pin", "land_rate_per_pin")
)
land_nbhd_rate <- read_parquet(
  paths$input$land_nbhd_rate$local
)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. Predict Values ------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Predicting off-market values with trained model")

# Load the final lightgbm model object and recipe from file
lgbm_final_full_fit <- lightsnip::lgbm_load(paths$output$workflow_fit$local)
lgbm_final_full_recipe <- readRDS(paths$output$workflow_recipe$local)

# Load the data for assessment. This is the universe of CARDs (not
# PINs) that needs values. Use the trained lightgbm model to estimate a single
# fair-market value for each card
assessment_card_data_pred <- read_parquet(paths$input$assessment$local) %>%
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
  ) %>%
  setDT()




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Post-Modeling Adjustments -------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Performing post-modeling adjustments")

## 3.1. Multicards -------------------------------------------------------------
message("Fixing multicard PINs")

# Cards represent buildings/improvements. A PIN can have multiple cards, and
# the total taxable value of the PIN is (usually) the sum of all cards
assessment_card_data_mc <- assessment_card_data_pred[, .(
  meta_year, meta_pin, meta_nbhd_code, meta_class, meta_card_num,
  char_bldg_sf, char_land_sf,
  meta_tieback_key_pin, meta_tieback_proration_rate,
  meta_1yr_pri_board_tot, pred_card_initial_fmv
)][
  # For prorated PINs with multiple cards, take the average of the card
  # (building) across PINs. This is because the same prorated building spread
  # across multiple PINs sometimes receives different values from the model
  !is.na(meta_tieback_key_pin),
  pred_card_intermediate_fmv := mean(pred_card_initial_fmv),
  by = .(meta_tieback_key_pin, meta_card_num)
][
  is.na(meta_tieback_key_pin),
  pred_card_intermediate_fmv := pred_card_initial_fmv
][
  # Aggregate multi-cards to the PIN-level by summing the predictions
  # of all cards. We use a heuristic here to limit the PIN-level total
  # value, this is to prevent super-high-value back-buildings/ADUs from
  # blowing up the PIN-level AV
  ,
  pred_pin_card_sum := fifelse(
    sum(pred_card_intermediate_fmv) * meta_tieback_proration_rate <=
      params$pv$multicard_yoy_cap *
        data.table::first(meta_1yr_pri_board_tot * 10) |
      is.na(meta_1yr_pri_board_tot) | .N != 2,
    sum(pred_card_intermediate_fmv),
    max(pred_card_intermediate_fmv)
  ),
  by = .(meta_pin)
]




## 3.2. Townhomes --------------------------------------------------------------
message("Averaging townhome complex predictions")

# For class 210 and 295s, we want all units in the same complex to
# have the same value (assuming they are nearly identical)

# Load townhome/rowhome complex IDs
complex_id_data <- setDT(read_parquet(paths$input$complex_id$local))
complex_id_data <- complex_id_data[, .(meta_pin, meta_complex_id)]

# Join complex IDs to the predictions, then for each complex, set the
# prediction to the average prediction of the complex
assessment_card_data_cid <- copy(assessment_card_data_mc)[
  complex_id_data,
  meta_complex_id := i.meta_complex_id,
  on = .(meta_pin)
][
  is.na(meta_complex_id),
  pred_pin_final_fmv := pred_pin_card_sum
][
  !is.na(meta_complex_id),
  pred_pin_final_fmv := mean(pred_pin_card_sum),
  by = .(meta_complex_id, meta_tieback_proration_rate)
]


## 3.3. Round ------------------------------------------------------------------
message("Rounding predictions")

# Round PIN-level predictions using the breaks and amounts specified in params
assessment_card_data_round <- copy(assessment_card_data_cid)[
  ,
  pred_pin_final_fmv_round_no_prorate := ccao::val_round_fmv(
    pred_pin_final_fmv,
    breaks = params$pv$round_break,
    round_to = params$pv$round_to_nearest,
    type = params$pv$round_type
  )
]




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4. Value Land ----------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Valuing land")

# Land values are provided by Valuations and are capped at a percentage of the
# total FMV for the PIN. For 210 and 295s (townhomes), there's sometimes a pre-
# calculated land total value, for all other classes, there's a $/sqft rate
assessment_pin_data_w_land <- unique(
  assessment_card_data_round,
  by = c("meta_year", "meta_pin")
)[
  ,
  .(
    meta_year, meta_pin, meta_nbhd_code, meta_complex_id,
    meta_tieback_key_pin, meta_tieback_proration_rate,
    char_land_sf, pred_pin_final_fmv, pred_pin_final_fmv_round_no_prorate
  )
][
  land_site_rate,
  land_rate_per_pin := i.land_rate_per_pin,
  on = .(meta_pin)
][
  land_nbhd_rate,
  land_rate_per_sqft := i.land_rate_per_sqft,
  on = .(meta_nbhd_code = meta_nbhd)
][
  ,
  pred_pin_final_fmv_land := ceiling(fcase(
    # nolint start
    # Use fixed, flat, per site land values first. If the fixed flat land value
    # exceeds the % of total FMV cap, set land FMV to the cap
    !is.na(land_rate_per_pin) &
      (land_rate_per_pin > pred_pin_final_fmv_round_no_prorate * params$pv$land_pct_of_total_cap),
    pred_pin_final_fmv_round_no_prorate * params$pv$land_pct_of_total_cap,

    # Otherwise, use the uncapped fixed land value
    !is.na(land_rate_per_pin),
    land_rate_per_pin,

    # If no fixed site value exists, use the $/sqft rates. Again check against
    # the % of FMV cap
    char_land_sf * land_rate_per_sqft >= pred_pin_final_fmv_round_no_prorate * params$pv$land_pct_of_total_cap,
    pred_pin_final_fmv_round_no_prorate * params$pv$land_pct_of_total_cap,

    # If the total land value doesn't exceed the cap, then just use the $/sqft
    rep(TRUE, .N),
    char_land_sf * land_rate_per_sqft
    # nolint end
  ))
][
  ,
  # Keep the uncapped value for display in desk review
  pred_pin_uncapped_fmv_land := ceiling(fcase(
    !is.na(land_rate_per_pin),
    land_rate_per_pin,
    rep(TRUE, .N),
    char_land_sf * land_rate_per_sqft
  ))
]




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 5. Prorate and Reapportion ---------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Prorating buildings")

# Prorating is the process of dividing a building's value among multiple PINs.
# See the steps outlined below for the process to determine a prorated value:
assessment_pin_data_prorated <- copy(assessment_pin_data_w_land)[
  !is.na(meta_tieback_key_pin),
  tieback_total_land_fmv := sum(pred_pin_final_fmv_land),
  by = .(meta_tieback_key_pin)
][
  is.na(meta_tieback_key_pin),
  tieback_total_land_fmv := pred_pin_final_fmv_land,
][
  ,
  # 1. Subtract the TOTAL value of the land of all linked PINs. This leaves
  # only the value of the building that spans the PINs
  pred_pin_final_fmv_bldg_no_prorate :=
    pred_pin_final_fmv_round_no_prorate - tieback_total_land_fmv
][
  ,
  # 2. Multiply the building by the proration rate of each PIN/card. This is
  # the proportion of the building's value held by each PIN
  pred_pin_final_fmv_bldg :=
    pred_pin_final_fmv_bldg_no_prorate * meta_tieback_proration_rate,
][
  ,
  temp_bldg_frac_prop :=
    pred_pin_final_fmv_bldg - as.integer(pred_pin_final_fmv_bldg)
][
  order(meta_tieback_key_pin, -temp_bldg_frac_prop),
][
  ,
  # 3. Assign the fractional portion of a building (cents) to whichever portion
  # is largest i.e. [1.59, 1.41] becomes [2, 1]
  temp_add_to_final := as.numeric(
    .N > 1 & seq_len(.N) == 1 & temp_bldg_frac_prop > 0.1e-7
  ),
  by = meta_tieback_key_pin
][
  ,
  temp_add_diff := temp_add_to_final * round(
    sum(pred_pin_final_fmv_bldg, na.rm = TRUE) -
      sum(as.integer(pred_pin_final_fmv_bldg), na.rm = TRUE)
  ),
  by = .(meta_tieback_key_pin)
][
  ,
  pred_pin_final_fmv_bldg := as.integer(pred_pin_final_fmv_bldg) +
    temp_add_diff
][
  ,
  # 4. To get the total value of the individual PINs, add the individual land
  # value of the PINs back to the prorated building value
  pred_pin_final_fmv_round :=
    pred_pin_final_fmv_bldg + pred_pin_final_fmv_land
][
  ,
  c("temp_bldg_frac_prop", "temp_add_to_final", "temp_add_diff") := NULL
][
  order(meta_pin),
]

# Merge the final PIN-level data back to the main tibble of predictions
assessment_card_data_merged <- assessment_pin_data_prorated[
  ,
  .(
    meta_year, meta_pin, meta_complex_id,
    pred_pin_final_fmv, pred_pin_final_fmv_round_no_prorate,
    land_rate_per_pin, land_rate_per_sqft, pred_pin_uncapped_fmv_land,
    pred_pin_final_fmv_land, pred_pin_final_fmv_bldg_no_prorate,
    pred_pin_final_fmv_bldg, pred_pin_final_fmv_round
  )
][
  assessment_card_data_pred,
  on = .(meta_year, meta_pin)
][
  ,
  `:=`(
    township_code = meta_township_code,
    meta_year = as.character(meta_year)
  )
][
  ,
  # Apportion the final prorated PIN-level value back out to the card-level
  # using the square footage of each improvement
  meta_card_pct_total_fmv := char_bldg_sf / sum(char_bldg_sf),
  by = .(meta_year, meta_pin)
][
  # In cases where bldg sqft is missing (rare), fill evenly across cards
  is.na(meta_card_pct_total_fmv),
  meta_card_pct_total_fmv := 1 / .N,
  by = .(meta_year, meta_pin)
][
  ,
  pred_card_final_fmv := pred_pin_final_fmv_bldg * meta_card_pct_total_fmv,
][
  ,
  temp_card_frac_prop := pred_card_final_fmv - as.integer(pred_card_final_fmv)
][
  order(meta_year, meta_pin, -temp_card_frac_prop),
][
  ,
  # More fractional rounding to deal with card values being split into cents
  temp_add_to_final := as.numeric(
    .N > 1 & seq_len(.N) == 1 & temp_card_frac_prop > 0.1e-7
  ),
  by = .(meta_year, meta_pin)
][
  ,
  temp_add_diff := temp_add_to_final * (
    sum(pred_card_final_fmv, na.rm = TRUE) -
      sum(as.integer(pred_card_final_fmv), na.rm = TRUE)
  ),
  by = .(meta_year, meta_pin)
][
  ,
  pred_card_final_fmv := round(as.integer(pred_card_final_fmv) + temp_add_diff)
][
  ,
  c("temp_card_frac_prop", "temp_add_to_final", "temp_add_diff") := NULL
]

# The test PINs below can be used to ensure that the order of operations
# for the adjustments above results in a sensible outcome:
# 17321110470000 05174150240000 05213220250000 08121220400000 06334030310000
# 16071280240000 17223100350000 30201160060000 16071280240000 25293010470000




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 6. Card-Level Data -----------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Saving card-level data")

# Keep only card-level variables of interest, including: ID variables (run_id,
# pin, card), characteristics, and predictions
assessment_card_data_merged %>%
  as_tibble() %>%
  select(
    meta_year, meta_pin, meta_class, meta_card_num, meta_card_pct_total_fmv,
    meta_complex_id, pred_card_initial_fmv, pred_card_final_fmv,
    all_of(params$model$predictor$all), township_code
  ) %>%
  mutate(meta_complex_id = as.numeric(meta_complex_id)) %>%
  ccao::vars_recode(
    starts_with("char_"),
    type = "long",
    as_factor = FALSE
  ) %>%
  write_parquet(paths$output$assessment_card$local)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 7. PIN-Level Data ------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Generate PIN-level stats for each run. These are used for desktop review,
# looking at YoY changes, comparing to sales, etc.

## 7.1. Load Sales/Land --------------------------------------------------------
message("Attaching recent sales to PIN-level data")

# Load the MOST RECENT sale per PIN from the year prior to the assessment year.
# These are the sales that will be used for ratio studies in the evaluate stage.
# We want our assessed value to be as close as possible to this sale
sales_data_cols <- c(
  "meta_pin", "meta_year", "sale_ratio_study_price",
  "sale_ratio_study_date", "sale_ratio_study_document_num"
)

sales_data_ratio_study <- copy(sales_data)[
  meta_year == params$assessment$data_year
][
  ,
  max_sale_date := max(meta_sale_date),
  by = .(meta_pin)
][
  meta_sale_date == max_sale_date
][
  ,
  `:=`(
    sale_ratio_study_price = meta_sale_price,
    sale_ratio_study_date = meta_sale_date,
    sale_ratio_study_document_num = meta_sale_document_num
  )
][, ..sales_data_cols]

sales_data_ratio_study <- unique(
  sales_data_ratio_study,
  by = sales_data_cols
)

# Keep the two most recent sales for each PIN from any year. These are just for
# review, not for ratio studies
sales_data_two_cols <- c(
  "meta_pin", "meta_year", "meta_sale_price",
  "meta_sale_date", "meta_sale_document_num"
)

sales_data_two_idx <- unique(sales_data, by = sales_data_two_cols)[
  ,
  .I[order(-as.integer(meta_sale_date))][1:2],
  by = .(meta_pin)
]

sales_data_two_temp <- unique(sales_data, by = sales_data_two_cols)[
  sales_data_two_idx$V1
][
  !is.na(meta_pin)
][
  , ..sales_data_two_cols
][
  ,
  mr := paste0("sale_recent_", seq_len(.N)),
  by = .(meta_pin)
]

sales_data_two_most_recent <- dcast(
  sales_data_two_temp,
  meta_pin ~ mr,
  value.var = c("meta_sale_date", "meta_sale_price", "meta_sale_document_num")
)
setnames(
  sales_data_two_most_recent,
  c(
    "meta_pin", "sale_recent_1_date", "sale_recent_2_date",
    "sale_recent_1_price", "sale_recent_2_price",
    "sale_recent_1_document_num", "sale_recent_2_document_num"
  )
)
setcolorder(
  sales_data_two_most_recent,
  c(
    "meta_pin", "sale_recent_1_date", "sale_recent_1_price",
    "sale_recent_1_document_num", "sale_recent_2_date",
    "sale_recent_2_price", "sale_recent_2_document_num"
  )
)


## 7.2. Collapse to PIN Level --------------------------------------------------
message("Collapsing card-level data to PIN level")

# Collapse card-level data to the PIN level, keeping the largest building on
# each PIN but summing the total square footage of all buildings
assessment_pin_data_base <- copy(assessment_card_data_merged)[
  ,
  `:=`(
    pred_pin_initial_fmv = sum(pred_card_initial_fmv),
    char_total_bldg_sf = sum(char_bldg_sf, na.rm = TRUE)
  ),
  by = .(meta_year, meta_pin)
]
assessment_pin_data_idx <- assessment_pin_data_base[
  ,
  .I[order(-char_bldg_sf)][1],
  by = .(meta_year, meta_pin)
]
assessment_pin_data_base <- assessment_pin_data_base[assessment_pin_data_idx$V1]

setnames(assessment_pin_data_base, gsub(
  paste0("^", rsn_prefix, "_"),
  "prior_near_",
  colnames(assessment_pin_data_base)
))
setnames(assessment_pin_data_base, gsub(
  paste0("^", rsf_prefix, "_"),
  "prior_far_",
  colnames(assessment_pin_data_base)
))

assessment_pin_data_base <- assessment_pin_data_base[, .SD, .SDcols = c(
  # Keep ID and meta variables
  "meta_year", "meta_pin", "meta_triad_code", "meta_township_code",
  "meta_nbhd_code", "meta_tax_code", "meta_class", "meta_tieback_key_pin",
  "meta_tieback_proration_rate", "meta_cdu", "meta_pin_num_cards",
  "meta_pin_num_landlines", "meta_complex_id",

  # Keep certain vital characteristics for the largest card on the PIN
  "char_yrblt", "char_land_sf", "char_ext_wall",
  "char_type_resd", "char_total_bldg_sf",

  # Keep locations, prior year values, and indicators
  "loc_longitude", "loc_latitude",
  unname(unlist(sapply(c(
    "loc_property_", "loc_cook_", "loc_chicago_", "loc_ward_",
    "loc_census_", "loc_school_", "prior_", "ind_"
  ), grep, names(assessment_pin_data_base), value = TRUE))),

  # Keep HIE flag
  "hie_num_expired",

  # Keep PIN-level predicted values and land rates
  "land_rate_per_pin", "land_rate_per_sqft",
  "pred_pin_initial_fmv",
  "pred_pin_final_fmv", "pred_pin_final_fmv_round_no_prorate",
  "pred_pin_uncapped_fmv_land", "pred_pin_final_fmv_land",
  "pred_pin_final_fmv_bldg_no_prorate", "pred_pin_final_fmv_bldg",
  "pred_pin_final_fmv_round", "township_code"
)][
  assessment_card_data_merged[
    ,
    .(
      meta_year, meta_pin,
      char_yrblt, char_bldg_sf, char_land_sf, char_beds,
      char_fbath, char_apts
    )
  ][
    ,
    .(meta_year, meta_pin, ind_missing = rowSums(is.na(.SD)))
  ][
    ,
    .(ind_char_missing_critical_value = sum(ind_missing) > 0),
    by = .(meta_year, meta_pin)
  ],
  on = c("meta_year", "meta_pin")
]


## 7.3. Attach Sales -----------------------------------------------------------
message("Attaching and comparing sale values")

# Attach sales data to the PIN-level data
assessment_pin_data_sale <- merge(
  assessment_pin_data_base,
  sales_data_two_most_recent,
  by = "meta_pin", all.x = TRUE
)
assessment_pin_data_sale <- merge(
  assessment_pin_data_sale,
  sales_data_ratio_study,
  by = c("meta_year", "meta_pin"), all.x = TRUE
)
ratio_cols <- grep("^prior_", names(assessment_pin_data_sale), value = TRUE)
assessment_pin_data_sale[
  ,
  `:=`(
    # Calculate effective land rates (rate with 50% cap) + the % of
    # the PIN value dedicated to the building
    pred_pin_land_rate_effective = pred_pin_final_fmv_land / char_land_sf,
    pred_pin_bldg_rate_effective = pred_pin_final_fmv_bldg / char_total_bldg_sf,
    pred_pin_land_pct_total = pred_pin_final_fmv_land / pred_pin_final_fmv_round
  ),
][
  ,
  # Convert prior values to FMV from AV, then calculate year-over-year
  # percent and nominal changes
  (ratio_cols) := lapply(.SD, "*", 10),
  .SDcols = ratio_cols
][
  ,
  `:=`(
    prior_far_yoy_change_nom = pred_pin_final_fmv_round - prior_far_tot,
    prior_far_yoy_change_pct =
      (pred_pin_final_fmv_round - prior_far_tot) / prior_far_tot,
    prior_near_yoy_change_nom = pred_pin_final_fmv_round - prior_near_tot,
    prior_near_yoy_change_pct =
      (pred_pin_final_fmv_round - prior_near_tot) / prior_near_tot
  )
]


## 7.4. Add Flags --------------------------------------------------------------
message("Adding Desk Review flags")

# Add flags are used to identify PINs for potential desktop review
assessment_pin_data_final <- copy(assessment_pin_data_sale)

# Rename existing indicators to flags
setnames(assessment_pin_data_final, gsub(
  "^ind_", "flag_",
  colnames(assessment_pin_data_final)
))
setnames(assessment_pin_data_final, "hie_num_expired", "flag_hie_num_expired")

assessment_pin_data_final[
  ,
  # Add flag for potential proration issues (rates don't sum to 1)
  flag_proration_sum_not_1 := fifelse(
    !is.na(meta_tieback_key_pin),
    sum(meta_tieback_proration_rate) != 1,
    FALSE
  ),
  by = .(meta_tieback_key_pin)
][
  ,
  # Flag for capped land value
  flag_land_value_capped := pred_pin_final_fmv_round *
    params$pv$land_pct_of_total_cap == pred_pin_final_fmv_land
][
  ,
  # Flags for changes in values
  `:=`(
    flag_prior_near_to_pred_unchanged =
      prior_near_tot >= pred_pin_final_fmv_round - 100 &
        prior_near_tot <= pred_pin_final_fmv_round + 100, # nolint
    flag_pred_initial_to_final_changed = ccao::val_round_fmv(
      pred_pin_initial_fmv,
      breaks = params$pv$round_break,
      round_to = params$pv$round_to_nearest,
      type = params$pv$round_type
    ) != pred_pin_final_fmv_round,
    flag_prior_near_yoy_inc_gt_50_pct = prior_near_yoy_change_pct > 0.5,
    flag_prior_near_yoy_dec_gt_5_pct = prior_near_yoy_change_pct < -0.05
  )
][
  ,
  # Flag high-value properties from prior years
  flag_prior_near_fmv_top_decile := ntile(prior_near_tot, 10) == 10,
  by = .(meta_township_code)
][
  ,
  `:=`(
    flag_hie_num_expired = nafill(flag_hie_num_expired, fill = 0),
    meta_pin_num_landlines = nafill(meta_pin_num_landlines, fill = 1),
    meta_complex_id = as.numeric(meta_complex_id)
  )
][
  is.na(flag_pin_is_multiland),
  flag_pin_is_multiland := FALSE
]


## 7.5. Clean/Reorder/Save -----------------------------------------------------
message("Saving final PIN-level data")

# Recode characteristics from numeric encodings to human-readable strings
assessment_pin_data_final %>%
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
    pred_pin_initial_fmv, pred_pin_final_fmv,
    pred_pin_final_fmv_round_no_prorate,
    pred_pin_final_fmv_bldg_no_prorate, pred_pin_final_fmv_land,
    pred_pin_final_fmv_bldg, pred_pin_final_fmv_round,
    pred_pin_bldg_rate_effective, pred_pin_land_rate_effective,
    pred_pin_land_pct_total, starts_with(c("sale_", "flag_")), township_code
  ) %>%
  as_tibble() %>%
  write_parquet(paths$output$assessment_pin$local)

# End the stage timer and write the time elapsed to a temporary file
tictoc::toc(log = TRUE)
bind_rows(tictoc::tic.log(format = FALSE)) %>%
  arrow::write_parquet(gsub("//*", "/", file.path(
    paths$intermediate$timing$local,
    "model_timing_assess.parquet"
  )))

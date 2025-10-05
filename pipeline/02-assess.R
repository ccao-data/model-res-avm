#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Setup ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# NOTE: See DESCRIPTION for library dependencies and R/setup.R for
# variables used in each pipeline stage

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
sales_data <- read_parquet(paths$input$training$local)

# Load land rates from file
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
    # Multi-card PINs with 2-3 cards get a special prediction based on the
    # combined building square footage of all cards on the PIN. See below
    # (under assessment_card_data_mc) for more details
    og_char_bldg_sf = char_bldg_sf,
    char_bldg_sf = ifelse(
      ind_pin_is_multicard & meta_pin_num_cards %in% c(2, 3),
      sum(char_bldg_sf),
      char_bldg_sf
    ),
    .by = meta_pin
  ) %>%
  mutate(
    pred_card_initial_fmv = predict(
      lgbm_final_full_fit,
      new_data = bake(
        lgbm_final_full_recipe,
        new_data = .,
        all_predictors()
      )
    )$.pred,
    char_bldg_sf = og_char_bldg_sf
  ) %>%
  select(-og_char_bldg_sf)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Post-Modeling Adjustments -------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Performing post-modeling adjustments")

## 3.1. Multicards -------------------------------------------------------------
message("Fixing multicard PINs")

# Cards represent buildings/improvements. A PIN can have multiple cards, and
# the total taxable value of the PIN is (usually) the sum of all cards
assessment_card_data_mc <- assessment_card_data_pred %>%
  select(
    meta_year, meta_pin, meta_nbhd_code, meta_class, meta_card_num,
    meta_pin_num_cards, char_bldg_sf, char_land_sf,
    meta_tieback_key_pin, meta_tieback_proration_rate,
    meta_1yr_pri_board_tot, pred_card_initial_fmv
  ) %>%
  # For prorated PINs with multiple cards, take the average of the card
  # (building) across PINs. This is because the same prorated building spread
  # across multiple PINs sometimes receives different values from the model
  group_by(meta_tieback_key_pin, meta_card_num, char_land_sf) %>%
  mutate(
    pred_card_intermediate_fmv = ifelse(
      is.na(meta_tieback_key_pin),
      pred_card_initial_fmv,
      mean(pred_card_initial_fmv)
    )
  ) %>%
  # For single-card PINs, the card-level predicted value is the PIN value.
  # For multi-card PINs with 2 or 3 cards, we aggregate the building square
  # footage of all cards into a single card (the largest), predict, then use
  # that prediction as the PIN value. For > 3 cards, we predict each card with
  # its original square footage then sum the predictions to get the PIN value
  group_by(meta_pin) %>%
  arrange(meta_pin, desc(char_bldg_sf), meta_card_num) %>%
  mutate(
    pred_pin_card_sum = ifelse(
      meta_pin_num_cards > 3,
      sum(pred_card_intermediate_fmv),
      first(pred_card_intermediate_fmv)
    )
  ) %>%
  arrange(meta_pin, meta_card_num) %>%
  ungroup()


## 3.2. Townhomes --------------------------------------------------------------
message("Averaging townhome complex predictions")

# For class 210 and 295s, we want all units in the same complex to
# have the same value (assuming they are nearly identical)

# Load townhome/rowhome complex IDs
complex_id_data <- read_parquet(paths$input$complex_id$local) %>%
  select(meta_pin, meta_complex_id)

# Join complex IDs to the predictions, then for each complex, set the
# prediction to the average prediction of the complex
assessment_card_data_cid <- assessment_card_data_mc %>%
  left_join(complex_id_data, by = "meta_pin") %>%
  group_by(meta_complex_id, meta_tieback_proration_rate) %>%
  mutate(
    pred_pin_final_fmv = ifelse(
      is.na(meta_complex_id),
      pred_pin_card_sum,
      mean(pred_pin_card_sum)
    )
  ) %>%
  ungroup()


## 3.3. Round ------------------------------------------------------------------
message("Rounding predictions")

# Round PIN-level predictions using the breaks and amounts specified in params
assessment_card_data_round <- assessment_card_data_cid %>%
  mutate(
    pred_pin_final_fmv_round_no_prorate = ccao::val_round_fmv(
      pred_pin_final_fmv,
      breaks = params$pv$round_break,
      round_to = params$pv$round_to_nearest,
      type = params$pv$round_type
    )
  )




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4. Value Land ----------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Valuing land")

# Land values are provided by Valuations and are capped at a percentage of the
# total FMV for the PIN. For 210 and 295s (townhomes), there's sometimes a pre-
# calculated land total value, for all other classes, there's a $/sqft rate
assessment_pin_data_w_land <- assessment_card_data_round %>%
  # Keep only the necessary unique PIN-level values, since land is valued by
  # PIN rather than card
  group_by(meta_year, meta_pin) %>%
  distinct(
    meta_nbhd_code, meta_class, meta_complex_id,
    meta_tieback_key_pin, meta_tieback_proration_rate,
    char_land_sf, pred_pin_final_fmv, pred_pin_final_fmv_round_no_prorate
  ) %>%
  ungroup() %>%
  left_join(
    land_nbhd_rate,
    by = c("meta_nbhd_code" = "meta_nbhd", "meta_class")
  ) %>%
  mutate(
    pred_pin_final_fmv_land = ceiling(case_when(
      # Use the land $/sqft rate (unless it exceeds the % of total value cap)
      char_land_sf * land_rate_per_sqft >= pred_pin_final_fmv_round_no_prorate *
        params$pv$land_pct_of_total_cap ~
        pred_pin_final_fmv_round_no_prorate * params$pv$land_pct_of_total_cap,
      TRUE ~ char_land_sf * land_rate_per_sqft
    )),
    # If the land $/sqft is missing, just use the max capped land value as a
    # default (usually 50% of the predicted value). Data doesn't usually get
    # land $/sqft until the beginning of the year we're modeling for, but a
    # predicted land value is required to calculate the final estimated FMV. As
    # such, setting this default lets us start modeling before we receive the
    # finalized land $/sqft rates
    pred_pin_final_fmv_land = ifelse(
      is.na(pred_pin_final_fmv_land),
      pred_pin_final_fmv_round_no_prorate * params$pv$land_pct_of_total_cap,
      pred_pin_final_fmv_land
    ),
    # Keep the uncapped value for display in desk review
    pred_pin_uncapped_fmv_land = ceiling(char_land_sf * land_rate_per_sqft)
  )




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 5. Prorate and Reapportion ---------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Prorating buildings")

# Prorating is the process of dividing a building's value among multiple PINs.
# See the steps outlined below for the process to determine a prorated value:
assessment_pin_data_prorated <- assessment_pin_data_w_land %>%
  group_by(meta_tieback_key_pin) %>%
  mutate(
    # 1. Determine the mean, unprorated building value for buildings that span
    # multiple PINs. This is the mean value of the predicted value minus land
    pred_pin_final_fmv_bldg_no_prorate = ifelse(
      is.na(meta_tieback_key_pin),
      pred_pin_final_fmv_round_no_prorate - pred_pin_final_fmv_land,
      mean(pred_pin_final_fmv_round_no_prorate - pred_pin_final_fmv_land)
    )
  ) %>%
  ungroup() %>%
  mutate(
    # 2. Multiply the building by the proration rate of each PIN/card. This is
    # the proportion of the building's value held by each PIN
    pred_pin_final_fmv_bldg =
      pred_pin_final_fmv_bldg_no_prorate * meta_tieback_proration_rate,
    temp_bldg_frac_prop =
      pred_pin_final_fmv_bldg - as.integer(pred_pin_final_fmv_bldg)
  ) %>%
  # 3. Assign the fractional portion of a building (cents) to whichever portion
  # is largest i.e. [1.59, 1.41] becomes [2, 1]
  group_by(meta_tieback_key_pin) %>%
  arrange(meta_tieback_key_pin, desc(temp_bldg_frac_prop)) %>%
  mutate(
    temp_add_to_final = as.numeric(
      n() > 1 & row_number() == 1 & temp_bldg_frac_prop > 0.1e-7
    ),
    temp_add_diff = temp_add_to_final * round(
      sum(pred_pin_final_fmv_bldg, na.rm = TRUE) -
        sum(as.integer(pred_pin_final_fmv_bldg), na.rm = TRUE)
    ),
    pred_pin_final_fmv_bldg = as.integer(pred_pin_final_fmv_bldg) +
      temp_add_diff
  ) %>%
  ungroup() %>%
  select(-starts_with("temp_")) %>%
  mutate(
    # 4. To get the total value of the individual PINs, add the individual land
    # value of the PINs back to the prorated building value
    pred_pin_final_fmv_round =
      pred_pin_final_fmv_bldg + pred_pin_final_fmv_land
  )

# Merge the final PIN-level data back to the main tibble of predictions
assessment_card_data_merged <- assessment_pin_data_prorated %>%
  select(
    meta_year, meta_pin, meta_complex_id,
    pred_pin_final_fmv, pred_pin_final_fmv_round_no_prorate,
    land_rate_per_sqft, pred_pin_uncapped_fmv_land,
    pred_pin_final_fmv_land, pred_pin_final_fmv_bldg_no_prorate,
    pred_pin_final_fmv_bldg, pred_pin_final_fmv_round
  ) %>%
  left_join(
    assessment_card_data_pred,
    by = c("meta_year", "meta_pin"),
    multiple = "all"
  ) %>%
  mutate(
    township_code = meta_township_code,
    meta_year = as.character(meta_year)
  ) %>%
  # Apportion the final prorated PIN-level value back out to the card-level
  # using the square footage of each improvement
  group_by(meta_year, meta_pin) %>%
  mutate(
    meta_card_pct_total_fmv = char_bldg_sf / sum(char_bldg_sf),
    # In cases where bldg sqft is missing (rare), fill evenly across cards
    meta_card_pct_total_fmv = ifelse(
      is.na(meta_card_pct_total_fmv),
      1 / n(),
      meta_card_pct_total_fmv
    ),
    pred_card_final_fmv = pred_pin_final_fmv_bldg * meta_card_pct_total_fmv,
    temp_card_frac_prop = pred_card_final_fmv - as.integer(pred_card_final_fmv)
  ) %>%
  # More fractional rounding to deal with card values being split into cents
  group_by(meta_year, meta_pin) %>%
  arrange(meta_year, meta_pin, desc(temp_card_frac_prop)) %>%
  mutate(
    temp_add_to_final = as.numeric(
      n() > 1 & row_number() == 1 & temp_card_frac_prop > 0.1e-7
    ),
    temp_add_diff = temp_add_to_final * (
      sum(pred_card_final_fmv, na.rm = TRUE) -
        sum(as.integer(pred_card_final_fmv), na.rm = TRUE)
    ),
    pred_card_final_fmv = round(as.integer(pred_card_final_fmv) + temp_add_diff)
  ) %>%
  ungroup() %>%
  select(-starts_with("temp_"))

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
char_vars <- params$model$predictor$all[
  grepl("^char_", params$model$predictor$all)
]
char_vars <- char_vars[!char_vars %in% c("char_apts", "char_recent_renovation")]
assessment_card_data_merged %>%
  select(
    meta_year, meta_pin, meta_class, meta_card_num, meta_card_pct_total_fmv,
    meta_complex_id, pred_card_initial_fmv, pred_card_final_fmv, char_class,
    all_of(params$model$predictor$all), township_code
  ) %>%
  mutate(
    meta_complex_id = as.numeric(meta_complex_id),
    char_apts = as.character(char_apts)
  ) %>%
  ccao::vars_recode(
    cols = any_of(char_vars),
    code_type = "long",
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
sales_data_ratio_study <- sales_data %>%
  # For ratio studies, we don't want to include outliers
  filter(!sv_is_outlier) %>%
  filter(meta_year == params$assessment$data_year) %>%
  # Kludge to remove some sales that somehow appear to be for a single card
  # on a multi-card PIN. Will need to go back and hand validate these
  filter(
    !meta_sale_document_num %in% c("2335646020", "2312245016")
  ) %>%
  group_by(meta_pin) %>%
  filter(meta_sale_date == max(meta_sale_date)) %>%
  distinct(
    meta_pin, meta_year,
    sale_ratio_study_price = meta_sale_price,
    sale_ratio_study_date = meta_sale_date,
    sale_ratio_study_document_num = meta_sale_document_num
  ) %>%
  ungroup()

# Keep the two most recent sales for each PIN from any year. These are just for
# review, not for ratio studies
sales_data_two_most_recent <- sales_data %>%
  distinct(
    meta_pin, meta_year,
    meta_sale_price, meta_sale_date, meta_sale_document_num, sv_is_outlier,
    sv_outlier_reason1, sv_outlier_reason2, sv_outlier_reason3
  ) %>%
  # Include outliers, since these data are used for desk review and
  # not for modeling
  rename(
    meta_sale_is_outlier = sv_is_outlier,
    meta_sale_outlier_reason1 = sv_outlier_reason1,
    meta_sale_outlier_reason2 = sv_outlier_reason2,
    meta_sale_outlier_reason3 = sv_outlier_reason3
  ) %>%
  group_by(meta_pin) %>%
  slice_max(meta_sale_date, n = 2) %>%
  mutate(mr = paste0("sale_recent_", row_number())) %>%
  tidyr::pivot_wider(
    id_cols = meta_pin,
    names_from = mr,
    values_from = c(
      meta_sale_date,
      meta_sale_price,
      meta_sale_document_num,
      meta_sale_is_outlier,
      meta_sale_outlier_reason1,
      meta_sale_outlier_reason2,
      meta_sale_outlier_reason3
    ),
    names_glue = "{mr}_{gsub('meta_sale_', '', .value)}"
  ) %>%
  select(meta_pin, contains("1"), contains("2")) %>%
  ungroup()


## 7.2. Collapse to PIN Level --------------------------------------------------
message("Collapsing card-level data to PIN level")

# Collapse card-level data to the PIN level, keeping the largest building on
# each PIN but summing the total square footage of all buildings
assessment_pin_data_base <- assessment_card_data_merged %>%
  group_by(meta_year, meta_pin) %>%
  arrange(meta_year, meta_pin, desc(char_bldg_sf)) %>%
  mutate(
    # Keep the sum of the initial card level values
    pred_pin_initial_fmv = sum(pred_card_initial_fmv),
    char_total_bldg_sf = sum(char_bldg_sf, na.rm = TRUE)
  ) %>%
  filter(row_number() == 1) %>%
  # Rename prior year comparison columns to near/far to maintain consistent
  # column names in Athena
  rename_with(
    .fn = ~ gsub(paste0(rsn_prefix, "_"), "prior_near_", .x),
    .cols = starts_with(rsn_prefix)
  ) %>%
  rename_with(
    .fn = ~ gsub(paste0(rsf_prefix, "_"), "prior_far_", .x),
    .cols = starts_with(rsf_prefix)
  ) %>%
  ungroup() %>%
  select(
    # Keep ID and meta variables
    meta_year, meta_pin, meta_triad_code, meta_township_code, meta_nbhd_code,
    meta_tax_code, meta_class, meta_tieback_key_pin,
    meta_tieback_proration_rate, meta_cdu, meta_pin_num_cards,
    meta_pin_num_landlines, meta_complex_id,

    # Keep certain vital characteristics for the largest card on the PIN
    char_yrblt, char_land_sf, char_ext_wall, char_type_resd, char_total_bldg_sf,

    # Keep locations, prior year values, and indicators
    loc_longitude, loc_latitude,
    starts_with(c(
      "loc_property_", "loc_chicago_", "loc_ward_",
      "loc_census", "loc_school_", "loc_tax_",
      "prior_", "ind_"
    )),

    # Keep HIE flag
    hie_num_expired,

    # Keep PIN-level predicted values and land rates
    land_rate_per_sqft,
    pred_pin_initial_fmv,
    pred_pin_final_fmv, pred_pin_final_fmv_round_no_prorate,
    pred_pin_uncapped_fmv_land, pred_pin_final_fmv_land,
    pred_pin_final_fmv_bldg_no_prorate, pred_pin_final_fmv_bldg,
    pred_pin_final_fmv_round, township_code
  ) %>%
  # Make a flag for any vital missing characteristics
  left_join(
    assessment_card_data_merged %>%
      select(
        meta_year, meta_pin,
        char_yrblt, char_bldg_sf, char_land_sf, char_beds,
        char_fbath, char_apts
      ) %>%
      mutate(ind_char_missing_critical_value = rowSums(is.na(.))) %>%
      group_by(meta_year, meta_pin) %>%
      summarize(
        ind_char_missing_critical_value =
          sum(ind_char_missing_critical_value) > 0
      ) %>%
      ungroup(),
    by = c("meta_year", "meta_pin")
  )


## 7.3. Attach Sales -----------------------------------------------------------
message("Attaching and comparing sale values")

# Attach sales data to the PIN-level data
assessment_pin_data_sale <- assessment_pin_data_base %>%
  left_join(sales_data_two_most_recent, by = "meta_pin") %>%
  left_join(sales_data_ratio_study, by = c("meta_year", "meta_pin")) %>%
  # Calculate effective land rates (rate with 50% cap) + the % of the PIN value
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


## 7.4. Add Flags --------------------------------------------------------------
message("Adding Desk Review flags")

# Flags are used to identify PINs for potential desktop review
assessment_pin_data_final <- assessment_pin_data_sale %>%
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
      params$pv$land_pct_of_total_cap == pred_pin_final_fmv_land
  ) %>%
  # Flags for changes in values
  mutate(
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
    flag_prior_near_yoy_dec_gt_5_pct = prior_near_yoy_change_pct < -0.05,
  ) %>%
  # Flag high-value properties from prior years
  group_by(meta_township_code) %>%
  mutate(flag_prior_near_fmv_top_decile = ntile(prior_near_tot, 10) == 10) %>%
  ungroup() %>%
  # Flags for HIEs / 288s (placeholder until 288 data is integrated)
  rename(flag_hie_num_expired = hie_num_expired) %>%
  mutate(
    flag_hie_num_expired = tidyr::replace_na(flag_hie_num_expired, 0),
    meta_pin_num_landlines = tidyr::replace_na(meta_pin_num_landlines, 1),
    flag_pin_is_multiland = tidyr::replace_na(flag_pin_is_multiland, FALSE)
  )


## 7.5. Clean/Reorder/Save -----------------------------------------------------
message("Saving final PIN-level data")

# Recode characteristics from numeric encodings to human-readable strings
assessment_pin_data_final %>%
  ccao::vars_recode(
    cols = starts_with("char_"),
    code_type = "short",
    as_factor = FALSE
  ) %>%
  # Coerce columns to their expected Athena output type
  mutate(
    meta_complex_id = as.numeric(meta_complex_id),
    flag_hie_num_expired = as.numeric(flag_hie_num_expired)
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

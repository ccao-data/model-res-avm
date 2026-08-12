#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Setup ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Set this to the base of the HomeVal staging URL, with no trailing slash.
# We'll use this URL to populate the HomeVal link in the output workbooks.
# We don't publish the staging URL in public code because we often deploy
# provisional values to staging, and we don't want users to find the staging
# app and assume the values are final
HOMEVAL_STAGING_BASE_URL <- "https://example.com"

# NOTE: See DESCRIPTION for library dependencies and R/setup.R for
# variables used in each pipeline stage

# Allow Java to use more memory
options(java.parameters = "-Xmx20g")

# Load libraries, helpers, and recipes from files
purrr::walk(list.files("R/", "\\.R$", full.names = TRUE), source)

# Load additional dev R libraries (see README#managing-r-dependencies)
suppressPackageStartupMessages({
  library(DBI)
  library(openxlsx)
  library(noctua)
  library(readr)
  library(stringr)
})

# Establish Athena connection
AWS_ATHENA_CONN_NOCTUA <- dbConnect(noctua::athena(), rstudio_conn_tab = FALSE)


#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. Pull Vacant Land ----------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Pulling vacant land data from Athena")

# Need to pull all vacant land PINs so that they can be valued separately from
# the regression model using a flat rate per neighborhood

# Each land PIN can have multiple "lines" that potentially receive different
# rates, here we grab the lines and square footage for each PIN
land <- dbGetQuery(
  conn = AWS_ATHENA_CONN_NOCTUA, glue("
  SELECT
      taxyr AS meta_year,
      parid AS meta_pin,
      lline AS meta_line_num,
      sf AS meta_line_sf
  FROM iasworld.land
  WHERE taxyr = '{params$assessment$data_year}'
  ")
)

# For land lines with no square footage, set to default of 0 if a secondary line
# If a main line, then set to 10
land <- land %>%
  group_by(meta_pin) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  mutate(
    meta_line_sf = case_when(
      count == 1 & is.na(meta_line_sf) ~ 10,
      count > 1 & is.na(meta_line_sf) ~ 0,
      TRUE ~ meta_line_sf
    )
  ) %>%
  select(-count)

# To include land with the desk review spreadsheets we need to pull the same
# columns for land PINs as the model output (assessment_pin)
vacant_land <- dbGetQuery(
  conn = AWS_ATHENA_CONN_NOCTUA, glue("
  SELECT
      uni.township_code,
      uni.pin AS meta_pin,
      uni.class AS meta_class,
      uni.nbhd_code AS meta_nbhd_code,
      uni.tax_municipality_name AS loc_tax_municipality_name,
      addr.prop_address_full AS loc_property_address,
      addr.prop_address_city_name As loc_property_city,
      addr.prop_address_state AS loc_property_state,
      addr.prop_address_zipcode_1 AS loc_property_zip,
      char.tieback_key_pin AS meta_tieback_key_pin,
      char.tieback_proration_rate AS meta_tieback_proration_rate,
      hist.mailed_bldg AS meta_mailed_bldg,
      hist.mailed_land AS meta_mailed_land,
      hist.mailed_tot AS meta_mailed_tot,
      hist.certified_bldg AS meta_certified_bldg,
      hist.certified_land AS meta_certified_land,
      hist.certified_tot AS meta_certified_tot,
      hist.board_bldg AS meta_board_bldg,
      hist.board_land AS meta_board_land,
      hist.board_tot AS meta_board_tot,
      hist.oneyr_pri_board_bldg AS meta_1yr_pri_board_bldg,
      hist.oneyr_pri_board_land AS meta_1yr_pri_board_land,
      hist.oneyr_pri_board_tot AS meta_1yr_pri_board_tot,
      hist.twoyr_pri_board_bldg AS meta_2yr_pri_board_bldg,
      hist.twoyr_pri_board_land AS meta_2yr_pri_board_land,
      hist.twoyr_pri_board_tot AS meta_2yr_pri_board_tot
  FROM default.vw_pin_universe uni
  LEFT JOIN default.vw_pin_address addr
      ON uni.pin = addr.pin
      AND uni.year = addr.year
  LEFT JOIN default.vw_card_res_char char
      ON uni.pin = char.pin
      AND uni.year = char.year
  LEFT JOIN default.vw_pin_history hist
      ON uni.pin = hist.pin
      AND uni.year = hist.year
  WHERE uni.year = '{params$assessment$data_year}'
  AND uni.class IN ('200', '201', '241')
  AND triad_code = '{params$export$triad_code}'
  ")
)

# Clean up the vacant land records to match the pipeline output
rsn_prefix <- gsub("_tot", "", params$ratio_study$near_column)
vacant_land_trans <- vacant_land %>%
  rename_with(
    .fn = ~ gsub(paste0(rsn_prefix, "_"), "prior_near_", .x),
    .cols = starts_with(rsn_prefix)
  ) %>%
  select(-contains("mailed"), -contains("certified"), -contains("board")) %>%
  mutate(across(starts_with("prior_near_"), ~ .x * 10))

# Grab single-PIN sales for vacant land classes. Only used for reference, not
# to create values
vacant_land_sales <- dbGetQuery(
  conn = AWS_ATHENA_CONN_NOCTUA, glue("
  SELECT
      sale.pin AS meta_pin,
      sale.year AS meta_year,
      sale.class AS meta_class,
      sale.sale_price AS meta_sale_price,
      sale.sale_date AS meta_sale_date,
      sale.doc_no AS meta_sale_document_num
  FROM default.vw_pin_sale sale
  WHERE NOT is_multisale
  AND class IN ('200', '201', '241')
  AND (year
      BETWEEN '{params$input$min_sale_year}'
      AND '{params$input$max_sale_year}')
  ")
)

# Transform sales data from long to wide, keeping most recent 2 sales
vacant_land_sales_trans <- vacant_land_sales %>%
  mutate(meta_sale_date = ymd(meta_sale_date)) %>%
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

# Load neighborhood level land rates
land_nbhd_rate <- dbGetQuery(
  conn = AWS_ATHENA_CONN_NOCTUA, glue("
  SELECT
      town_nbhd AS meta_nbhd_code,
      class AS meta_class,
      land_rate_per_sqft
  FROM ccao.land_nbhd_rate
  WHERE year = '{params$assessment$year}'
  ")
)

# Combine land data into a single dataframe with the same structure as
# assessment_pin. Carry over improvement values from prior years
vacant_land_merged <- vacant_land_trans %>%
  left_join(vacant_land_sales_trans, by = "meta_pin") %>%
  left_join(land_nbhd_rate, by = c("meta_nbhd_code", "meta_class")) %>%
  left_join(
    land %>%
      group_by(meta_pin) %>%
      summarize(
        char_land_sf = sum(meta_line_sf),
        flag_pin_is_multiland = n() > 1
      ),
    by = "meta_pin"
  ) %>%
  mutate(
    # Replace missing values for some PINs (very few, usually brand new)
    across(
      c(char_land_sf, prior_near_land, prior_near_bldg, prior_near_tot),
      ~ replace_na(.x, 10)
    ),
    prior_near_land_rate = round(prior_near_land / char_land_sf, 2),
    prior_near_land_pct_total = round(prior_near_land / prior_near_tot, 4),
    pred_pin_final_fmv_bldg = ifelse(
      !is.na(prior_near_bldg),
      prior_near_bldg,
      0
    ),
    # Certain PINs have basically placeholder values, we want to carry these
    # over
    pred_pin_final_fmv_land = ifelse(
      prior_near_tot <= 100,
      prior_near_tot,
      ceiling(char_land_sf * land_rate_per_sqft)
    ),
    pred_pin_final_fmv = pred_pin_final_fmv_bldg + pred_pin_final_fmv_land,
    pred_pin_final_fmv_round = pred_pin_final_fmv,
    pred_pin_land_rate_effective = land_rate_per_sqft,
    pred_pin_land_pct_total =
      pred_pin_final_fmv_land / pred_pin_final_fmv_round,
    prior_near_yoy_change_nom = pred_pin_final_fmv_round - prior_near_tot,
    prior_near_yoy_change_pct = prior_near_yoy_change_nom / prior_near_tot,
    across(
      c(prior_near_yoy_change_pct, prior_near_land_pct_total),
      ~ replace(.x, is.nan(.x), 0)
    ),
    flag_pin_is_prorated = meta_tieback_proration_rate != 1,
    flag_pin_is_multiland,
    flag_land_value_capped = 0,
    flag_prior_near_to_pred_unchanged =
      prior_near_tot == pred_pin_final_fmv_round,
    flag_prior_near_yoy_inc_gt_50_pct = prior_near_yoy_change_pct > 0.5,
    flag_prior_near_yoy_dec_gt_5_pct = prior_near_yoy_change_pct < -0.05,
    across(c(starts_with("flag_"), ends_with("_price")), as.numeric)
  )


#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Pull Model Data -----------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Pulling model data from Athena")

# Pull the PIN-level assessment data, which contains all the fields needed to
# create the review spreadsheets
assessment_pin <- dbGetQuery(
  conn = AWS_ATHENA_CONN_NOCTUA, glue("
  SELECT *
  FROM model.assessment_pin
  WHERE run_id = '{params$export$run_id}'
  AND meta_triad_code = '{params$export$triad_code}'
  ")
)

# Pull card-level data only for all PINs. Needed for upload, since values are
# tracked by card, even though they're presented by PIN
assessment_card <- dbGetQuery(
  conn = AWS_ATHENA_CONN_NOCTUA, glue("
  SELECT c.*
  FROM model.assessment_card c
  INNER JOIN (
      SELECT *
      FROM model.assessment_pin
      WHERE run_id = '{params$export$run_id}'
      AND meta_triad_code = '{params$export$triad_code}'
  ) p
  ON c.year = p.year
      AND c.run_id = p.run_id
      AND c.meta_pin = p.meta_pin
  ")
)

# Pull assessable permit flag
flag_assessable_permits <- dbGetQuery(
  conn = AWS_ATHENA_CONN_NOCTUA, glue("
  SELECT pin, has_recent_assessable_permit
  FROM default.vw_pin_status
  WHERE year = '{params$assessment$data_year}'
  ")
)


#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4. Prep Desk Review ----------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Preparing data for Desk Review export")

format_char_apts <- function(char_apts) {
  # Convert the long format for char_apts to a shorter format that's easier
  # to scan in a spreadsheet
  return(
    case_when(
      is.na(char_apts) | tolower(char_apts) == "none" ~ NA_character_,
      tolower(char_apts) == "two" ~ "2",
      tolower(char_apts) == "three" ~ "3",
      tolower(char_apts) == "four" ~ "4",
      tolower(char_apts) == "five" ~ "5",
      tolower(char_apts) == "six" ~ "6",
      TRUE ~ "Missing"
    )
  )
}

summarize_char <- function(col) {
  # Summary function that can take a column name `col` and return a
  # comma-separated string of all the distinct, not-null values in that column.
  # Note that nulls are preserved if and only if _all_ values in the column
  # are null
  return(
    ifelse(
      all(is.na(col)),
      NA,
      paste(unique(na.omit(col)), collapse = ", ")
    )
  )
}

# Aggregate a set of chars that are only recorded on the card level so that
# we can display them in the PIN Detail sheet
summarized_card_chars <- assessment_card %>%
  mutate(
    char_apts = format_char_apts(char_apts),
    # At the ingest stage, char_ncu is set to 0 for non-212 properties, so
    # cast this case to null
    char_ncu = ifelse(char_class == "212", char_ncu, NA)
  ) %>%
  # Aggregate card-level chars by PIN and output them as comma-separated
  # lists of unique values
  summarize(
    across(
      all_of(c(
        "char_air", "char_apts", "char_beds", "char_bsmt", "char_bsmt_fin",
        "char_heat", "char_ncu"
      )),
      ~ summarize_char(.x)
    ),
    .by = "meta_pin"
  )

assessment_pin_w_card_chars <- assessment_pin %>%
  left_join(summarized_card_chars, by = "meta_pin")

# Merge vacant land data with data from the residential AVM
assessment_pin_w_land <- assessment_pin_w_card_chars %>%
  mutate(
    meta_complex_id = as.numeric(meta_complex_id),
    across(ends_with("_date"), ymd),
    across(starts_with("flag_"), as.numeric),
    across(where(is.numeric), ~ na_if(.x, Inf))
  ) %>%
  bind_rows(vacant_land_merged) %>%
  filter(!is.na(pred_pin_final_fmv_land)) %>%
  mutate(across(ends_with("_date"), as_date))

# Make sure to set the homeval link to the correct link rather than the example
# at the top of the script.
if (identical(HOMEVAL_STAGING_BASE_URL, "https://example.com")) {
  stop("HOMEVAL_STAGING_BASE_URL is still set to example.com.")
}

# Prep data with a few additional columns + put everything in the right
# order for DR sheets
assessment_pin_prepped <- assessment_pin_w_land %>%
  mutate(
    prior_near_land_rate = round(prior_near_land / char_land_sf, 2),
    prior_near_bldg_rate = round(prior_near_bldg / char_total_bldg_sf, 2),
    prior_near_land_pct_total = round(prior_near_land / prior_near_tot, 4),
    property_full_address = paste0(
      loc_property_address,
      ", ", loc_property_city, " ", loc_property_state,
      ", ", loc_property_zip
    ),
    homeval_report = glue(
      '=HYPERLINK("{HOMEVAL_STAGING_BASE_URL}/{year}/{meta_pin}.html")'
    ),
    valuations_note = NA, # Empty notes field for Valuations to fill out
    sale_ratio = NA # Initialize as NA so we can fill out with a formula later
  ) %>%
  # Add assessable permit flag
  left_join(flag_assessable_permits, by = c("meta_pin" = "pin")) %>%
  mutate(
    flag_has_recent_assessable_permit =
      as.numeric(has_recent_assessable_permit),
    # Only keep outlier values when the sale is marked as an outlier
    sale_recent_1_outlier_reason =
      if_else(sale_recent_1_is_outlier, sale_recent_1_outlier_reason, ""),
    sale_recent_2_outlier_reason =
      if_else(sale_recent_2_is_outlier, sale_recent_2_outlier_reason, "")
  ) %>%
  # Select fields for output to workbook
  select(
    township_code, meta_pin, meta_class, meta_nbhd_code,
    property_full_address, loc_tax_municipality_name, meta_complex_id,
    meta_pin_num_cards, meta_tieback_key_pin, meta_tieback_proration_rate,
    prior_near_land, prior_near_bldg, prior_near_tot,
    prior_near_land_rate, prior_near_bldg_rate, prior_near_land_pct_total,
    pred_pin_final_fmv, pred_pin_final_fmv_land, pred_pin_final_fmv_bldg,
    pred_pin_final_fmv_round, land_rate_per_sqft, pred_pin_land_rate_effective,
    pred_pin_bldg_rate_effective, pred_pin_land_pct_total,
    prior_near_yoy_change_nom, prior_near_yoy_change_pct,
    sale_ratio, valuations_note, sale_recent_1_date, sale_recent_1_price,
    sale_recent_1_outlier_reason, sale_recent_1_document_num,
    sale_recent_2_date, sale_recent_2_price, sale_recent_2_outlier_reason,
    sale_recent_2_document_num, char_yrblt, char_beds, char_ext_wall, char_bsmt,
    char_bsmt_fin, char_air, char_heat, char_total_bldg_sf, char_type_resd,
    char_land_sf, char_apts, char_ncu,
    homeval_report, flag_pin_is_prorated, flag_proration_sum_not_1,
    flag_proration_tieback_cycle, flag_pin_is_multicard, flag_pin_is_multiland,
    flag_land_gte_95_percentile, flag_bldg_gte_95_percentile,
    flag_land_value_capped,
    flag_prior_near_to_pred_unchanged, flag_pred_initial_to_final_changed,
    flag_prior_near_yoy_inc_gt_50_pct, flag_prior_near_yoy_dec_gt_5_pct,
    flag_char_missing_critical_value, flag_has_recent_assessable_permit
  ) %>%
  arrange(township_code, meta_pin) %>%
  mutate(
    meta_pin = glue(
      '=HYPERLINK("https://www.cookcountyassessor.com/pin/{meta_pin}",
      "{meta_pin}")'
    ),
    property_full_address = str_remove_all(
      property_full_address,
      "[^[:alnum:]|' ',.-]"
    ),
  )

# Get all PINs with multiple cards, break out into supplemental data set to
# attach to each town
assessment_card_prepped <- assessment_card %>%
  semi_join(
    assessment_pin %>%
      filter(as.logical(as.numeric(flag_pin_is_multicard))) %>%
      select(meta_pin),
    by = "meta_pin"
  ) %>%
  select(
    township_code, meta_pin, meta_card_num, char_class, meta_nbhd_code,
    meta_card_pct_total_fmv, pred_card_initial_fmv, pred_card_final_fmv,
    char_yrblt, char_beds, char_ext_wall, char_bsmt, char_bsmt_fin, char_air,
    char_heat, char_bldg_sf, char_type_resd, char_land_sf, char_apts, char_ncu
  ) %>%
  mutate(
    meta_pin = glue(
      '=HYPERLINK("https://www.cookcountyassessor.com/pin/{meta_pin}",
      "{meta_pin}")'
    ),
    char_apts = format_char_apts(char_apts),
    # Convert char_ncu from 0 to null for non-212s
    char_ncu = ifelse(char_class != "212", NA, char_ncu)
  ) %>%
  arrange(township_code, meta_pin, meta_card_num)


#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 5. Export Desk Review --------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Indices of all schema columns whose `cond` field equals `cond_name`
cols_with_cond <- function(schema, cond_name) {
  which(vapply(schema, function(x) identical(x$cond, cond_name), logical(1)))
}

# Indices of all schema columns marked hidden = TRUE
cols_hidden <- function(schema) {
  which(vapply(schema, function(x) isTRUE(x$hidden), logical(1)))
}

# Schema for the PIN Detail sheet. Each entry is a column written to the
# sheet in order; column positions are derived via col_pos() at runtime,
# so adding or removing a column only requires updating this list — all
# formatting and formula references below are driven by it.
pin_detail_schema <- list(
  meta_pin = list(formula = TRUE, display_name = "PIN"),
  meta_class = list(display_name = "Class"),
  meta_nbhd_code = list(display_name = "Nbhd."),
  property_full_address = list(display_name = "Street Address"),
  loc_tax_municipality_name = list(display_name = "Municipality"),
  meta_complex_id = list(display_name = "Townhome Complex ID"),
  meta_pin_num_cards = list(display_name = "PIN Num. Cards"),
  meta_tieback_key_pin = list(display_name = "Tieback Key PIN"),
  meta_tieback_proration_rate = list(
    style = "pct", display_name = "Tieback (Proration) Rate"
  ),
  prior_near_land = list(style = "price", display_name = "Land"),
  prior_near_bldg = list(style = "price", display_name = "Building"),
  prior_near_tot = list(style = "price", display_name = "Total"),
  prior_near_land_rate = list(
    style = "2digit_price", display_name = "Lnd. Rate S. F."
  ),
  prior_near_bldg_rate = list(
    style = "2digit_price", display_name = "Bld. Rate S. F."
  ),
  prior_near_land_pct_total = list(
    style = "pct", display_name = "Lnd. % of Total"
  ),
  pred_pin_final_fmv = list(
    style = "price", display_name = "Before Rounding"
  ),
  pred_pin_final_fmv_land = list(style = "price", display_name = "Land"),
  pred_pin_final_fmv_bldg = list(style = "price", display_name = "Building"),
  pred_pin_final_fmv_round = list(
    style = "price_highlight", display_name = "Total"
  ),
  land_rate_per_sqft = list(
    style = "2digit_price", display_name = "Lnd. Rate Original"
  ),
  pred_pin_land_rate_effective = list(
    style = "2digit_price", display_name = "Lnd. Rate Effective"
  ),
  pred_pin_bldg_rate_effective = list(
    style = "2digit_price", display_name = "Bld. Rate S. F."
  ),
  pred_pin_land_pct_total = list(
    style = "pct", display_name = "Lnd. % of Tot."
  ),
  prior_near_yoy_change_nom = list(
    style = "price", display_name = "YoY ∆ $"
  ),
  prior_near_yoy_change_pct = list(
    style = "pct", cond = "color_scale", display_name = "YoY ∆ %"
  ),
  sale_ratio = list(
    formula = TRUE, style = "2digit_num", display_name = "Sale Ratio"
  ),
  valuations_note = list(display_name = "Valuations Notes"),
  sale_recent_1_date = list(
    cond = "sale_outlier_1", display_name = "Sale Date 1"
  ),
  sale_recent_1_price = list(
    style = "price", cond = "sale_outlier_1", display_name = "Sale Amount 1"
  ),
  sale_recent_1_outlier_reason = list(
    cond = "sale_outlier_1",
    display_name = "Non-Arm's-Length Sale Flag 1"
  ),
  sale_recent_1_document_num = list(
    cond = "sale_outlier_1", display_name = "Sale Doc. 1"
  ),
  sale_recent_2_date = list(
    cond = "sale_outlier_2", display_name = "Sale Date 2"
  ),
  sale_recent_2_price = list(
    style = "price", cond = "sale_outlier_2", display_name = "Sale Amount 2"
  ),
  sale_recent_2_outlier_reason = list(
    cond = "sale_outlier_2",
    display_name = "Non-Arm's-Length Sale Flag 2"
  ),
  sale_recent_2_document_num = list(
    cond = "sale_outlier_2", display_name = "Sale Doc. 2"
  ),
  char_yrblt = list(display_name = "Year Built"),
  char_beds = list(style = "right_align", display_name = "# Beds"),
  char_ext_wall = list(display_name = "Ext. Wall"),
  char_bsmt = list(display_name = "Bsmt. Type"),
  char_bsmt_fin = list(display_name = "Bsmt. Finish"),
  char_air = list(display_name = "Central Air"),
  char_heat = list(display_name = "Heat"),
  char_total_bldg_sf = list(
    style = "comma", display_name = "Total Bld. S. F."
  ),
  char_type_resd = list(display_name = "Stories"),
  char_land_sf = list(style = "comma", display_name = "Lnd. S. F."),
  char_apts = list(style = "right_align", display_name = "# Res. Units"),
  char_ncu = list(style = "right_align", display_name = "# Comm. Units"),
  homeval_report = list(formula = TRUE, display_name = "Link"),
  flag_pin_is_prorated = list(display_name = "Is Prorated"),
  flag_proration_sum_not_1 = list(
    display_name = "Proration Rates Don't Sum to 100%"
  ),
  flag_proration_tieback_cycle = list(
    display_name = "Proration Tieback Cycle"
  ),
  flag_pin_is_multicard = list(display_name = "Multi-Card"),
  flag_pin_is_multiland = list(display_name = "Multi-Land"),
  flag_land_gte_95_percentile = list(display_name = "Lnd. >= 95% in Town"),
  flag_bldg_gte_95_percentile = list(display_name = "Bld. >= 95% in Town"),
  flag_land_value_capped = list(display_name = "Land Value Capped"),
  flag_prior_near_to_pred_unchanged = list(
    display_name = "Value Unchanged"
  ),
  flag_pred_initial_to_final_changed = list(
    display_name = "Post-Modeling Change"
  ),
  flag_prior_near_yoy_inc_gt_50_pct = list(
    display_name = "YoY Change >= 50%"
  ),
  flag_prior_near_yoy_dec_gt_5_pct = list(
    display_name = "YoY Change <= -5%"
  ),
  flag_char_missing_critical_value = list(
    display_name = "Critical Char. Missing"
  ),
  flag_has_recent_assessable_permit = list(
    display_name = "Recent Assessable Permit"
  ),
  total_mv = list(
    formula = TRUE, style = "price", hidden = TRUE, display_name = "Total MV"
  ),
  mv_difference = list(
    formula = TRUE, style = "price", hidden = TRUE,
    display_name = "MV Difference"
  )
)

# Schema for the Card Detail sheet.
card_detail_schema <- list(
  meta_pin = list(formula = TRUE, display_name = "PIN"),
  meta_card_num = list(display_name = "Card"),
  char_class = list(display_name = "Class"),
  meta_nbhd_code = list(display_name = "Nbhd."),
  meta_card_pct_total_fmv = list(
    style = "pct", display_name = "Card % Total (By Sqft)"
  ),
  pred_card_initial_fmv = list(
    style = "price", display_name = "Card Initial FMV"
  ),
  pred_card_final_fmv = list(
    style = "price", display_name = "Card Final FMV"
  ),
  char_yrblt = list(display_name = "Year Built"),
  char_beds = list(display_name = "# Beds"),
  char_ext_wall = list(display_name = "Ext. Wall"),
  char_bsmt = list(display_name = "Bsmt. Type"),
  char_bsmt_fin = list(display_name = "Bsmt. Finish"),
  char_air = list(display_name = "Central Air"),
  char_heat = list(display_name = "Heat"),
  char_bldg_sf = list(
    style = "comma", display_name = "Bld. S. F."
  ),
  char_type_resd = list(display_name = "Stories"),
  char_land_sf = list(
    style = "comma", display_name = "Lnd. S. F."
  ),
  char_apts = list(
    style = "right_align", display_name = "# Res. Units"
  ),
  char_ncu = list(
    style = "right_align", display_name = "# Comm. Units"
  )
)

template_path <- here("misc", "desk_review_template.xlsx")
validate_schema_vs_template(
  pin_detail_schema, template_path, "PIN Detail",
  header_row = 4
)
validate_schema_vs_template(
  card_detail_schema, template_path, "Card Detail",
  header_row = 4
)

# Formatting styles — defined once outside the per-town loop
style_price <- createStyle(numFmt = "$#,##0")
style_2digit_price <- createStyle(numFmt = "$#,##0.00")
style_2digit_num <- createStyle(numFmt = "0.00")
style_pct <- createStyle(numFmt = "PERCENTAGE")
style_comma <- createStyle(numFmt = "COMMA")
style_link <- createStyle(fontColour = "blue", textDecoration = "underline")
style_right_align <- createStyle(halign = "right")
style_price_highlight <- createStyle(fgFill = "#FFFFCC", numFmt = "$#,##0")

# Named map from schema style tags to style objects
wb_styles <- list(
  price           = style_price,
  price_highlight = style_price_highlight,
  `2digit_price`  = style_2digit_price,
  `2digit_num`    = style_2digit_num,
  pct             = style_pct,
  comma           = style_comma,
  right_align     = style_right_align
)

# Write raw data to sheets for parcel details
for (town in unique(assessment_pin_prepped$township_code)) {
  message("Now processing: ", town_convert(town))

  # Filter overall data to specific township
  assessment_pin_filtered <- assessment_pin_prepped %>%
    filter(township_code == town) %>%
    select(-township_code)

  # Load the excel workbook template from file
  wb <- loadWorkbook(here("misc", "desk_review_template.xlsx"))


  # 5.1. PIN-Level -------------------------------------------------------------

  # Get range of rows in the PIN data + number of header rows
  num_head <- 6 # Number of header rows
  pin_row_range <- (num_head + 1):(nrow(assessment_pin_filtered) + num_head)
  pin_row_range_w_header <- c(num_head, pin_row_range)
  pin_col_range <- seq_along(pin_detail_schema)

  # Pre-compute column letters for formula strings and conditional formatting
  # rules — derived from the schema so they update automatically when columns
  # are added or removed
  fmv_round_letter <- int2col(
    col_pos(pin_detail_schema, "pred_pin_final_fmv_round")
  )
  prior_tot_letter <- int2col(col_pos(pin_detail_schema, "prior_near_tot"))
  sale_1_price_letter <- int2col(
    col_pos(pin_detail_schema, "sale_recent_1_price")
  )

  assessment_pin_w_row_ids <- assessment_pin_filtered %>%
    tibble::rowid_to_column("row_id") %>%
    mutate(row_id = row_id + num_head)

  # Calculate MVs so we can store them as separate, hidden columns for use
  # in the neighborhood breakouts pivot table
  assessment_pin_mvs <- assessment_pin_w_row_ids %>%
    mutate(
      total_mv = glue("={fmv_round_letter}{row_id}"),
      mv_difference = glue(
        "=({fmv_round_letter}{row_id})",
        " - ({prior_tot_letter}{row_id})"
      )
    ) %>%
    select(total_mv, mv_difference)

  # Calculate sales ratios, and use a formula so that they update dynamically
  # if the spreadsheet user updates the FMV
  assessment_pin_sale_ratios <- assessment_pin_w_row_ids %>%
    mutate(
      sale_ratio = glue(
        '=IF(ISBLANK({sale_1_price_letter}{row_id}), "",',
        " {fmv_round_letter}{row_id}",
        " / {sale_1_price_letter}{row_id})"
      )
    )

  # Mark AV fields and sales ratio fields as formulas, since these fields
  # compute values based on other fields
  class(assessment_pin_mvs$total_mv) <- c(
    class(assessment_pin_mvs$total_mv), "formula"
  )
  class(assessment_pin_mvs$mv_difference) <- c(
    class(assessment_pin_mvs$mv_difference), "formula"
  )
  class(assessment_pin_sale_ratios$sale_ratio) <- c(
    class(assessment_pin_sale_ratios$sale_ratio), "formula"
  )

  # Generate sheet and column headers
  model_header <- str_to_title(paste(
    params$assessment$year, "Model"
  ))
  comp_header <- str_to_title(paste(
    params$ratio_study$near_year, params$ratio_study$near_stage
  ))
  sheet_header <- str_to_title(glue::glue(
    comp_header, "Values vs.", model_header, "Values - Parcel-Level Results",
    .sep = " "
  ))

  pin_sheet_name <- "PIN Detail"
  class(assessment_pin_filtered$meta_pin) <- c(
    class(assessment_pin_filtered$meta_pin), "formula"
  )

  # Apply cell styles driven by the schema — one addStyle call per style group
  for (style_name in names(wb_styles)) {
    style_cols <- cols_with_style(pin_detail_schema, style_name)
    if (length(style_cols) == 0) next
    addStyle(wb, pin_sheet_name,
      style = wb_styles[[style_name]],
      rows = pin_row_range, cols = style_cols, gridExpand = TRUE
    )
  }
  addFilter(wb, pin_sheet_name, num_head, pin_col_range)

  # Format YoY % change column with a range of colors from low to high
  conditionalFormatting(
    wb, pin_sheet_name,
    cols = col_pos(pin_detail_schema, "prior_near_yoy_change_pct"),
    rows = pin_row_range,
    style = c("#F8696B", "#FFFFFF", "#00B0F0"),
    rule = c(-1, 0, 1),
    type = "colourScale"
  )
  # Format sale columns red when an outlier flag is present. Applied once per
  # sale group, with the anchor cell derived from the schema.
  for (sale_num in 1:2) {
    reason_col <- col_pos(
      pin_detail_schema,
      paste0("sale_recent_", sale_num, "_outlier_reason")
    )
    outlier_rule <- paste0("$", int2col(reason_col), num_head + 1, '!=""')
    conditionalFormatting(wb, pin_sheet_name,
      cols = cols_with_cond(
        pin_detail_schema, paste0("sale_outlier_", sale_num)
      ),
      rows = pin_row_range,
      style = createStyle(bgFill = "#FF9999"),
      rule = outlier_rule,
      type = "expression"
    )
  }

  # Write PIN-level data to workbook
  writeData(
    wb, pin_sheet_name, assessment_pin_filtered,
    startCol = 1, startRow = num_head + 1, colNames = FALSE
  )

  # Write formulas and headers to workbook
  writeFormula(
    wb, pin_sheet_name,
    assessment_pin_filtered$meta_pin,
    startRow = num_head + 1
  )
  writeFormula(
    wb, pin_sheet_name,
    x = assessment_pin_filtered$homeval_report,
    startCol = col_pos(pin_detail_schema, "homeval_report"),
    startRow = num_head + 1
  )
  writeFormula(
    wb, pin_sheet_name,
    assessment_pin_sale_ratios$sale_ratio,
    startCol = col_pos(pin_detail_schema, "sale_ratio"),
    startRow = num_head + 1
  )
  writeData(
    wb, pin_sheet_name, tibble(sheet_header),
    startCol = 2, startRow = 1, colNames = FALSE
  )
  writeData(
    wb, pin_sheet_name, tibble(params$export$run_id),
    startCol = 3, startRow = 3, colNames = FALSE
  )
  writeData(
    wb, pin_sheet_name, tibble(comp_header),
    startCol = col_pos(pin_detail_schema, "prior_near_land"),
    startRow = 5, colNames = FALSE
  )
  writeData(
    wb, pin_sheet_name, tibble(model_header),
    startCol = col_pos(pin_detail_schema, "pred_pin_final_fmv"),
    startRow = 5, colNames = FALSE
  )

  # Write hidden formulas
  writeFormula(
    wb, pin_sheet_name,
    assessment_pin_mvs$total_mv,
    startCol = col_pos(pin_detail_schema, "total_mv"),
    startRow = num_head + 1
  )
  writeFormula(
    wb, pin_sheet_name,
    assessment_pin_mvs$mv_difference,
    startCol = col_pos(pin_detail_schema, "mv_difference"),
    startRow = num_head + 1
  )
  hidden_cols <- cols_hidden(pin_detail_schema)
  setColWidths(
    wb, pin_sheet_name,
    hidden_cols,
    widths = rep(1, length(hidden_cols)),
    hidden = rep(TRUE, length(hidden_cols)),
    ignoreMergedCells = FALSE
  )

  # Add a named range for the PIN-level data, which the template will use
  # to populate the Neighborhood Breakouts pivot table
  createNamedRegion(
    wb, pin_sheet_name,
    cols = pin_col_range, rows = pin_row_range_w_header,
    name = "pin_detail_range", overwrite = TRUE
  )


  # 5.2. Card-Level ------------------------------------------------------------

  # Filter overall data to specific township
  assessment_card_filtered <- assessment_card_prepped %>%
    filter(township_code == town) %>%
    select(-township_code)

  card_sheet_name <- "Card Detail"
  class(assessment_card_filtered$meta_pin) <- c(
    class(assessment_card_filtered$meta_pin), "formula"
  )

  # Get range of rows in the card data + number of header rows
  card_num_head <- 4
  card_row_range <-
    (card_num_head + 1):(nrow(assessment_card_filtered) + card_num_head)
  card_col_range <- seq_along(card_detail_schema)

  # Apply cell styles driven by the schema
  for (style_name in names(wb_styles)) {
    style_cols <- cols_with_style(card_detail_schema, style_name)
    if (length(style_cols) == 0) next
    addStyle(wb, card_sheet_name,
      style = wb_styles[[style_name]],
      rows = card_row_range, cols = style_cols, gridExpand = TRUE
    )
  }
  addFilter(wb, card_sheet_name, card_num_head, card_col_range)

  # Write card-level data to workbook
  writeData(
    wb, card_sheet_name, assessment_card_filtered,
    startCol = 1, startRow = card_num_head + 1, colNames = FALSE
  )

  # Write formulas and headers to workbook
  writeFormula(
    wb, card_sheet_name,
    assessment_card_filtered$meta_pin,
    startRow = card_num_head + 1
  )
  writeData(
    wb, card_sheet_name, tibble(model_header),
    startCol = col_pos(card_detail_schema, "meta_card_pct_total_fmv"),
    startRow = card_num_head - 1, colNames = FALSE
  )

  # 5.3. Save output -----------------------------------------------------------

  # Save workbook to file based on town name
  workbook_name <- glue(
    params$assessment$year,
    str_replace(town_convert(town), " ", "_"),
    "Initial_Model_Values.xlsx",
    .sep = "_"
  )
  saveWorkbook(
    wb,
    here(
      "output", "desk_review", workbook_name
    ),
    overwrite = TRUE
  )
  rm(wb)
}

### NOTE ###
# OpenXLSX is not perfect and messes up the macros and formatting on saved
# workbooks. To finish each workbook, you must manually:

# 1. Open the Neighborhood Breakouts sheet and ensure that the values are
#    all formatted correctly in the pivot table; if not (e.g. if
#    `Average of YoY ∆ %` is formatted as a date when it should be a percentage)
#    then manually update the formatting by selecting
#    PivotTable Fields > Values > {fieldname} > Value Field Settings... >
#    Number Format.


#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 6. Prep iasWorld Upload ------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Preparing data for iasWorld export")

# Here we want to extract the building value for each card for upload to
# iasWorld. Land valuation is handled via rates in iasWorld, so no
# need to include the land portion of each PIN
upload_data <- assessment_pin %>%
  left_join(
    assessment_card,
    by = c("township_code", "meta_pin"),
    multiple = "all"
  ) %>%
  # Calculate the UNPRORATED building value of each card using the same
  # distribution method from the assessment stage
  mutate(
    pred_card_final_fmv_no_prorate = pred_pin_final_fmv_bldg_no_prorate *
      meta_card_pct_total_fmv,
    temp_card_frac_prop = pred_card_final_fmv_no_prorate -
      as.integer(pred_card_final_fmv_no_prorate)
  ) %>%
  group_by(meta_pin) %>%
  arrange(desc(temp_card_frac_prop)) %>%
  mutate(
    temp_add_to_final = as.numeric(
      n() > 1 & row_number() == 1 & temp_card_frac_prop > 0.1e-7
    ),
    temp_add_diff = temp_add_to_final * round(
      sum(pred_card_final_fmv_no_prorate, na.rm = TRUE) -
        sum(as.integer(pred_card_final_fmv_no_prorate), na.rm = TRUE)
    ),
    pred_card_final_fmv_no_prorate = round(
      as.integer(pred_card_final_fmv_no_prorate) + temp_add_diff
    )
  ) %>%
  ungroup() %>%
  select(
    township_code,
    PARID = meta_pin,
    CARD = meta_card_num,
    MV = pred_card_final_fmv_no_prorate
  )


#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 7. Export iasWorld Upload ----------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Write each town to a CSV for mass upload
for (town in unique(upload_data$township_code)) {
  message("Now processing: ", town_convert(town))

  upload_data_fil <- upload_data %>%
    filter(township_code == town) %>%
    select(-township_code) %>%
    arrange(PARID, CARD)

  write_csv(
    x = upload_data_fil,
    file = here(
      "output", "iasworld",
      glue(
        params$assessment$year,
        str_replace(town_convert(town), " ", "_"),
        "iasworld_upload.csv",
        .sep = "_"
      )
    ),
    na = "",
    col_names = TRUE
  )
}

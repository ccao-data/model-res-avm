#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Setup ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# NOTE: See DESCRIPTION for library dependencies and R/setup.R for
# variables used in each pipeline stage

# Load libraries, helpers, and recipes from files
purrr::walk(list.files("R/", "\\.R$", full.names = TRUE), source)

# Load additional dev R libraries (see README#managing-r-dependencies)
suppressPackageStartupMessages({
  library(DBI)
  library(openxlsx)
  library(RJDBC)
})

# Setup the Athena JDBC driver
aws_athena_jdbc_driver <- RJDBC::JDBC(
  driverClass = "com.simba.athena.jdbc.Driver",
  classPath = list.files("~/drivers", "^Athena.*jar$", full.names = TRUE),
  identifier.quote = "'"
)

# Establish Athena connection
AWS_ATHENA_CONN_JDBC <- dbConnect(
  aws_athena_jdbc_driver,
  url = Sys.getenv("AWS_ATHENA_JDBC_URL"),
  aws_credentials_provider_class = Sys.getenv("AWS_CREDENTIALS_PROVIDER_CLASS"),
  Schema = "Default",
  WorkGroup = "read-only-with-scan-limit"
)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. Pull Vacant Land ----------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Pulling vacant land data from Athena")

# Need to pull all vacant land PINs so that they can be valued separately from
# the regression model using a flat rate per neighborhood

# Each land PIN can have multiple "lines" that potentially receive different
# rates, here we grab the lines and square footage for each PIN
land <- dbGetQuery(
  conn = AWS_ATHENA_CONN_JDBC, glue("
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
  conn = AWS_ATHENA_CONN_JDBC, glue("
  SELECT
      uni.township_code,
      uni.pin AS meta_pin,
      uni.class AS meta_class,
      uni.nbhd_code AS meta_nbhd_code,
      uni.prop_address_full AS loc_property_address,
      uni.prop_address_city_name As loc_property_city,
      uni.prop_address_state AS loc_property_state,
      uni.prop_address_zipcode_1 AS loc_property_zip,
      uni.cook_municipality_name AS loc_cook_municipality_name,
      uni.tieback_key_pin AS meta_tieback_key_pin,
      uni.tieback_proration_rate AS meta_tieback_proration_rate,
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
  conn = AWS_ATHENA_CONN_JDBC, glue("
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
  mutate(meta_sale_date = ymd_hms(meta_sale_date)) %>%
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
  conn = AWS_ATHENA_CONN_JDBC, glue("
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
  conn = AWS_ATHENA_CONN_JDBC, glue("
  SELECT *
  FROM model.assessment_pin
  WHERE run_id = '{params$export$run_id}'
  AND meta_triad_code = '{params$export$triad_code}'
  ")
)

# Pull card-level data only for all PINs. Needed for upload, since values are
# tracked by card, even though they're presented by PIN
assessment_card <- dbGetQuery(
  conn = AWS_ATHENA_CONN_JDBC, glue("
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




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4. Prep Desk Review ----------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Preparing data for Desk Review export")

# Merge vacant land data with data from the residential AVM
assessment_pin_merged <- assessment_pin %>%
  mutate(
    meta_complex_id = as.numeric(meta_complex_id),
    across(ends_with("_date"), ymd),
    across(starts_with("flag_"), as.numeric),
    across(where(is.numeric), ~ na_if(.x, Inf))
  ) %>%
  bind_rows(vacant_land_merged) %>%
  filter(!is.na(pred_pin_final_fmv_land)) %>%
  mutate(across(ends_with("_date"), as_date))

# Prep data with a few additional columns + put everything in the right
# order for DR sheets
assessment_pin_prepped <- assessment_pin_merged %>%
  mutate(
    prior_near_land_rate = round(prior_near_land / char_land_sf, 2),
    prior_near_bldg_rate = round(prior_near_bldg / char_total_bldg_sf, 2),
    prior_near_land_pct_total = round(prior_near_land / prior_near_tot, 4),
    property_full_address = paste0(
      loc_property_address,
      ", ", loc_property_city, " ", loc_property_state,
      ", ", loc_property_zip
    )
  ) %>%
  select(
    township_code, meta_pin, meta_class, meta_nbhd_code,
    property_full_address, loc_cook_municipality_name, meta_complex_id,
    meta_pin_num_cards, meta_tieback_key_pin, meta_tieback_proration_rate,
    prior_near_land, prior_near_bldg, prior_near_tot,
    prior_near_land_rate, prior_near_bldg_rate, prior_near_land_pct_total,
    pred_pin_final_fmv, pred_pin_final_fmv_land, pred_pin_final_fmv_bldg,
    pred_pin_final_fmv_round, land_rate_per_sqft, pred_pin_land_rate_effective,
    pred_pin_bldg_rate_effective, pred_pin_land_pct_total,
    prior_near_yoy_change_nom, prior_near_yoy_change_pct,
    sale_recent_1_date, sale_recent_1_price, sale_recent_1_document_num,
    sale_recent_2_date, sale_recent_2_price, sale_recent_2_document_num,
    char_yrblt, char_total_bldg_sf, char_type_resd, char_land_sf,
    flag_pin_is_prorated, flag_proration_sum_not_1,
    flag_pin_is_multicard, flag_pin_is_multiland,
    flag_land_gte_95_percentile, flag_bldg_gte_95_percentile,
    flag_land_value_capped, flag_hie_num_expired,
    flag_prior_near_to_pred_unchanged, flag_pred_initial_to_final_changed,
    flag_prior_near_yoy_inc_gt_50_pct, flag_prior_near_yoy_dec_gt_5_pct,
    flag_char_missing_critical_value
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
    )
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
    township_code, meta_pin, meta_card_num, meta_class, meta_nbhd_code,
    meta_card_pct_total_fmv, pred_card_initial_fmv, pred_card_final_fmv,
    char_yrblt, char_beds, char_ext_wall, char_heat, char_bldg_sf,
    char_type_resd, char_land_sf
  ) %>%
  mutate(
    meta_pin = glue(
      '=HYPERLINK("https://www.cookcountyassessor.com/pin/{meta_pin}",
      "{meta_pin}")'
    )
  ) %>%
  arrange(township_code, meta_pin, meta_card_num)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 5. Export Desk Review --------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Write raw data to sheets for parcel details
for (town in unique(assessment_pin_prepped$township_code)) {
  message("Now processing: ", town_convert(town))


  ## 5.1. PIN-Level ------------------------------------------------------------

  # Filter overall data to specific township
  assessment_pin_filtered <- assessment_pin_prepped %>%
    filter(township_code == town) %>%
    select(-township_code)

  # Generate sheet and column headers
  model_header <- str_to_title(paste(
    params$assessment$year, "Model"
  ))
  comp_header <- str_to_title(paste(
    params$ratio_study$near_year, params$ratio_study$near_stage
  ))
  sheet_header <- str_to_title(glue(
    comp_header, "Values vs.", model_header, "Values - Parcel-Level Results",
    .sep = " "
  ))

  pin_sheet_name <- "PIN Detail"
  class(assessment_pin_filtered$meta_pin) <- c(
    class(assessment_pin_filtered$meta_pin), "formula"
  )

  # Get range of rows in the PIN data + number of header rows
  pin_row_range <- 7:(nrow(assessment_pin_filtered) + 9)

  # Load the excel workbook template from file
  wb <- loadWorkbook(here("misc", "desk_review_template.xlsx"))

  # Create formatting styles
  style_price <- createStyle(numFmt = "$#,##0")
  style_2digit <- createStyle(numFmt = "$#,##0.00")
  style_pct <- createStyle(numFmt = "PERCENTAGE")
  style_comma <- createStyle(numFmt = "COMMA")

  # Add styles to PIN sheet
  addStyle(
    wb, pin_sheet_name,
    style = style_price,
    rows = pin_row_range, cols = c(10:12, 16:19, 24, 27, 30), gridExpand = TRUE
  )
  addStyle(
    wb, pin_sheet_name,
    style = style_2digit,
    rows = pin_row_range, cols = c(13:14, 20:22), gridExpand = TRUE
  )
  addStyle(
    wb, pin_sheet_name,
    style = style_pct,
    rows = pin_row_range, cols = c(9, 15, 23, 25), gridExpand = TRUE
  )
  addStyle(
    wb, pin_sheet_name,
    style = style_comma,
    rows = pin_row_range, cols = c(33, 35), gridExpand = TRUE
  )
  addFilter(wb, pin_sheet_name, 6, 1:48)

  # Write PIN-level data to workbook
  writeData(
    wb, pin_sheet_name, assessment_pin_filtered,
    startCol = 1, startRow = 7, colNames = FALSE
  )

  # Write formulas and headers to workbook
  writeFormula(
    wb, pin_sheet_name,
    assessment_pin_filtered$meta_pin,
    startRow = 7
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
    startCol = 10, startRow = 5, colNames = FALSE
  )
  writeData(
    wb, pin_sheet_name, tibble(model_header),
    startCol = 16, startRow = 5, colNames = FALSE
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
  card_row_range <- 5:(nrow(assessment_card_filtered) + 6)

  # Add styles to card sheet
  addStyle(
    wb, card_sheet_name,
    style = style_price,
    rows = card_row_range, cols = c(6:7), gridExpand = TRUE
  )
  addStyle(
    wb, card_sheet_name,
    style = style_pct,
    rows = card_row_range, cols = c(5), gridExpand = TRUE
  )
  addStyle(
    wb, card_sheet_name,
    style = style_comma,
    rows = card_row_range, cols = c(12, 14), gridExpand = TRUE
  )
  addFilter(wb, card_sheet_name, 4, 1:14)

  # Write card-level data to workbook
  writeData(
    wb, card_sheet_name, assessment_card_filtered,
    startCol = 1, startRow = 5, colNames = FALSE
  )

  # Write formulas and headers to workbook
  writeFormula(
    wb, card_sheet_name,
    assessment_card_filtered$meta_pin,
    startRow = 5
  )
  writeData(
    wb, card_sheet_name, tibble(model_header),
    startCol = 5, startRow = 3, colNames = FALSE
  )

  # Save workbook to file based on town name
  saveWorkbook(
    wb,
    here(
      "output", "desk_review",
      glue(
        params$assessment$year,
        str_replace(town_convert(town), " ", "_"),
        "Initial_Model_Values.xlsx",
        .sep = "_"
      )
    ),
    overwrite = TRUE
  )
  rm(wb)
}




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 5. Prep iasWorld Upload ------------------------------------------------------
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
    PARID = meta_pin, CARD = meta_card_num,
    USER37 = pred_card_final_fmv_no_prorate,
    USER24 = meta_tieback_proration_rate.x,
    OVRRCNLD = pred_card_final_fmv
  )




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 6. Export iasWorld Upload ----------------------------------------------------
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

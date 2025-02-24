#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Setup ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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
      uni.tax_municipality_name AS loc_cook_municipality_name,
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

if (comp_enable) {
  message("Pulling comp data from Athena")
  # We don't know how many comp_score_{x} columns there are, and selecting
  # columns via wildcard in SQL is complicated, so just query everything and
  # then select the necessary columns in R
  comps <- dbGetQuery(
    conn = AWS_ATHENA_CONN_NOCTUA, glue("
    SELECT *
    FROM model.comp
    WHERE run_id = '{params$export$run_id}'
    ")
  ) %>%
    mutate(
      overall_comp_score = select(., starts_with("comp_score_")) %>%
        rowMeans(na.rm = TRUE)
    ) %>%
    select(
      pin, overall_comp_score,
      comp_pin_1, comp_document_num_1, comp_score_1,
      comp_pin_2, comp_document_num_2, comp_score_2
    )

  # Merge comp data into assessment data. Start with single-card PINs, where
  # the comps for the card are the same as the comps for the parcel
  assessment_pin_single_card <- assessment_pin_w_land %>%
    filter(meta_pin_num_cards == 1 | is.na(meta_pin_num_cards)) %>%
    left_join(comps, by = join_by(meta_pin == pin))

  # In cases where the PIN has multiple cards, choose the highest-performing
  # comps across all cards as the top two. This is necessary because comps are
  # defined at the card level and not the parcel level.
  # To start, group the comps by PIN and select the comps with the top
  # overall_score. This is ugly datatable code but it is much faster than the
  # dplyr version using group_by and slice_max
  comps_by_top_score <- comps[
    comps[, .I[overall_comp_score == max(overall_comp_score)], by = pin]$V1
  ] %>%
    distinct()
  # Next, join the PIN-level data to the top-scoring comps
  assessment_pin_multicard <- assessment_pin_w_land %>%
    filter(meta_pin_num_cards > 1) %>%
    left_join(comps_by_top_score, by = join_by(meta_pin == pin))
  # Finally, combine both single and multicard PINs
  assessment_pin_merged <- bind_rows(
    assessment_pin_single_card, assessment_pin_multicard
  ) %>%
    arrange(meta_pin)

  # Query and filter training data to use as a comp detail view
  training_data <- read_parquet(paths$input$training$local) %>%
    filter(!ind_pin_is_multicard, !sv_is_outlier)
} else {
  # Add NA columns for comps so that assessment_pin_merged has the same
  # shape in both conditional branches
  assessment_pin_merged <- assessment_pin_merged %>%
    cbind(
      tibble(
        overall_comp_score = rep(NA, each = nrow(assessment_pin_merged)),
        comp_pin_1 = rep(NA, each = nrow(assessment_pin_merged)),
        comp_score_1 = rep(NA, each = nrow(assessment_pin_merged)),
        comp_pin_2 = rep(NA, each = nrow(assessment_pin_merged)),
        comp_score_2 = rep(NA, each = nrow(assessment_pin_merged)),
      )
    )
  training_data <- tibble()
}

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
    ),
    valuations_note = NA, # Empty notes field for Valuations to fill out
    sale_ratio = NA # Initialize as NA so we can fill out with a formula later
  ) %>%
  # Add assessable permit flag
  left_join(flag_assessable_permits, by = c("meta_pin" = "pin")) %>%
  mutate(
    flag_has_recent_assessable_permit =
      as.numeric(has_recent_assessable_permit),
    sale_recent_1_outlier_reasons = str_remove_all(ifelse(
      sale_recent_1_is_outlier,
      paste(
        str_replace_na(sale_recent_1_outlier_reason1, ""),
        str_replace_na(sale_recent_1_outlier_reason2, ""),
        sep = ", "
      ),
      NA_character_
    ), ", $"),
    sale_recent_2_outlier_reasons = str_remove_all(ifelse(
      sale_recent_2_is_outlier,
      paste(
        str_replace_na(sale_recent_2_outlier_reason1, ""),
        str_replace_na(sale_recent_2_outlier_reason2, ""),
        sep = ", "
      ),
      NA_character_
    ), ", $")
  ) %>%
  # Select fields for output to workbook
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
    sale_ratio, valuations_note,
    sale_recent_1_date, sale_recent_1_price,
    sale_recent_1_outlier_reasons, sale_recent_1_document_num,
    sale_recent_2_date, sale_recent_2_price,
    sale_recent_2_outlier_reasons, sale_recent_2_document_num,
    char_yrblt, char_beds, char_ext_wall, char_bsmt, char_bsmt_fin, char_air,
    char_heat, char_total_bldg_sf, char_type_resd, char_land_sf, char_apts,
    char_ncu,
    comp_pin_1, comp_document_num_1, comp_score_1,
    comp_pin_2, comp_document_num_2, comp_score_2, overall_comp_score,
    flag_pin_is_prorated, flag_proration_sum_not_1,
    flag_pin_is_multicard, flag_pin_is_multiland,
    flag_land_gte_95_percentile, flag_bldg_gte_95_percentile,
    flag_land_value_capped, flag_hie_num_expired,
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

# Write raw data to sheets for parcel details
for (town in unique(assessment_pin_prepped$township_code)) {
  message("Now processing: ", town_convert(town))

  # Filter overall data to specific township
  assessment_pin_filtered <- assessment_pin_prepped %>%
    filter(township_code == town) %>%
    select(-township_code)

  ## 5.1 Comp details ----------------------------------------------------------

  # Filter the training data so that we only display sales that are referenced.
  # First, get the indexes of every sale whose comp is referenced in
  # the PIN-level details
  training_pin_in_comps <- training_data$meta_sale_document_num %in% (
    assessment_pin_filtered %>%
      select(comp_document_num_1, comp_document_num_2) %>%
      unlist()
  )
  # Next, filter the training data so only referenced sales are included.
  # This is ugly but faster than the equivalent filter() operation
  training_data_filtered <- training_data[training_pin_in_comps, ]
  # Select only the columns that are needed for the comps detail view
  training_data_selected <- training_data_filtered %>%
    select(
      meta_pin, meta_sale_document_num, meta_sale_price,
      meta_sale_date, char_class, meta_nbhd_code,
      loc_property_address, char_yrblt, char_beds, char_ext_wall, char_bsmt,
      char_bsmt_fin, char_air, char_heat, char_bldg_sf, char_type_resd,
      char_land_sf, char_apts, char_ncu
    ) %>%
    ccao::vars_recode(code_type = "long") %>%
    mutate(
      char_apts = format_char_apts(char_apts),
      char_ncu = ifelse(char_class == "212", char_ncu, NA),
      meta_pin = glue(
        '=HYPERLINK("https://www.cookcountyassessor.com/pin/{meta_pin}",
      "{meta_pin}")'
      )
    )

  # It seems like Excel can only handle between-sheet links if the linked
  # sheet name has no spaces... Perhaps there's an undocumented workaround,
  # but for now, use a sheet name with no spaces for the comp detail view
  comp_sheet_name <- "Comparables"

  # Get range of rows in the comp data + number of header rows
  comp_row_range <- 5:(nrow(training_data_selected) + 6)

  # Load the excel workbook template from file
  wb <- loadWorkbook(here("misc", "desk_review_template.xlsx"))

  # Create formatting styles
  style_price <- createStyle(numFmt = "$#,##0")
  style_2digit_price <- createStyle(numFmt = "$#,##0.00")
  style_2digit_num <- createStyle(numFmt = "0.00")
  style_pct <- createStyle(numFmt = "PERCENTAGE")
  style_comma <- createStyle(numFmt = "COMMA")
  style_link <- createStyle(fontColour = "blue", textDecoration = "underline")
  style_right_align <- createStyle(halign = "right")

  # Add styles to comp sheet
  addStyle(
    wb, comp_sheet_name,
    style = style_price,
    rows = comp_row_range, cols = c(3), gridExpand = TRUE
  )
  addStyle(
    wb, comp_sheet_name,
    style = style_comma,
    rows = comp_row_range, cols = c(15, 17), gridExpand = TRUE
  )
  addStyle(
    wb, comp_sheet_name,
    style = style_right_align,
    rows = comp_row_range, cols = 18:19, gridExpand = TRUE
  )
  addFilter(wb, comp_sheet_name, 4, 1:19)

  class(training_data_selected$meta_pin) <- c(
    class(training_data_selected$meta_pin), "formula"
  )

  writeFormula(
    wb, comp_sheet_name,
    training_data_selected$meta_pin,
    startRow = 5
  )

  # Write comp data to workbook
  writeData(
    wb, comp_sheet_name, training_data_selected,
    startCol = 1, startRow = 5, colNames = FALSE
  )

  # 5.2. PIN-Level -------------------------------------------------------------

  # Update PIN-level data to link to comps detail sheet
  training_data_ids <- training_data_filtered %>%
    tibble::rowid_to_column("comp_detail_id") %>%
    select(meta_sale_document_num, comp_detail_id)

  assessment_pin_filtered <- assessment_pin_filtered %>%
    left_join(
      training_data_ids,
      by = join_by(
        comp_document_num_1 == meta_sale_document_num
      )
    ) %>%
    rename(comp_document_num_1_coord = comp_detail_id) %>%
    left_join(
      training_data_ids,
      by = join_by(
        comp_document_num_2 == meta_sale_document_num
      )
    ) %>%
    rename(comp_document_num_2_coord = comp_detail_id) %>%
    mutate(
      across(
        matches("_coord"),
        ~ ifelse(
          is.na(.x),
          NA,
          getCellRefs(data.frame(row = .x + 4, column = 2))
        )
      )
    ) %>%
    mutate(
      comp_document_num_1 = ifelse(
        is.na(comp_document_num_1_coord),
        NA,
        glue::glue(
          '=HYPERLINK("#{comp_sheet_name}!{comp_document_num_1_coord}",',
          '"{comp_document_num_1}")'
        )
      ),
      comp_document_num_2 = ifelse(
        is.na(comp_document_num_2_coord),
        NA,
        glue::glue(
          '=HYPERLINK("#{comp_sheet_name}!{comp_document_num_2_coord}",',
          '"{comp_document_num_2}")'
        )
      )
    ) %>%
    select(-ends_with("_coord"), -comp_pin_1, -comp_pin_2)

  # Get range of rows in the PIN data + number of header rows
  num_head <- 6 # Number of header rows
  pin_row_range <- (num_head + 1):(nrow(assessment_pin_filtered) + num_head)
  pin_row_range_w_header <- c(num_head, pin_row_range)
  pin_col_range <- 1:68 # Don't forget the two hidden rows at the end

  assessment_pin_w_row_ids <- assessment_pin_filtered %>%
    tibble::rowid_to_column("row_id") %>%
    mutate(row_id = row_id + num_head)

  # Calculate AVs so we can store them as separate, hidden columns for use
  # in the neighborhood breakouts pivot table
  assessment_pin_avs <- assessment_pin_w_row_ids %>%
    mutate(
      total_av = glue::glue("=S{row_id} * 0.1"),
      av_difference = glue::glue("=(S{row_id} * 0.1) - (L{row_id} * 0.1)")
    ) %>%
    select(total_av, av_difference)

  # Calculate sales ratios, and use a formula so that they update dynamically
  # if the spreadsheet user updates the FMV
  assessment_pin_sale_ratios <- assessment_pin_w_row_ids %>%
    mutate(
      sale_ratio = glue::glue(
        '=IF(ISBLANK(AC{row_id}), "", S{row_id} / AC{row_id})'
      )
    )

  # Mark AV fields and sales ratio fields as formulas, since these fields
  # compute values based on other fields
  class(assessment_pin_avs$total_av) <- c(
    class(assessment_pin_avs$total_av), "formula"
  )
  class(assessment_pin_avs$av_difference) <- c(
    class(assessment_pin_avs$av_difference), "formula"
  )
  class(assessment_pin_sale_ratios$sale_ratio) <- c(
    class(assessment_pin_sale_ratios$sale_ratio), "formula"
  )

  # Make comp doc num fields formulas so Excel understands the links
  class(assessment_pin_filtered$comp_document_num_1) <- c(
    class(assessment_pin_filtered$comp_document_num_1), "formula"
  )
  class(assessment_pin_filtered$comp_document_num_2) <- c(
    class(assessment_pin_filtered$comp_document_num_2), "formula"
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

  # Add styles to PIN sheet
  addStyle(
    wb, pin_sheet_name,
    style = style_price,
    rows = pin_row_range,
    cols = c(10:12, 16:18, 24, 29, 33, 67, 68), gridExpand = TRUE
  )
  addStyle(
    wb, pin_sheet_name,
    style = style_2digit_price,
    rows = pin_row_range, cols = c(13:14, 20:22), gridExpand = TRUE
  )
  addStyle(
    wb, pin_sheet_name,
    style = style_2digit_num,
    rows = pin_row_range, cols = c(26), gridExpand = TRUE
  )
  addStyle(
    wb, pin_sheet_name,
    style = style_pct,
    rows = pin_row_range, cols = c(9, 15, 23, 25, 49, 51, 52), gridExpand = TRUE
  )
  addStyle(
    wb, pin_sheet_name,
    style = style_comma,
    rows = pin_row_range, cols = c(43, 45), gridExpand = TRUE
  )
  addStyle(
    wb, pin_sheet_name,
    style = createStyle(fgFill = "#FFFFCC", numFmt = "$#,##0"),
    rows = pin_row_range, cols = c(19), gridExpand = TRUE
  )
  addStyle(
    wb, pin_sheet_name,
    style = style_right_align,
    rows = pin_row_range, cols = c(37, 46, 47), gridExpand = TRUE
  )
  # For some reason comp links do not get autoformatted as links, possibly
  # due to Excel not parsing within-sheet links as hyperlinks for the purposes
  # of styling
  addStyle(
    wb, pin_sheet_name,
    style = style_link,
    rows = pin_row_range, cols = c(48, 50), gridExpand = TRUE
  )
  addFilter(wb, pin_sheet_name, 6, pin_col_range)

  # Format comp score columns with a range of colors from low (red) to high
  # (blue)
  conditionalFormatting(
    wb, pin_sheet_name,
    cols = c(49, 51, 52),
    rows = pin_row_range,
    style = c("#F8696B", "#FFFFFF", "#00B0F0"),
    rule = c(0, 0.5, 1),
    type = "colourScale"
  )
  # Format YoY % change column with a range of colors from low to high
  conditionalFormatting(
    wb, pin_sheet_name,
    cols = c(25),
    rows = pin_row_range,
    style = c("#F8696B", "#FFFFFF", "#00B0F0"),
    rule = c(-1, 0, 1),
    type = "colourScale"
  )
  # Format sale columns such that they are red if the sale has an outlier flag
  conditionalFormatting(
    wb, pin_sheet_name,
    cols = 28:31,
    rows = pin_row_range,
    style = createStyle(bgFill = "#FF9999"),
    rule = '$AD7!=""',
    type = "expression"
  )
  # For some reason vector cols don't work with expressions, so we have
  # to duplicate the conditional formatting for the sale outlier flag above
  # to apply it to the second range of columns
  conditionalFormatting(
    wb, pin_sheet_name,
    cols = 32:35,
    rows = pin_row_range,
    style = createStyle(bgFill = "#FF9999"),
    rule = '$AH7!=""',
    type = "expression"
  )

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
  writeFormula(
    wb, pin_sheet_name,
    assessment_pin_sale_ratios$sale_ratio,
    startCol = 26, startRow = 7
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

  # Write hidden formulas
  writeFormula(
    wb, pin_sheet_name,
    assessment_pin_avs$total_av,
    startCol = 67,
    startRow = 7
  )
  writeFormula(
    wb, pin_sheet_name,
    assessment_pin_avs$av_difference,
    startCol = 68,
    startRow = 7
  )
  setColWidths(
    wb, pin_sheet_name,
    c(67, 68),
    widths = 1,
    hidden = c(TRUE, TRUE), ignoreMergedCells = FALSE
  )

  # Add a named range for the PIN-level data, which the template will use
  # to populate the Neighborhood Breakouts pivot table
  createNamedRegion(
    wb, pin_sheet_name,
    cols = pin_col_range, rows = pin_row_range_w_header,
    name = "pin_detail_range", overwrite = TRUE
  )


  # 5.3. Card-Level ------------------------------------------------------------

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
    rows = card_row_range, cols = c(15, 17), gridExpand = TRUE
  )
  addStyle(
    wb, card_sheet_name,
    style = style_right_align,
    rows = card_row_range, cols = c(18, 19), gridExpand = TRUE
  )
  addFilter(wb, card_sheet_name, 4, 1:19)

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

  # 5.4 Save output ------------------------------------------------------------

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

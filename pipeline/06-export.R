#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Setup ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Pre-allocate memory for java JDBC driver
options(java.parameters = "-Xmx10g")

# Load R libraries
library(aws.s3)
library(ccao)
library(DBI)
library(dplyr)
library(glue)
library(here)
library(openxlsx)
library(RJDBC)
library(stringr)
library(yaml)

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

# Load the parameters file containing the export settings
params <- read_yaml("params.yaml")




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. Pull Data -----------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Pull the PIN-level assessment data, which contains all the fields needed to
# create the review spreadsheets
assessment_pin <- dbGetQuery(
  conn = AWS_ATHENA_CONN_JDBC, glue("
  SELECT *
  FROM model.assessment_pin
  WHERE run_id = '{params$export$run_id_res}'
  AND meta_triad_code = '{params$export$triad_code}'
  ")
)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Prep ----------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Prep data with a few additional columns + put everything in the right
# order for DR sheets
assessment_pin_prepped <- assessment_pin %>%
  mutate(
    prior_near_land_rate = round(prior_near_land / char_land_sf, 2),
    prior_near_bldg_rate = round(prior_near_bldg / char_total_bldg_sf, 2),
    prior_near_land_pct_total = round(prior_near_land / prior_near_tot, 2),
    property_full_address = paste0(
      loc_property_address, 
      ", ", loc_property_city, " ", loc_property_state, 
      ", ",loc_property_zip
    )
  ) %>%
  select(
    township_code, meta_pin, meta_class, meta_nbhd_code,
    property_full_address, loc_cook_municipality_name,
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
  mutate(
    across(starts_with("flag_"), as.numeric),
    across(where(is.numeric), ~ na_if(.x, Inf))
  ) %>%
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




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4. Export Spreadsheets -------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Write raw data to sheets for parcel details
for (town in unique(assessment_pin_prepped$township_code)) {
  message("Now processing: ", town_convert(town))
  
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
  
  sheet_name <- "Parcel Detail"
  class(assessment_pin_prepped$meta_pin) <- c(
    class(assessment_pin_prepped$meta_pin), "formula"
  )
  
  # Get range of rows in the data + number of header rows
  row_range <- 8:(nrow(assessment_pin_filtered) + 9)
  
  # Load the excel workbook template from file 
  wb <- loadWorkbook(here("misc", "desk_review_template.xlsx"))
  
  # Create formatting styles
  style_price <- createStyle(numFmt = "$#,##0")
  style_2digit <- createStyle(numFmt = "$#,##0.00")
  style_pct <- createStyle(numFmt = "PERCENTAGE")
  style_comma <- createStyle(numFmt = "COMMA")
  
  # Add styles to workbook
  addStyle(
    wb, sheet_name, style = style_price,
    rows = row_range, cols = c(9:11, 15:18, 23, 26, 29), gridExpand = TRUE
  )
  addStyle(
    wb, sheet_name, style = style_2digit,
    rows = row_range, cols = c(12:13, 19:21), gridExpand = TRUE
  )
  addStyle(
    wb, sheet_name, style = style_pct,
    rows = row_range, cols = c(8, 14, 22, 24), gridExpand = TRUE
  )
  addStyle(
    wb, sheet_name, style = style_comma,
    rows = row_range, cols = c(32, 34), gridExpand = TRUE
  )
  addFilter(wb, sheet_name, 7, 1:49)
  
  # Write PIN-level data to workbook
  writeData(
    wb, sheet_name, assessment_pin_filtered,
    startCol = 1, startRow = 8, colNames = FALSE
  )
  
  # Write formulas and headers to workbook
  writeFormula(wb, sheet_name, assessment_pin_filtered$meta_pin, startRow = 8)
  writeData(
    wb, sheet_name, tibble(sheet_header),
    startCol = 1, startRow = 1, colNames = FALSE
  )
  writeData(
    wb, sheet_name, tibble(params$export$run_id_res),
    startCol = 2, startRow = 3, colNames = FALSE
  )
  writeData(
    wb, sheet_name, tibble(comp_header),
    startCol = 9, startRow = 6, colNames = FALSE
  )
  writeData(
    wb, sheet_name, tibble(model_header),
    startCol = 15, startRow = 6, colNames = FALSE
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

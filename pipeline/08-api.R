#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Setup ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Pre-allocate memory for java JDBC driver
options(java.parameters = "-Xmx10g")

# Load R libraries
suppressPackageStartupMessages({
  library(arrow)
  library(ccao)
  library(DBI)
  library(dplyr)
  library(glue)
  library(here)
  library(openxlsx)
  library(purrr)
  library(RJDBC)
  library(stringr)
  library(yaml)
})

# Load helpers and recipes from files
walk(list.files("R/", "\\.R$", full.names = TRUE), source)

# Load the parameters file containing the export settings
params <- read_yaml("params.yaml")

# Initialize a dictionary of file paths. See misc/file_dict.csv for details
run_id <- params$export$run_id
year <- substr(run_id, 1, 4)
paths <- model_file_dict(run_id, year)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. Load Data -----------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Loading data for API creation")

# Load metadata to get predictors used and other info
metadata <- read_parquet(paths$output$metadata$s3)
predictors <- metadata$model_predictor_all_name[[1]]
towns <- ccao::town_dict %>%
  filter(triad_code == params$export$triad_code) %>%
  pull(township_code)

# Load categorical variable dictionary for lookup and data validation
dict <- ccao::vars_dict %>%
  filter(
    var_data_type == "categorical",
    var_name_model %in% predictors,
    var_name_model != "meta_modeling_group"
  ) %>%
  distinct(var_name_pretty, var_code, var_value)

dict <- bind_rows(
  dict,
  ccao::vars_dict %>%
    filter(var_name_model == "meta_modeling_group") %>%
    distinct(var_name_pretty, var_code = var_value_short, var_value)
)

# Typically the most important predictors in CCAO models
top_predictors <- c(
  "meta_township_code", "meta_nbhd_code",
  "char_bldg_sf", "char_fbath", "char_yrblt", "char_land_sf", "char_frpl",
  "loc_school_elementary_district_geoid", "loc_school_secondary_district_geoid",
  "acs5_median_income_per_capita_past_year"
)

# Load the final card-level dataset
card_data <- arrow::open_dataset(
  file.path(
    gsub("\\/$", "", paths$output$assessment_card$s3),
    paste0("year=", year),
    paste0("run_id=", run_id, "/")
  )
) %>%
  collect()




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Export API Workbooks ------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Write raw data to sheets for parcel details
for (town in towns) {
  message("Now processing: ", town_convert(town))

  # Load data from file, then make it pretty for saving to sheet
  card_data_town <- card_data %>%
    filter(meta_township_code == town) %>%
    mutate(api_prediction = NA, api_prediction_rounded = NA) %>%
    select(
      meta_pin, meta_card_num, meta_class, pred_card_initial_fmv,
      api_prediction, api_prediction_rounded,
      all_of(top_predictors),
      meta_modeling_group, meta_tieback_proration_rate,
      starts_with("char_"),
      starts_with("loc_"),
      starts_with("time"),
      starts_with("prox_"),
      starts_with("acs5_"),
      starts_with("other_")
    ) %>%
    arrange(meta_pin, meta_card_num) %>%
    mutate(
      across(where(is.numeric), ~ round(.x, 8)),
      meta_pin = ccao::pin_format_pretty(meta_pin, full_length = TRUE)
    ) %>%
    var_encode(cols = starts_with("char_"))

  # Load workbook and styles
  wb <- loadWorkbook(here("misc", "model_api_template.xlsm"))
  pin_sheet_header <- paste0("Run ID: ", run_id)
  pin_row_range <- 6:(nrow(card_data_town) + 7)
  style_price <- createStyle(numFmt = "$#,##0")
  csht <- "Cards"
  dsht <- "Dictionary"

  # Write dictionary and data validation
  writeData(wb, dsht, dict, startCol = 1, startRow = 2, colNames = FALSE)
  mappings <- tribble(
    ~col, ~dict,
    "S", c(2, 3),
    "T", c(4, 9),
    "U", c(10, 12),
    "V", c(13, 15),
    "X", c(16, 19),
    "Y", c(20, 22),
    "Z", c(23, 26),
    "AA", c(27, 28),
    "AB", c(29, 30),
    "AC", c(31, 34),
    "AD", c(35, 42),
    "AF", c(43, 46),
    "AG", c(47, 49),
    "AI", c(50, 55),
    "AK", c(56, 57),
    "AL", c(58, 59),
    "AM", c(60, 65),
    "Q", c(66, 70)
  )

  pwalk(mappings, function(col, dict) {
    dataValidation(
      wb, csht,
      col = col2int(col), rows = pin_row_range,
      type = "list", value = glue("'{dsht}'!$B${dict[1]}:$B${dict[2]}")
    )
  })

  # Write the cleaned data to workbook
  addStyle(
    wb, csht,
    style = style_price,
    rows = pin_row_range, cols = 4:6, gridExpand = TRUE
  )
  writeData(
    wb, csht, tibble(pin_sheet_header),
    startCol = 2, startRow = 1, colNames = FALSE
  )
  writeData(
    wb, csht, card_data_town,
    startCol = 1, startRow = 6, colNames = FALSE
  )

  # Save the file workbook to file
  saveWorkbook(
    wb,
    here(
      "output", "api_workbook",
      glue(
        year,
        str_replace(town_convert(town), " ", "_"),
        "API_Workbook.xlsm",
        .sep = "_"
      )
    ),
    overwrite = TRUE
  )
  rm(wb)

  ### NOTE ###
  # OpenXLSX is not perfect and messes up the macros and formatting on saved
  # workbooks. To finish each workbook, you must manually:

  # 1. Hide row 3 (model API variable names)
  # 2. Developer tab > Visual Basic. Under Microsoft Excel Objects in the
  #    explorer panel, copy the code from Sheet1 to Sheet2 (Cards).
  # 3. Save, then close and re-open the workbook. Test the API by changing a
  #    characteristic.
}
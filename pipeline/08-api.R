#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Setup ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# NOTE: See DESCRIPTION for library dependencies and R/setup.R for
# variables used in each pipeline stage

# Load libraries, helpers, and recipes from files
purrr::walk(list.files("R/", "\\.R$", full.names = TRUE), source)

# Load additional dev R libraries (see README#managing-r-dependencies)
suppressPackageStartupMessages({
  library(openxlsx)
})

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
  pull(township_code)

# Load categorical variable dictionary for lookup and data validation
dict <- ccao::vars_dict %>%
  filter(
    var_data_type == "categorical",
    var_name_model %in% predictors
  ) %>%
  distinct(var_name_pretty, var_code, var_value)

# Compute dictionary row ranges keyed by model variable name. Row numbers are
# offset by 1 to account for the header row in the Dictionary sheet.
dict_ranges <- dict %>%
  mutate(.row = row_number() + 1L) %>%
  left_join(
    ccao::vars_dict %>%
      filter(var_data_type == "categorical", var_name_model %in% predictors) %>%
      distinct(var_name_model, var_name_pretty, var_code, var_value),
    by = c("var_name_pretty", "var_code", "var_value")
  ) %>%
  group_by(var_name_model) %>%
  summarize(rows = list(range(.row)), .groups = "drop") %>%
  {
    setNames(.$rows, .$var_name_model)
  }


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

# Schema for all columns in the Cards sheet. display_name must match the
# corresponding header in row 4 of model_api_template.xlsm exactly (whitespace
# is normalized). Adding or removing a column requires updating both this list
# and the template.
api_cards_schema <- list(
  meta_pin = list(display_name = "PIN"),
  meta_card_num = list(display_name = "Card"),
  meta_class = list(display_name = "Class"),
  pred_card_initial_fmv = list(
    display_name = "Original Prediction",
    style = "price"
  ),
  api_prediction = list(display_name = "API Prediction", style = "price"),
  api_prediction_rounded = list(
    display_name = "Rounded API Prediction",
    style = "price"
  ),
  meta_township_code = list(display_name = "Town"),
  meta_nbhd_code = list(display_name = "Nbhd"),
  char_bldg_sf = list(display_name = "Bldg. Sqft."),
  char_fbath = list(display_name = "F. Baths"),
  char_yrblt = list(display_name = "Year Built"),
  char_land_sf = list(display_name = "Land Sqft."),
  char_frpl = list(display_name = "Fireplaces"),
  loc_school_elementary_district_geoid = list(
    display_name = "Elementary District GEOID"
  ),
  loc_school_secondary_district_geoid = list(
    display_name = "Secondary District GEOID"
  ),
  acs5_median_income_per_capita_past_year = list(
    display_name = "Median Income, Per Capita in Past Year"
  ),
  char_class = list(display_name = "Class"),
  char_air = list(display_name = "Central Air", categorical = TRUE),
  char_apts = list(display_name = "Num. Apts", categorical = TRUE),
  char_attic_fnsh = list(display_name = "Attic Finish", categorical = TRUE),
  char_attic_type = list(display_name = "Attic Type", categorical = TRUE),
  char_beds = list(display_name = "Beds"),
  char_bsmt = list(display_name = "Bsmt. Type", categorical = TRUE),
  char_bsmt_fin = list(display_name = "Bsmt. Finish", categorical = TRUE),
  char_ext_wall = list(display_name = "Ext. Wall", categorical = TRUE),
  char_gar1_att = list(display_name = "Gar. 1 Attached", categorical = TRUE),
  char_gar1_cnst = list(
    display_name = "Gar. 1 Ext. Wall Material",
    categorical = TRUE
  ),
  char_gar1_size = list(display_name = "Gar. 1 Size", categorical = TRUE),
  char_hbath = list(display_name = "H. Baths"),
  char_heat = list(display_name = "Central Heat", categorical = TRUE),
  char_ncu = list(display_name = "NCU"),
  char_porch = list(display_name = "Porch", categorical = TRUE),
  char_roof_cnst = list(display_name = "Roof Material", categorical = TRUE),
  char_rooms = list(display_name = "Rooms"),
  char_tp_dsgn = list(display_name = "Design Plan", categorical = TRUE),
  char_type_resd = list(display_name = "Type of Resd.", categorical = TRUE),
  char_recent_renovation = list(display_name = "Recent Reno."),
  loc_longitude = list(display_name = "Longitude"),
  loc_latitude = list(display_name = "Latitude"),
  loc_census_tract_geoid = list(display_name = "Census Tract"),
  loc_env_flood_fs_factor = list(display_name = "First Street Flood Factor"),
  loc_access_cmap_walk_nta_score = list(
    display_name = "CMAP Score (No Transit)"
  ),
  loc_access_cmap_walk_total_score = list(display_name = "CMAP Total Score"),
  loc_tax_municipality_name = list(display_name = "Municipality"),
  time_sale_year = list(display_name = "Year of Sale"),
  time_sale_day = list(display_name = "Day of Sale"),
  time_sale_quarter_of_year = list(display_name = "Sale Quarter of Year"),
  time_sale_month_of_year = list(display_name = "Sale Month of Year"),
  time_sale_day_of_year = list(display_name = "Sale Day of Year"),
  time_sale_day_of_month = list(display_name = "Sale Day of Month"),
  time_sale_day_of_week = list(display_name = "Sale Day of Week"),
  time_sale_post_covid = list(display_name = "Sale After COVID"),
  prox_num_pin_in_half_mile = list(
    display_name = "Num. of PINs in Half Mile"
  ),
  prox_num_bus_stop_in_half_mile = list(
    display_name = "Num. of Bus Stops in Half Mile"
  ),
  prox_num_foreclosure_per_1000_pin_past_5_years = list(
    display_name = "Num. of Foreclosures Per 1000 PINs (Past 5 Years)"
  ),
  prox_avg_school_rating_in_half_mile = list(
    display_name = "Avg. School Rating in Half Mile"
  ),
  prox_airport_dnl_total = list(display_name = "Airport DNL Total"),
  prox_nearest_bike_trail_dist_ft = list(
    display_name = "Nearest Bike Trail Dist. (Feet)"
  ),
  prox_nearest_cemetery_dist_ft = list(
    display_name = "Nearest Cemetery Dist. (Feet)"
  ),
  prox_nearest_cta_route_dist_ft = list(
    display_name = "Nearest CTA Route Dist. (Feet)"
  ),
  prox_nearest_cta_stop_dist_ft = list(
    display_name = "Nearest CTA Stop Dist. (Feet)"
  ),
  prox_nearest_hospital_dist_ft = list(
    display_name = "Nearest Hospital Dist. (Feet)"
  ),
  prox_lake_michigan_dist_ft = list(
    display_name = "Lake Michigan Dist. (Feet)"
  ),
  prox_nearest_metra_route_dist_ft = list(
    display_name = "Nearest Metra Route Dist. (Feet)"
  ),
  prox_nearest_metra_stop_dist_ft = list(
    display_name = "Nearest Metra Stop Dist. (Feet)"
  ),
  prox_nearest_park_dist_ft = list(display_name = "Nearest Park Dist. (Feet)"),
  prox_nearest_railroad_dist_ft = list(
    display_name = "Nearest Railroad Dist. (Feet)"
  ),
  prox_nearest_university_dist_ft = list(
    display_name = "Nearest University Dist. (Feet)"
  ),
  prox_nearest_vacant_land_dist_ft = list(
    display_name = "Nearest Vacant Land Dist. (Feet)"
  ),
  prox_nearest_water_dist_ft = list(
    display_name = "Nearest Water Dist. (Feet)"
  ),
  prox_nearest_golf_course_dist_ft = list(
    display_name = "Nearest Golf Course Dist. (Feet)"
  ),
  prox_nearest_road_highway_dist_ft = list(
    display_name = "Nearest Highway Dist. (Feet)"
  ),
  prox_nearest_road_arterial_dist_ft = list(
    display_name = "Nearest Arterial Road Dist. (Feet)"
  ),
  prox_nearest_road_collector_dist_ft = list(
    display_name = "Nearest Collector Road Dist. (Feet)"
  ),
  prox_nearest_road_arterial_daily_traffic = list(
    display_name = "Traffic of Nearest Arterial Road"
  ),
  prox_nearest_road_collector_daily_traffic = list(
    display_name = "Traffic of Nearest Collector Road"
  ),
  prox_nearest_new_construction_dist_ft = list(
    display_name = "Nearest New Construction Dist. (Feet)"
  ),
  prox_nearest_stadium_dist_ft = list(
    display_name = "Nearest Stadium Dist. (Feet)"
  ),
  acs5_percent_age_children = list(
    display_name = "Percent Population Age, Under 19 Years Old"
  ),
  acs5_percent_age_senior = list(
    display_name = "Percent Population Age, Over 65 Years Old"
  ),
  acs5_median_age_total = list(display_name = "Median Population Age"),
  acs5_percent_household_family_married = list(
    display_name = "Percent Households Family, Married"
  ),
  acs5_percent_household_nonfamily_alone = list(
    display_name = "Percent Households Nonfamily, Living Alone"
  ),
  acs5_percent_education_high_school = list(
    display_name = "Percent Population Education, High School Degree"
  ),
  acs5_percent_education_bachelor = list(
    display_name = "Percent Population Education, Bachelor Degree"
  ),
  acs5_percent_education_graduate = list(
    display_name = "Percent Population Education, Graduate Degree"
  ),
  acs5_percent_income_below_poverty_level = list(
    display_name = "Percent Population Income, Below Poverty Level"
  ),
  acs5_median_income_household_past_year = list(
    display_name = "Median Income, Household in Past Year"
  ),
  acs5_percent_income_household_received_snap_past_year = list(
    display_name = paste0(
      "Percent Population Income, Received SNAP",
      " in Past Year"
    )
  ),
  acs5_percent_employment_unemployed = list(
    display_name = "Percent Population Employment, Unemployed"
  ),
  acs5_median_household_total_occupied_year_built = list(
    display_name = "Median Occupied Household, Total, Year Built"
  ),
  acs5_median_household_renter_occupied_gross_rent = list(
    display_name = "Median Occupied Household, Renter, Gross Rent"
  ),
  acs5_percent_household_owner_occupied = list(
    display_name = "Percent Occupied Households, Owner"
  ),
  other_tax_bill_rate = list(display_name = "Tax Bill Aggregate Rate"),
  meta_sale_count_past_n_years = list(
    display_name = "Num. Sales In The Past 5 Years"
  ),
  shp_parcel_centroid_dist_ft_sd = list(
    display_name = "Standard Deviation from Centroid to Vertices (Feet)"
  ),
  shp_parcel_edge_len_ft_sd = list(
    display_name = "Standard Deviation Parcel Edge Length (Feet)"
  ),
  shp_parcel_interior_angle_sd = list(
    display_name = "Standard Deviation Parcel Interior Angle (Degrees)"
  ),
  shp_parcel_mrr_area_ratio = list(
    display_name =
      "Ratio of Parcel Area to Minimum Rotated Bounding Rectangle"
  ),
  shp_parcel_mrr_side_ratio = list(
    display_name = paste0(
      "Ratio of Parcel Minimum Rotated Bounding Rectangle ",
      "Longest to Shortest Side"
    )
  ),
  shp_parcel_num_vertices = list(display_name = "Number of Parcel Vertices")
)


validate_schema_vs_template(
  api_cards_schema,
  here("misc", "model_api_template.xlsm"),
  sheet_name = "Cards",
  header_row = 4
)

wb_styles <- list(
  price = createStyle(numFmt = "$#,##0")
)

# Write raw data to sheets for parcel details
for (town in towns) {
  message("Now processing: ", town_convert(town))

  # Load data from file, then make it pretty for saving to sheet
  card_data_town <- card_data %>%
    filter(meta_township_code == town) %>%
    mutate(api_prediction = NA, api_prediction_rounded = NA) %>%
    arrange(meta_pin, meta_card_num) %>%
    mutate(
      across(where(is.numeric), ~ round(.x, 8)),
      meta_pin = ccao::pin_format_pretty(meta_pin, full_length = TRUE)
    ) %>%
    var_encode(
      # The column selection here is a little hacky, but gets around the fact
      # that the `cols` attribute can't handle a select clause
      # like `(starts_with(x) & !y)`
      cols = card_data %>%
        select(starts_with("char_") & !char_apts) %>%
        names()
    ) %>%
    # Align column order with the schema
    select(all_of(names(api_cards_schema)))

  # Load workbook and styles
  wb <- loadWorkbook(here("misc", "model_api_template.xlsm"))
  pin_sheet_header <- run_id
  pin_row_range <- 6:(nrow(card_data_town) + 7)
  csht <- "Cards"
  dsht <- "Dictionary"

  # Write dictionary and data validation
  writeData(wb, dsht, dict, startCol = 1, startRow = 2, colNames = FALSE)
  iwalk(
    Filter(function(x) isTRUE(x$categorical), api_cards_schema),
    function(col_spec, col_name) {
      dict_rows <- dict_ranges[[col_name]]
      dataValidation(
        wb, csht,
        col = col_pos(api_cards_schema, col_name), rows = pin_row_range,
        type = "list",
        value = glue("'{dsht}'!$B${dict_rows[1]}:$B${dict_rows[2]}")
      )
    }
  )

  # Apply cell styles driven by the schema — one addStyle call per style group
  for (style_name in names(wb_styles)) {
    style_cols <- cols_with_style(api_cards_schema, style_name)
    if (length(style_cols) == 0) next
    addStyle(wb, csht,
      style = wb_styles[[style_name]],
      rows = pin_row_range, cols = style_cols, gridExpand = TRUE
    )
  }
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
        town_get_triad(town, name = TRUE),
        str_replace(town_convert(town), " ", "-"),
        pin_sheet_header,
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

  # 1. Hide row 4 (model API variable names)
  # 2. Save, then close and re-open the workbook. Test the API by changing a
  #    characteristic.
}

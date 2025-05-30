#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Setup ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# NOTE: See DESCRIPTION for library dependencies and R/setup.R for
# variables used in each pipeline stage

# Start the stage timer
tictoc::tic.clearlog()
tictoc::tic("Ingest")

# Load libraries, helpers, and recipes from files
purrr::walk(list.files("R/", "\\.R$", full.names = TRUE), source)

# Load additional dev R libraries (see README#managing-r-dependencies)
suppressPackageStartupMessages({
  library(DBI)
  library(igraph)
  library(noctua)
})

# Adds arrow support to speed up ingest process.
noctua_options(unload = TRUE)

# Establish Athena connection
AWS_ATHENA_CONN_NOCTUA <- dbConnect(
  noctua::athena(),
  s3_staging_dir   = "s3://ccao-athena-results-us-east-1/",
  region_name      = "us-east-1",
  rstudio_conn_tab = FALSE
)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. Define Functions ----------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Ingest-specific helper functions for data cleaning, etc.

# Create a dictionary of column types, as specified in ccao::vars_dict
col_type_dict <- ccao::vars_dict %>%
  distinct(var_name = var_name_model, var_type = var_data_type) %>%
  drop_na(var_name)

# Mini-function to ensure that columns are the correct type
recode_column_type <- function(col, col_name, dictionary = col_type_dict) {
  col_type <- dictionary %>%
    filter(var_name == col_name) %>%
    pull(var_type)
  switch(col_type,
    numeric = as.numeric(col),
    character = as.character(col),
    logical = as.logical(as.numeric(col)),
    categorical = as.factor(col),
    date = lubridate::as_date(col)
  )
}

# Mini function to deal with arrays
# Some Athena columns are stored as arrays but are converted to string on
# ingest. In such cases, we either keep the contents of the cell (if 1 unit),
# collapse the array into a comma-separated string (if more than 1 unit),
# or replace with NA if the array is empty
process_array_column <- function(x) {
  purrr::map_chr(x, function(cell) {
    if (length(cell) > 1) {
      paste(cell, collapse = ", ")
    } else if (length(cell) == 1) {
      as.character(cell) # Convert the single element to character
    } else {
      NA # Handle cases where the array is empty
    }
  })
}




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Pull Data -----------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Pulling data from Athena")

# Pull the training data, which contains actual sales + attached characteristics
# from the residential input view. Earlier years are included to help calculate
# lagged features
tictoc::tic("Training data pulled")
training_data <- dbGetQuery(
  conn = AWS_ATHENA_CONN_NOCTUA, glue("
  SELECT
      sale.sale_price AS meta_sale_price,
      sale.sale_date AS meta_sale_date,
      sale.doc_no AS meta_sale_document_num,
      sale.deed_type AS meta_sale_deed_type,
      sale.seller_name AS meta_sale_seller_name,
      sale.buyer_name AS meta_sale_buyer_name,
      sale.sv_is_outlier,
      sale.sv_outlier_reason1,
      sale.sv_outlier_reason2,
      sale.sv_outlier_reason3,
      sale.sv_run_id,
      res.*
  FROM model.vw_card_res_input res
  INNER JOIN default.vw_pin_sale sale
      ON sale.pin = res.meta_pin
      AND sale.year = res.year
  WHERE CAST(res.year AS int)
      BETWEEN CAST({params$input$min_sale_year} AS int) -
        {params$input$n_years_prior}
      AND CAST({params$input$max_sale_year} AS int)
  AND sale.deed_type IN ('01', '02', '05')
  AND NOT sale.is_multisale
  AND NOT sale.sale_filter_same_sale_within_365
  AND NOT sale.sale_filter_less_than_10k
  AND NOT sale.sale_filter_deed_type
  ")
)
tictoc::toc()

# Pull all ADDCHARS/HIE data. These are Home Improvement Exemptions (HIEs)
# stored in the legacy (AS/400) data system
tictoc::tic("HIE data pulled")
hie_data <- dbGetQuery(
  conn = AWS_ATHENA_CONN_NOCTUA, glue("
  SELECT *
  FROM ccao.hie
  ")
)
tictoc::toc()

# Save HIE data for use in report generation
hie_data %>%
  write_parquet(paths$input$hie$local)

# Pull all residential PIN input data for the assessment and prior year. We will
# only use the assessment year to run the model, but the prior year can be used
# for report generation
tictoc::tic("Assessment data pulled")
assessment_data <- dbGetQuery(
  conn = AWS_ATHENA_CONN_NOCTUA, glue("
  SELECT *
  FROM model.vw_card_res_input
  WHERE year BETWEEN '{as.numeric(params$assessment$data_year) - 1}'
    AND '{params$assessment$data_year}'
  ")
)
tictoc::toc()

# Save both years for report generation using the characteristics
assessment_data %>%
  write_parquet(paths$input$char$local)

# Save only the assessment year data to use for assessing values
assessment_data <- assessment_data %>%
  filter(year == params$assessment$data_year)

# Pull neighborhood-level land rates ($/sqft), as calculated by Valuations
tictoc::tic("Land rate data pulled")
land_nbhd_rate_data <- dbGetQuery(
  conn = AWS_ATHENA_CONN_NOCTUA, glue("
  SELECT *
  FROM ccao.land_nbhd_rate
  WHERE year = '{params$assessment$year}'
  ")
)
tictoc::toc()

# Close connection to Athena
dbDisconnect(AWS_ATHENA_CONN_NOCTUA)
rm(AWS_ATHENA_CONN_NOCTUA)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4. Home Improvement Exemptions -----------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Adding HIE data to training and assessment sets")

# HIEs need to be combined with the training data such that the training data
# uses the characteristics at the time of sale, rather than the un-updated
# characteristics used for assessment. See GitHub wiki for more information:
# https://github.com/ccao-data/wiki/blob/master/Residential/Home-Improvement-Exemptions.md # nolint

## 4.1. Training Data ----------------------------------------------------------

# Convert legacy data to sparse representation with 1 active row per HIE year.
# NOTE: ONLY join to non-multicard PINs, since HIEs cannot be matched when there
# are multiple cards
hie_data_training_sparse <- hie_data %>%
  ccao::chars_sparsify(
    pin_col = pin,
    year_col = year,
    town_col = qu_town,
    upload_date_col = qu_upload_date,
    additive_source = any_of(chars_cols$add$source),
    replacement_source = any_of(chars_cols$replace$source)
  ) %>%
  mutate(
    ind_pin_is_multicard = FALSE,
    year = as.character(year)
  )

# Merge the HIE data with the training data, updating/adding to training data
# characteristics
training_data_w_hie <- training_data %>%
  mutate(across(
    all_of(ccao::chars_cols$add$target),
    ~ recode_column_type(.x, cur_column())
  )) %>%
  left_join(
    hie_data_training_sparse,
    by = c("meta_pin" = "pin", "year", "ind_pin_is_multicard")
  ) %>%
  mutate(qu_class = ifelse(qu_class != "288", qu_class, meta_class)) %>%
  ccao::chars_update(
    additive_target = any_of(chars_cols$add$target),
    replacement_target = any_of(chars_cols$replace$target)
  ) %>%
  select(-starts_with("qu_")) %>%
  mutate(
    hie_num_active = replace_na(hie_num_active, 0),
    char_porch = recode(char_porch, "3" = "0")
  ) %>%
  relocate(hie_num_active, .before = meta_cdu)


## 4.2. Assessment Data --------------------------------------------------------

# For assessment data, we want to include ONLY the HIEs that expire in the
# assessment year
hie_data_assessment_sparse <- hie_data %>%
  filter(hie_last_year_active == as.numeric(params$assessment$year) - 1) %>%
  ccao::chars_sparsify(
    pin_col = pin,
    year_col = year,
    town_col = qu_town,
    upload_date_col = qu_upload_date,
    additive_source = any_of(chars_cols$add$source),
    replacement_source = any_of(chars_cols$replace$source)
  ) %>%
  mutate(
    ind_pin_is_multicard = FALSE,
    year = as.character(year)
  )

# Update assessment data with any expiring HIEs. Add a field for the number
# of HIEs expired for each PIN
assessment_data_w_hie <- assessment_data %>%
  mutate(across(
    all_of(ccao::chars_cols$add$target),
    ~ recode_column_type(.x, cur_column())
  )) %>%
  left_join(
    hie_data_assessment_sparse,
    by = c("meta_pin" = "pin", "year", "ind_pin_is_multicard")
  ) %>%
  mutate(qu_class = ifelse(qu_class != "288", qu_class, meta_class)) %>%
  ccao::chars_update(
    additive_target = any_of(chars_cols$add$target),
    replacement_target = any_of(chars_cols$replace$target)
  ) %>%
  select(-starts_with("qu_")) %>%
  mutate(
    hie_num_active = replace_na(hie_num_active, 0),
    char_porch = recode(char_porch, "3" = "0")
  ) %>%
  rename(hie_num_expired = hie_num_active) %>%
  relocate(hie_num_expired, .before = meta_cdu)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 5. Add Features and Clean ----------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Adding time features and cleaning")

## 5.1. Training Data ----------------------------------------------------------

# Clean up the training data. Goal is to get it into a publishable format.
# Final featurization, missingness, etc. is handled via Tidymodels recipes
training_data_clean <- training_data_w_hie %>%
  # Recode factor variables using the definitions stored in ccao::vars_dict
  # This will remove any categories not stored in the dictionary and convert
  # them to NA (useful since there are a lot of misrecorded variables)
  ccao::vars_recode(cols = starts_with("char_"), code_type = "code") %>%
  # Recode the number of apartments from its numeric code to its actual number
  # of units. Additionally, ensure non-multi-family PINs always have NONE apts
  ccao::vars_recode(
    cols = all_of("char_apts"),
    code_type = "short",
    as_factor = FALSE
  ) %>%
  mutate(
    char_apts = case_when(
      char_class %in% c("211", "212") & !is.na(char_apts) ~ char_apts,
      char_class %in% c("211", "212") & is.na(char_apts) ~ "UNKNOWN",
      TRUE ~ "NONE"
    ),
    char_apts = factor(
      char_apts,
      levels = c("TWO", "THREE", "FOUR", "FIVE", "SIX", "UNKNOWN", "NONE")
    ),
    char_ncu = ifelse(char_class == "212" & !is.na(char_ncu), char_ncu, 0)
  ) %>%
  mutate(
    across(starts_with("loc_tax_"), process_array_column),
    loc_tax_municipality_name =
      replace_na(loc_tax_municipality_name, "UNINCORPORATED")
  ) %>%
  # Coerce columns to the data types recorded in the dictionary. Necessary
  # because the SQL drivers will often coerce types on pull (boolean becomes
  # character)
  mutate(across(
    any_of(col_type_dict$var_name),
    ~ recode_column_type(.x, cur_column())
  )) %>%
  # Only exclude explicit outliers from training. Sales with missing validation
  # outcomes will be considered non-outliers
  mutate(
    sv_is_outlier = replace_na(sv_is_outlier, FALSE)
  ) %>%
  mutate(
    # Miscellaneous column-level cleanup
    ccao_is_corner_lot = replace_na(ccao_is_corner_lot, FALSE),
    across(where(is.character), \(x) na_if(x, "")),
    across(where(bit64::is.integer64), \(x) as.numeric(x))
  ) %>%
  # Get a count of the number of sales that have occurred in the last n years
  left_join(
    left_join(
      training_data %>%
        select(meta_pin, meta_sale_document_num, meta_sale_date),
      training_data %>%
        filter(!sv_is_outlier) %>%
        select(meta_pin, meta_sale_date),
      by = "meta_pin",
      relationship = "many-to-many"
    ) %>%
      # as.duration(1) excludes the same sale from being identified as within
      # 3 years of itself
      mutate(within_n_years = between(
        meta_sale_date.x - meta_sale_date.y,
        as.duration(1),
        as.duration(years(params$input$n_years_prior))
      )) %>%
      # Distinct is necessary because of multicard sales
      distinct() %>%
      summarise(
        meta_sale_count_past_n_years = as.numeric(
          sum(within_n_years, na.rm = TRUE)
        ),
        .by = c("meta_pin", "meta_sale_document_num")
      ),
    by = c("meta_pin", "meta_sale_document_num")
  ) %>%
  # Create time/date features using lubridate
  mutate(
    # Calculate interval periods and time since the start of the sales sample
    time_interval = interval(
      make_date(params$input$min_sale_year, 1, 1),
      ymd(meta_sale_date)
    ),
    time_sale_year = as.numeric(year(meta_sale_date)),
    time_sale_day = as.numeric(time_interval %/% days(1)) + 1,
    # Get components of dates to correct for seasonality and other factors
    time_sale_quarter_of_year = paste0("Q", quarter(meta_sale_date)),
    time_sale_month_of_year = as.integer(month(meta_sale_date)),
    time_sale_day_of_year = as.integer(yday(meta_sale_date)),
    time_sale_day_of_month = as.integer(day(meta_sale_date)),
    time_sale_day_of_week = as.integer(wday(meta_sale_date)),
    time_sale_post_covid = meta_sale_date >= make_date(2020, 3, 15)
  ) %>%
  # Reorder resulting columns
  select(-any_of(c("time_interval"))) %>%
  relocate(starts_with("sv_"), .after = everything()) %>%
  relocate("year", .after = everything()) %>%
  relocate("meta_sale_count_past_n_years", .after = meta_sale_buyer_name) %>%
  # Drop invalid sales outside the sample date range or with obvious incorrect
  # characteristic values
  filter(
    between(
      meta_sale_date,
      make_date(params$input$min_sale_year, 1, 1),
      make_date(params$input$max_sale_year, 12, 31)
    ),
    between(char_bldg_sf, 0, 60000),
    between(char_beds, 0, 40),
    between(char_rooms, 0, 50),
    between(char_fbath, 0, 15),
    between(char_hbath, 0, 10),
  ) %>%
  mutate(
    # Recode unexpectedly low values to NA
    char_bldg_sf = replace(char_bldg_sf, which(char_bldg_sf < 300), NA),
    char_land_sf = replace(char_land_sf, which(char_land_sf < 300), NA),
    across(
      c(char_beds, char_rooms, char_fbath),
      \(x) replace(x, which(x == 0), NA)
    )
  ) %>%
  as_tibble() %>%
  write_parquet(paths$input$training$local)


## 5.2. Assessment Data --------------------------------------------------------

# Clean the assessment data. This is the target data that the trained model is
# used on. The cleaning steps are the same as above, with the exception of the
# time variables and identifying complexes
assessment_data_clean <- assessment_data_w_hie %>%
  ccao::vars_recode(cols = starts_with("char_"), code_type = "code") %>%
  ccao::vars_recode(
    cols = all_of("char_apts"),
    code_type = "short",
    as_factor = FALSE
  ) %>%
  # Apply the helper function to process array columns
  mutate(
    across(starts_with("loc_tax_"), process_array_column),
    loc_tax_municipality_name =
      replace_na(loc_tax_municipality_name, "UNINCORPORATED")
  ) %>%
  mutate(
    char_apts = case_when(
      char_class %in% c("211", "212") & !is.na(char_apts) ~ char_apts,
      char_class %in% c("211", "212") & is.na(char_apts) ~ "UNKNOWN",
      TRUE ~ "NONE"
    ),
    char_apts = factor(
      char_apts,
      levels = c("TWO", "THREE", "FOUR", "FIVE", "SIX", "UNKNOWN", "NONE")
    ),
    char_ncu = ifelse(char_class == "212" & !is.na(char_ncu), char_ncu, 0)
  ) %>%
  mutate(across(
    any_of(col_type_dict$var_name),
    ~ recode_column_type(.x, cur_column())
  )) %>%
  # Same feature cleanup as the training data
  mutate(
    ccao_is_corner_lot = replace_na(ccao_is_corner_lot, FALSE),
    across(where(is.character), \(x) na_if(x, "")),
    across(where(bit64::is.integer64), \(x) as.numeric(x))
  ) %>%
  # Get a count of the number of sales that have occurred in the last n years
  left_join(
    left_join(
      assessment_data %>% select(meta_pin),
      training_data %>%
        filter(!sv_is_outlier) %>%
        select(meta_pin, meta_sale_date),
      by = "meta_pin",
      relationship = "many-to-many"
    ) %>%
      # as.duration(1) excludes the same sale from being identified as within
      # 3 years of itself
      mutate(within_n_years = between(
        # Here we're looking back from the lien date instead of the sale date,
        # as in the training data
        ymd(params$assessment$date) - as_date(meta_sale_date),
        as.duration(1),
        as.duration(years(params$input$n_years_prior))
      )) %>%
      # Distinct is necessary because of multicard sales
      distinct() %>%
      summarise(
        # Subtract 1 from the count of prior sales. The reasoning here is
        # difficult, but basically: in the training data, this feature only has
        # a count > 0 conditional on multiple sales. In the assessment data, if
        # we count the lien date as a sale, the "multiple sales" conditional
        # isn't technically true. Therefore, we subtract 1 from the count to
        # make the feature consistent between the training and assessment data
        meta_sale_count_past_n_years = as.numeric(
          pmax(sum(within_n_years, na.rm = TRUE) - 1, 0)
        ),
        .by = "meta_pin"
      ),
    by = "meta_pin",
    relationship = "many-to-many"
  ) %>%
  # Create sale date features BASED ON THE ASSESSMENT DATE. The model predicts
  # the sale price of properties on the date of assessment. Not the date of an
  # actual sale
  dplyr::mutate(
    meta_sale_date = as_date(params$assessment$date),
    time_interval = interval(
      make_date(params$input$min_sale_year, 1, 1),
      ymd(meta_sale_date)
    ),
    time_sale_year = as.numeric(year(meta_sale_date)),
    time_sale_day = as.numeric(time_interval %/% days(1)) + 1,
    time_sale_quarter_of_year = paste0("Q", quarter(meta_sale_date)),
    time_sale_month_of_year = as.integer(month(meta_sale_date)),
    time_sale_day_of_year = as.integer(yday(meta_sale_date)),
    time_sale_day_of_month = as.integer(day(meta_sale_date)),
    time_sale_day_of_week = as.integer(wday(meta_sale_date)),
    time_sale_post_covid = meta_sale_date >= make_date(2020, 3, 15)
  ) %>%
  select(-any_of(c("time_interval"))) %>%
  relocate(starts_with("sv_"), .after = everything()) %>%
  relocate("year", .after = everything()) %>%
  relocate(starts_with("meta_sale_"), .after = hie_num_expired) %>%
  as_tibble() %>%
  write_parquet(paths$input$assessment$local)


## 5.3. Complex IDs ------------------------------------------------------------
message("Creating townhome complex identifiers")

# Townhomes and rowhomes within the same "complex" or building should
# ultimately receive the same final assessed value. However, a single row of
# identical townhomes can have multiple PINs and the CCAO does not maintain a
# unique complex ID. Further, PINs within a complex often have nearly, but not
# exactly, identical characteristics.

# To solve this issue and assign each complex an ID, we do some clever "fuzzy"
# joining and then link each PIN into an undirected graph. See this SO post
# for more details on the methodology:
# https://stackoverflow.com/questions/68353869/create-group-based-on-fuzzy-criteria # nolint
complex_id_temp <- assessment_data_clean %>%
  filter(meta_class %in% c("210", "295")) %>%
  # Self-join with attributes that must be exactly matching
  select(
    meta_pin, meta_card_num, meta_township_code, meta_class,
    all_of(params$input$complex$match_exact),
    any_of(paste0("char_", names(params$input$complex$match_fuzzy))),
    loc_x_3435, loc_y_3435
  ) %>%
  full_join(
    eval(.),
    by = params$input$complex$match_exact,
    multiple = "all",
    relationship = "many-to-many"
  ) %>%
  # Filter with attributes that can be "fuzzy" matched
  filter(
    char_rooms.x >= char_rooms.y - params$input$complex$match_fuzzy$rooms,
    char_rooms.x <= char_rooms.y + params$input$complex$match_fuzzy$rooms,
    char_bldg_sf.x >= char_bldg_sf.y - params$input$complex$match_fuzzy$bldg_sf,
    char_bldg_sf.x <= char_bldg_sf.y + params$input$complex$match_fuzzy$bldg_sf,
    # nolint start
    (char_yrblt.x >= char_yrblt.y - params$input$complex$match_fuzzy$yrblt &
      char_yrblt.x <= char_yrblt.y + params$input$complex$match_fuzzy$yrblt) |
      is.na(char_yrblt.x),
    # Units must be within 250 feet of other units
    (loc_x_3435.x >= loc_x_3435.y - params$input$complex$match_fuzzy$dist_ft &
      loc_x_3435.x <= loc_x_3435.y + params$input$complex$match_fuzzy$dist_ft) |
      is.na(loc_x_3435.x),
    (loc_y_3435.x >= loc_y_3435.y - params$input$complex$match_fuzzy$dist_ft &
      loc_y_3435.x <= loc_y_3435.y + params$input$complex$match_fuzzy$dist_ft) |
      is.na(loc_y_3435.x)
    # nolint end
  ) %>%
  # Combine PINs into a graph
  select(meta_pin.x, meta_pin.y) %>%
  igraph::graph_from_data_frame(directed = FALSE) %>%
  igraph::components() %>%
  igraph::membership() %>%
  # Convert graph to tibble and clean up
  utils::stack() %>%
  as_tibble() %>%
  mutate(ind = as.character(ind)) %>%
  rename(meta_pin = ind, meta_complex_id = values)

# Attach original PIN data and fill any missing complexes with
# a sequential integer
complex_id_data <- assessment_data_clean %>%
  filter(meta_class %in% c("210", "295")) %>%
  distinct(meta_pin, meta_township_code, meta_class) %>%
  left_join(complex_id_temp, by = "meta_pin") %>%
  arrange(meta_complex_id) %>%
  mutate(meta_complex_id = ifelse(
    !is.na(meta_complex_id),
    meta_complex_id,
    lag(meta_complex_id) + 1
  )) %>%
  # Break long "chains" of fuzzy matched properties into separate groups if the
  # chain spans more than the allowed square foot difference
  left_join(
    assessment_data_clean %>%
      filter(meta_class %in% c("210", "295")) %>%
      group_by(meta_pin) %>%
      summarize(tot_sqft = sum(char_bldg_sf)),
    by = "meta_pin"
  ) %>%
  group_by(meta_complex_id) %>%
  mutate(
    char_break = floor(tot_sqft / params$input$complex$match_fuzzy$bldg_sf),
    char_break = char_break - min(char_break),
    char_break = floor(char_break / 2)
  ) %>%
  group_by(meta_complex_id, char_break) %>%
  mutate(meta_complex_id = cur_group_id()) %>%
  ungroup() %>%
  select(-c(tot_sqft, char_break)) %>%
  mutate(meta_complex_id = as.numeric(meta_complex_id)) %>%
  write_parquet(paths$input$complex_id$local)


## 5.4. Land Rates -------------------------------------------------------------
message("Saving land rates")

# Write land data directly to file, since it's already mostly clean
land_nbhd_rate_data %>%
  select(meta_nbhd = town_nbhd, meta_class = class, land_rate_per_sqft) %>%
  write_parquet(paths$input$land_nbhd_rate$local)

# Reminder to upload to DVC store
message(
  "Be sure to add updated input data to DVC and finalized data to S3\n",
  "See https://dvc.org/doc/start/data-management/data-versioning ",
  "for more information"
)

# End the stage timer
tictoc::toc(log = FALSE)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Setup ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Pre-allocate memory for java JDBC driver
options(java.parameters = "-Xmx10g")

# Load R libraries
library(arrow)
library(ccao)
library(DBI)
library(data.table)
library(dplyr)
library(glue)
library(here)
library(igraph)
library(lubridate)
library(purrr)
library(RJDBC)
library(s2)
library(sf)
library(tictoc)
library(tidyr)
library(yaml)
source(here("R", "helpers.R"))

# Initialize a dictionary of file paths. See misc/file_dict.csv for details
paths <- model_file_dict()

# Load the parameters file containing the run settings
params <- read_yaml("params.yaml")

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
# 2. Pull Data -----------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Pull the training data, which contains actual sales + attached characteristics
# from the residential input view
tictoc::tic()
training_data <- dbGetQuery(
  conn = AWS_ATHENA_CONN_JDBC, glue("
  SELECT
      sale.sale_price AS meta_sale_price,
      sale.sale_date AS meta_sale_date,
      sale.doc_no AS meta_sale_document_num,
      res.*
  FROM model.vw_card_res_input res
  INNER JOIN default.vw_pin_sale sale
      ON sale.pin = res.meta_pin
      AND sale.year = res.meta_year
  WHERE (res.meta_year
      BETWEEN '{params$input$min_sale_year}'
      AND '{params$input$max_sale_year}')
  AND ((sale.sale_price_log10
      BETWEEN sale.sale_filter_lower_limit
      AND sale.sale_filter_upper_limit)
      AND sale.sale_filter_count >= 10)
  AND NOT is_multisale
  ")
)
tictoc::toc()

# Pull all ADDCHARS/HIE data. These are Home Improvement Exemptions (HIEs)
# stored in the legacy (AS/400) data system
tictoc::tic()
hie_data <- dbGetQuery(
  conn = AWS_ATHENA_CONN_JDBC, glue("
  SELECT *
  FROM ccao.hie
  ")
)
tictoc::toc()

# Pull all residential PIN input data for the assessment year. This will be the
# data we actually run the model on
tictoc::tic()
assessment_data <- dbGetQuery(
  conn = AWS_ATHENA_CONN_JDBC, glue("
  SELECT *
  FROM model.vw_card_res_input
  WHERE meta_year = '{params$assessment$data_year}'
  ")
)
tictoc::toc()

# Pull site-specific (pre-determined) land values and neighborhood-level land
# rates per sqft, as calculated by Valuations
tictoc::tic()
land_site_rate_data <- dbGetQuery(
  conn = AWS_ATHENA_CONN_JDBC, glue("
  SELECT *
  FROM ccao.land_site_rate
  WHERE year = '{params$assessment$year}'
  ")
)

land_nbhd_rate_data <- dbGetQuery(
  conn = AWS_ATHENA_CONN_JDBC, glue("
  SELECT *
  FROM ccao.land_nbhd_rate
  WHERE year = '{params$assessment$year}'
  ")
)
tictoc::toc()

# Close connection to Athena
dbDisconnect(AWS_ATHENA_CONN_JDBC)
rm(AWS_ATHENA_CONN_JDBC, aws_athena_jdbc_driver)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Define Functions ----------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Ingest-specific helper functions for data cleaning, spatial lags, etc.

# Create a dictionary of column types, as specified in ccao::vars_dict
col_type_dict <- ccao::vars_dict %>%
  distinct(var_name = var_name_model, var_type = var_data_type)

# Mini-function to ensure that columns are the correct type
recode_column_type <- function(col, col_name, dict = col_type_dict) {
  col_type <- dict %>%
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


# Find the K-nearest neighbors within a group to calculate a spatial lag
st_knn <- function(x, y = NULL, k = 1) {
  s2x <- sf::st_as_s2(x)
  if (is.null(y)) {
    z <- s2::s2_closest_edges(s2x, s2x, k = (k + 1))
    # Drop the starting observation/point
    z <- lapply(z, sort)
    z <- lapply(seq_along(z), function(i) setdiff(z[[i]], i))
    return(z)
  } else {
    s2y <- sf::st_as_s2(y)
    z <- s2::s2_closest_edges(s2x, s2y, k = k)
  }
}




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4. Home Improvement Exemptions -----------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# HIEs need to be combined with the training data such that the training data
# uses the characteristics at the time of sale, rather than the un-updated
# characteristics used for assessment. See GitLab wiki for more information:
# https://gitlab.com/groups/ccao-data-science---modeling/-/wikis/Residential/Home%20Improvement%20Exemptions

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
    ind_pin_is_multicard = "0",
    year = as.character(year)
  )

# Merge the HIE data with the training data, updating/adding to training data
# characteristics
training_data_w_hie <- training_data %>%
  mutate(char_ncu = as.numeric(char_ncu)) %>%
  left_join(
    hie_data_training_sparse,
    by = c("meta_pin" = "pin", "meta_year" = "year", "ind_pin_is_multicard")
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
  )


## 4.2. Assessment Data ----------------------------------------------------------

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
    ind_pin_is_multicard = "0",
    year = as.character(year)
  )

# Update assessment data with any expiring HIEs. Add a field for the number
# of HIEs expired for each PIN
assessment_data_w_hie <- assessment_data %>%
  mutate(char_ncu = as.numeric(char_ncu)) %>%
  left_join(
    hie_data_assessment_sparse,
    by = c("meta_pin" = "pin", "meta_year" = "year", "ind_pin_is_multicard")
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
  rename(hie_num_expired = hie_num_active)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 5. Add Features and Clean ----------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

## 5.1. Training Data ----------------------------------------------------------

# Clean up the training data. Goal is to get it into a publishable format.
# Final featurization, filling, etc. is handled via Tidymodels recipes
training_data_clean <- training_data_w_hie %>%
  # Recode factor variables using the definitions stored in ccao::vars_dict
  # This will remove any categories not stored in the dictionary and convert
  # them to NA (useful since there are a lot of misrecorded variables)
  ccao::vars_recode(cols = starts_with("char_"), type = "code") %>%
  # Coerce columns to the data types recorded in the dictionary. Necessary
  # because the SQL drivers will often coerce types on pull (boolean becomes
  # character)
  mutate(across(everything(), ~ recode_column_type(.x, cur_column()))) %>%
  # Create sale date features using lubridate
  dplyr::mutate(
    # Calculate interval periods and times since Jan 01, 1997
    time_interval = interval(ymd("1997-01-01"), ymd(.data$meta_sale_date)),
    time_sale_year = year(meta_sale_date),
    time_sale_day = time_interval %/% days(1),

    # Get components of dates for to correct for seasonality
    time_sale_quarter_of_year = paste0("Q", quarter(meta_sale_date)),
    time_sale_day_of_year = yday(meta_sale_date),
    
    # Time window to use for cross-validation and calculating spatial lags
    time_split = time_interval %/% months(params$input$time_split)
  )

# Calculate KNN spatial lags for each N month period used in CV. The N month
# partitioning ensures that no training data leaks into the validation set
# during CV
training_data_lagged <- training_data_clean %>%
  # Convert coords to geometry used to calculate weights
  filter(!is.na(loc_longitude), !is.na(loc_latitude)) %>%
  st_as_sf(
    coords = c("loc_longitude", "loc_latitude"),
    crs = 4326,
    remove = FALSE
  ) %>%
  # Divide training data into N-month periods and calculate spatial lags
  # for each period
  group_by(time_split) %>%
  mutate(
    nb = st_knn(geometry, k = params$input$spatial_lag$k),
    across(
      all_of(params$input$spatial_lag$predictor),
      function(x) purrr::map_dbl(nb, function(idx, var = x) mean(var[idx])),
      .names = "lag_{.col}"
    )
  ) %>%
  # Clean up output, bind rows that were missing lat/lon, and write to file
  ungroup() %>%
  st_drop_geometry() %>%
  bind_rows(
    training_data_clean %>%
      filter(is.na(loc_x_3435) | is.na(loc_y_3435))
  ) %>%
  select(-c(nb, time_interval)) %>%
  write_parquet(paths$input$training$local)


## 5.2. Assessment Data --------------------------------------------------------

# Clean the assessment data. This the target data that the trained model is
# used on. The cleaning steps are the same as above, with the exception of the
# time vars and identifying complexes
assessment_data_clean <- assessment_data_w_hie %>%
  ccao::vars_recode(cols = starts_with("char_"), type = "code") %>%
  mutate(across(everything(), ~ recode_column_type(.x, cur_column()))) %>%
  # Create sale date features BASED ON THE ASSESSMENT DATE. The model predicts
  # the sale price of properties on the date of assessment. Not the date of an
  # actual sale
  dplyr::mutate(
    meta_sale_date = as_date(params$assessment$date),
    time_interval = interval(ymd("1997-01-01"), ymd(.data$meta_sale_date)),
    time_sale_year = year(meta_sale_date),
    time_sale_day = time_interval %/% days(1),
    time_sale_quarter_of_year = paste0("Q", quarter(meta_sale_date)),
    time_sale_day_of_year = yday(meta_sale_date),
    time_split = time_interval %/% months(params$input$time_split)
  )

# Grab the most recent sale within the last 3 split periods for each PIN. This
# will be the search space for the spatial lag for assessment data
sales_data <- training_data_clean %>%
  filter(
    meta_sale_date >= as_date(params$assessment$date) -
      months(params$input$time_split * 3),
    !ind_pin_is_multicard,
    !is.na(loc_longitude)
  ) %>%
  group_by(meta_pin) %>%
  filter(max(meta_sale_date) == meta_sale_date) %>%
  ungroup() %>%
  st_as_sf(
    coords = c("loc_longitude", "loc_latitude"),
    crs = 4326,
    remove = TRUE
  ) %>%
  select(
    meta_pin, meta_year, meta_sale_price, 
    all_of(params$input$spatial_lag$predictor),
  )

# Join the sales data to the assessment data. Create lagged spatial predictors
# using the K-nearest neighboring sales. This replicates a spatial Durbin model
# with a time component
assessment_data_lagged <- assessment_data_clean %>%
  filter(!is.na(loc_longitude), !is.na(loc_latitude)) %>%
  st_as_sf(
    coords = c("loc_longitude", "loc_latitude"),
    crs = 4326,
    remove = FALSE
  ) %>%
  mutate(
    nb = st_knn(geometry, sales_data$geometry, k = params$input$spatial_lag$k),
    meta_sale_price = NA_real_,
    across(
      all_of(params$input$spatial_lag$predictor),
      function(x) purrr::map_dbl(nb, function(idx, var = dplyr::cur_column()) {
        mean(sales_data[[var]][idx])
      }),
      .names = "lag_{.col}"
    )
  ) %>%
  # Clean up output and write to file
  ungroup() %>%
  st_drop_geometry() %>%
  bind_rows(
    assessment_data_clean %>%
      filter(is.na(loc_x_3435) | is.na(loc_y_3435))
  ) %>%
  select(-c(nb, meta_sale_price, time_interval)) %>%
  write_parquet(paths$input$assessment$local)


## 5.3. Complex IDs ------------------------------------------------------------

# Townhomes and rowhomes within the same "complex" or building should
# ultimately receive the same final assessed value. However, a single row of
# identical townhomes can have multiple PINs and the CCAO does not maintain a
# unique complex ID. Further, PINs within a complex often have nearly, but not
# exactly, identical characteristics.

# To solve this issue and assign each complex an ID, we do some clever "fuzzy"
# joining and then link each PIN into an undirected graph. See this SO post
# for more details on the methodology:
# https://stackoverflow.com/questions/68353869/create-group-based-on-fuzzy-criteria
complex_id_temp <- assessment_data_clean %>%
  filter(meta_class %in% c("210", "295")) %>%
  # Self-join with attributes that must be exactly matching
  select(
    meta_pin, meta_card_num, meta_township_code, meta_class,
    char_bsmt, char_gar1_size, char_attic_fnsh, char_beds,
    char_rooms, char_bldg_sf, char_yrblt, loc_x_3435, loc_y_3435
  ) %>%
  full_join(eval(.), by = params$input$complex$match_exact) %>%
  # Filter with attributes that can be "fuzzy" matched
  filter(
    char_rooms.x >= char_rooms.y - params$input$complex$match_fuzzy$rooms,
    char_rooms.x <= char_rooms.y + params$input$complex$match_fuzzy$rooms,
    char_bldg_sf.x >= char_bldg_sf.y - params$input$complex$match_fuzzy$bldg_sf,
    char_bldg_sf.x <= char_bldg_sf.y + params$input$complex$match_fuzzy$bldg_sf,
    ((char_yrblt.x >= char_yrblt.y - params$input$complex$match_fuzzy$yrblt &
      char_yrblt.x <= char_yrblt.y + params$input$complex$match_fuzzy$yrblt) |
      is.na(char_yrblt.x)
    ),

    # Units must be within 250 feet of other units
    ((loc_x_3435.x >= loc_x_3435.y - params$input$complex$match_fuzzy$dist_ft &
      loc_x_3435.x <= loc_x_3435.y + params$input$complex$match_fuzzy$dist_ft) |
      is.na(loc_x_3435.x)
    ),
    ((loc_y_3435.x >= loc_y_3435.y - params$input$complex$match_fuzzy$dist_ft &
      loc_y_3435.x <= loc_y_3435.y + params$input$complex$match_fuzzy$dist_ft) |
      is.na(loc_y_3435.x)
    )
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

# Attach original PIN data and fill any missing with a sequential integer
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
  write_parquet(paths$input$complex_id$local)


## 5.4. Land Rates -------------------------------------------------------------

# Write land data directly to file, since it's already mostly clean
land_site_rate_data %>%
  select(meta_pin = pin, meta_class = class, land_rate_per_pin, year) %>%
  write_parquet(paths$input$land_site_rate$local)
land_nbhd_rate_data %>%
  select(meta_nbhd = town_nbhd, land_rate_per_sqft) %>%
  write_parquet(paths$input$land_nbhd_rate$local)

# Reminder to upload to DVC store
message(
  "Be sure to add updated input data to DVC and finalized data to git LFS!\n",
  "See https://dvc.org/doc/start/data-and-model-versioning for more information"
)

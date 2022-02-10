#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Setup ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Pre-allocate memory for java JDBC driver
options(java.parameters = "-Xmx10g")

# Load R libraries
library(arrow)
library(ccao)
library(DBI)
library(dplyr)
library(glue)
library(here)
library(igraph)
library(lubridate)
library(RJDBC)
library(tictoc)
library(tidyr)
source(here("R", "helpers.R"))

# Initialize a dictionary of file paths and URIs. See R/file_dict.csv
paths <- model_file_dict()

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
  Schema = "Default"
)

# Set the assessment year of the data (data year to base assessment on)
model_assessment_data_year <- Sys.getenv(
  "MODEL_ASSESSMENT_DATA_YEAR", unset = lubridate::year(Sys.Date()) - 1
)

# Get the year of assessment
model_assessment_year <- Sys.getenv(
  "MODEL_ASSESSMENT_YEAR",
  unset = lubridate::year(Sys.Date())
)

# Set the assessment date, usually Jan 1st
model_assessment_date <- Sys.getenv(
  "MODEL_ASSESSMENT_DATE",
  unset = lubridate::make_date(lubridate::year(Sys.Date()))
)

# Get the minimum and maximum years to use for the training data (sales) sample
model_min_sale_year <- Sys.getenv(
  "MODEL_MIN_SALE_YEAR",
  unset = as.numeric(model_assessment_data_year) - 7
)
model_max_sale_year <- Sys.getenv(
  "MODEL_MAX_SALE_YEAR",
  unset = as.numeric(model_assessment_data_year)
)

# Whether or not to generate a new set of townhome complex identifiers
model_generate_complex_id <- Sys.getenv(
  "MODEL_GENERATE_COMPLEX_ID",
  unset = FALSE
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
      BETWEEN '{model_min_sale_year}' 
      AND '{model_max_sale_year}')
  AND ((sale.sale_price_log10
      BETWEEN sale.sale_filter_lower_limit
      AND sale.sale_filter_upper_limit)
      AND sale.sale_filter_count >= 10)
  AND NOT is_multisale
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
  WHERE meta_year = '{model_assessment_data_year}'
  ")
)
tictoc::toc()

# Pull site-specific (pre-determined) land values and neighborhood-level land
# rates per sqft, as calculated by Valuations
tictoc::tic()
land_site_rate_data <- dbGetQuery(
  conn = AWS_ATHENA_CONN_JDBC, glue("
  SELECT *
  FROM other.land_site_rate
  WHERE year = '{model_assessment_year}'
  ")
)

land_nbhd_rate_data <- dbGetQuery(
  conn = AWS_ATHENA_CONN_JDBC, glue("
  SELECT *
  FROM other.land_nbhd_rate
  WHERE year = '{model_assessment_year}'
  ")
)
tictoc::toc()

# Close connection to Athena
dbDisconnect(AWS_ATHENA_CONN_JDBC)
rm(AWS_ATHENA_CONN_JDBC, aws_athena_jdbc_driver)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Clean Data ----------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Create a dictionary of column types, as specified in ccao::vars_dict
col_type_dict <- ccao::vars_dict %>%
  distinct(var_name = var_name_model, var_type = var_data_type)

# Mini-function to ensure that columns are the correct type
recode_column_type <- function(col, col_name, dict = col_type_dict) {
  col_type <- dict %>%
    filter(var_name == col_name) %>%
    pull(var_type)
  
  switch(
    col_type,
    numeric = as.numeric(col),
    character = as.character(col),
    logical = as.logical(as.numeric(col)),
    categorical = as.factor(col),
    date = lubridate::as_date(col)
  )
}


## 3.1. Training Data ----------------------------------------------------------

# Clean up the training data. Goal is to get it into a publishable format.
# Final featurization, filling, etc. is handled via recipes
training_data_clean <- training_data %>%
  
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
    time_sale_week = time_interval %/% weeks(1),
    
    # Get components of dates for fixed effects to correct seasonality
    time_sale_quarter_of_year = quarter(meta_sale_date),
    time_sale_week_of_year = week(meta_sale_date),

    # Create indicators for dates that fall in particular months
    time_sale_during_school_year = month(meta_sale_date) %in% c(1:5, 9:12),
    time_sale_during_holidays = month(meta_sale_date) %in% c(11, 12, 1)
  ) %>%
  select(-any_of("time_interval")) %>%
  write_parquet(paths$input$training$local)


## 3.2. Assessment Data --------------------------------------------------------

# Clean the assessment data. This the target data that the trained model is 
# used on. The cleaning steps are the same as above, with the exception of the
# time vars and identifying complexes
assessment_data_clean <- assessment_data %>%
  ccao::vars_recode(cols = starts_with("char_"), type = "code") %>%
  mutate(across(everything(), ~ recode_column_type(.x, cur_column()))) %>%
  
  # Create sale date features BASED ON THE ASSESSMENT DATE. The model predicts
  # the sale price of properties on the date of assessment. Not the date of an
  # actual sale
  dplyr::mutate(
    meta_sale_date = as_date(model_assessment_date),
    time_interval = interval(ymd("1997-01-01"), ymd(.data$meta_sale_date)),
    time_sale_year = year(meta_sale_date),
    time_sale_week = time_interval %/% weeks(1),
    time_sale_quarter_of_year = quarter(meta_sale_date),
    time_sale_week_of_year = week(meta_sale_date),
    time_sale_during_school_year = month(meta_sale_date) %in% c(1:5, 9:12),
    time_sale_during_holidays = month(meta_sale_date) %in% c(11, 12, 1)
  ) %>%
  select(-any_of("time_interval")) %>%
  write_parquet(paths$input$assessment$local)


## 3.3. Complex IDs ------------------------------------------------------------

# Townhomes and rowhomes within the same "complex" or building should
# ultimately receive the same final assessed value. However, a single row of
# identical townhomes can have multiple PINs and the CCAO does not maintain a
# unique complex ID. Further, PINs within a complex often have nearly, but not
# exactly, identical characteristics.

# To solve this issue and assign each complex an ID, we do some clever "fuzzy"
# joining and then link each PIN into an undirected graph. See this SO post
# for more details on the methodology: 
# https://stackoverflow.com/questions/68353869/create-group-based-on-fuzzy-criteria
if (model_generate_complex_id) {
  complex_id_temp <- assessment_data_clean %>%
    filter(meta_class %in% c("210", "295")) %>%
    
    # Self-join with attributes that must be exactly matching
    select(
      meta_pin, meta_card_num, meta_township_code, meta_class,
      char_bsmt, char_gar1_size, char_attic_fnsh, char_beds,
      char_rooms, char_bldg_sf, char_yrblt, loc_x_3435, loc_y_3435 
    ) %>%
    full_join(
      eval(.), by = c(
        "meta_township_code", "meta_class", "char_bsmt",
        "char_gar1_size", "char_attic_fnsh", "char_beds"
      )
    ) %>%
    
    # Filter with attributes that can be "fuzzy" matched
    filter(
      char_rooms.x >= char_rooms.y - 1,
      char_rooms.x <= char_rooms.y + 1,
      char_bldg_sf.x >= char_bldg_sf.y - 25,
      char_bldg_sf.x <= char_bldg_sf.y + 25,
      ((char_yrblt.x >= char_yrblt.y - 4 &
          char_yrblt.x <= char_yrblt.y + 4) |
         is.na(char_yrblt.x)
      ),
      
      # Units must be within 250 feet of other units
      ((loc_x_3435.x >= loc_x_3435.y - 250 &
          loc_x_3435.x <= loc_x_3435.y + 250) |
         is.na(loc_x_3435.x)
      ),
      ((loc_y_3435.x >= loc_y_3435.y - 250 &
          loc_y_3435.x <= loc_y_3435.y + 250) |
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
}


## 3.4. Land Rates -------------------------------------------------------------

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

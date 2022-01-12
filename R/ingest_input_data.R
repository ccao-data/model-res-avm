# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Setup ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Pre-allocate memory for java JDBC driver
options(java.parameters = "-Xmx10g")
gc()

# Load R libraries
library(arrow)
library(DBI)
library(dplyr)
library(glue)
library(RJDBC)
library(tictoc)

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

# Set valuation parameters. Here we're setting the assessment year of the data
assessment_year <- Sys.getenv(
  "R_ASSESSMENT_YEAR", unset = format(Sys.Date(), "%Y")
)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Pull Data ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Pull the training data, which contains actual sales + attached characteristics
# from the residential input view. Drop multi-code property and outlier sales
tictoc::tic()
training_data <- dbGetQuery(
  conn = AWS_ATHENA_CONN_JDBC, "
  SELECT
      sale.sale_price AS meta_sale_price,
      sale.sale_date AS meta_sale_date,
      sale.doc_no AS meta_document_num,
      res.*
  FROM model.vw_res_input res
  INNER JOIN default.vw_pin_sale sale
      ON sale.pin = res.meta_pin
      AND sale.year = res.meta_year
  WHERE res.meta_year >= '2015'
  AND NOT res.ind_pin_is_multicard
  AND ((sale.sale_price_log10
      BETWEEN sale.sale_filter_lower_limit
      AND sale.sale_filter_upper_limit)
      AND sale.sale_filter_count >= 10)
  "
)
tictoc::toc()

# Pull all residential PIN input data for the assessment year. This will be the
# data we actually run the model on
tictoc::tic()
assessment_data <- dbGetQuery(
  conn = AWS_ATHENA_CONN_JDBC, glue("
  SELECT *
  FROM model.vw_res_input
  WHERE meta_year = '{assessment_year}'
  ")
)
tictoc::toc()




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Clean Data ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -




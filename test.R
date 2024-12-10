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
library(sf)
# Adds arrow support to speed up ingest process.
noctua_options(unload = TRUE)

# Establish Athena connection
AWS_ATHENA_CONN_NOCTUA <- dbConnect(noctua::athena())

assessment_data <- dbGetQuery(
  conn = AWS_ATHENA_CONN_NOCTUA, glue("
  SELECT *
  FROM model.vw_card_res_input
  WHERE year BETWEEN '{as.numeric(params$assessment$data_year) - 1}'
    AND '{params$assessment$data_year}'
  ")
)
tictoc::toc()
# Load neighborhood data and set CRS
nbhd <- dbGetQuery(
  conn = AWS_ATHENA_CONN_NOCTUA,
  glue("SELECT * FROM spatial.neighborhood WHERE Year = '2019'")
) %>%
  st_as_sf(crs = 4326) # Convert directly to sf object with CRS

# Create a buffered neighborhood
nbhd_buffered <- st_buffer(nbhd, dist = 1000)

# Convert training_data to an sf object with the same CRS
test <- assessment_data %>%
  filter(!is.na(loc_longitude) & !is.na(loc_latitude)) %>%
  st_as_sf(coords = c("loc_longitude", "loc_latitude"), crs = st_crs(nbhd)) %>%
  filter(meta_year == '2023')



# Transform training_data to the same CRS as the neighborhood buffer
test <- st_transform(test, crs = st_crs(nbhd_buffered))

# Perform spatial joins for original and buffered neighborhoods
within_original_grouped <- st_join(test, nbhd, join = st_within)
within_buffered_grouped <- st_join(test, nbhd_buffered, join = st_within)

# Ensure both datasets are sf objects and have the geometry columns intact
buffered <- within_buffered_grouped %>%
  select(meta_pin, town_nbhd, geometry, char_bldg_sf, char_yrblt) %>%
  st_as_sf() %>%
  st_as_sf(crs = 4326)

# Perform a left join by meta_pin to match attributes
target <- within_original_grouped %>%
  select(meta_pin, town_nbhd, geometry) %>%
  st_as_sf() %>%
  st_as_sf(crs = 4326)



calculate_distances <- function(target, buffered) {
  # Check and align CRS
  if (st_crs(target) != st_crs(buffered)) {
    cat("Aligning CRS between target and buffered datasets.\n")
    buffered <- st_transform(buffered, st_crs(target))  # Transform buffered CRS to match target CRS
  }

  total_rows <- nrow(target) # Total number of rows in the target dataset

  # Initialize an empty result list
  results <- list()

  for (i in seq_len(total_rows)) {
    # Extract the current target row
    current_target <- target[i, ]

    # Ensure CRS of current_target is set
    if (is.null(st_crs(current_target))) {
      current_target <- st_set_crs(current_target, st_crs(target))
    }

    # Calculate distances for all matching pins in buffered
    filtered_data <- buffered %>%
      filter(town_nbhd == current_target$town_nbhd[[1]]) %>%  # Match by neighborhood
      mutate(
        distance_to_target = as.numeric(st_distance(geometry, current_target$geometry)),
        distance_within_1000ft = distance_to_target <= 1000,
        target_meta_pin = current_target$meta_pin[[1]]  # Add the meta_pin from target for reference
      ) %>%
      filter(distance_within_1000ft)

    # Append the result to the list
    results[[i]] <- filtered_data

    # Display progress count
    cat("Processed", i, "out of", total_rows, "rows\n")
  }

  # Combine all results into a single data frame
  result <- bind_rows(results)

  return(result)
}

result <- calculate_distances(target, buffered)

result %>%
  group_by(target_meta_pin) %>%
  summarise(
    mean_char_yrblt = mean(char_yrblt, na.rm = TRUE),
    mean_char_bldg_sf = mean(char_bldg_sf, na.rm = TRUE),
    standard_deviation_char_yrblt = sd(char_yrblt, na.rm = TRUE),
    standard_deviation_char_bldg_sf = sd(char_bldg_sf, na.rm = TRUE)
  ) %>%
  write_parquet("output.parquet")

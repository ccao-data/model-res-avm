# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Setup ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Start the script timer and clear logs from prior script
tictoc::tic.clearlog()
tictoc::tic("Assess")

# Load R libraries
library(arrow)
library(assessr)
library(ccao)
library(dplyr)
library(here)
library(lightsnip)
library(purrr)
library(recipes)
library(stringr)
library(tictoc)

# Load helpers and recipes from files
walk(list.files("R/", "\\.R$", full.names = TRUE), source)

# Initialize a dictionary of file paths and URIs. See R/helpers.R
paths <- model_file_dict()

# Get year/stage to use for previous comparison/ratio studies
model_assessment_data_year <- Sys.getenv("MODEL_ASSESSMENT_DATA_YEAR")
rsn_year <- Sys.getenv(
  "MODEL_RATIO_STUDY_NEAR_YEAR", model_assessment_data_year
)
rsn_stage <- Sys.getenv(
  "MODEL_RATIO_STUDY_NEAR_STAGE", "mailed"
)
rsf_year <- Sys.getenv(
  "MODEL_RATIO_STUDY_FAR_YEAR", "2019"
)
rsf_stage <- Sys.getenv(
  "MODEL_RATIO_STUDY_FAR_STAGE",
  as.character(as.numeric(model_assessment_data_year) - 3)
)

# Column names to use for previous ratio study comparison
rsf_column <- get_rs_col_name(model_assessment_data_year, rsf_year, rsf_stage)
rsn_column <- get_rs_col_name(model_assessment_data_year, rsn_year, rsn_stage)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Setup ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Only create the assessment performance set if running in a local (non CI)
# session
if (interactive()) {
  
  # Load the final lightgbm model object and recipe from file
  lgbm_final_full_fit <- lightsnip::lgbm_load(paths$output$workflow_fit$local)
  lgbm_final_full_recipe <- readRDS(paths$output$workflow_recipe$local)
  
  # Load the MOST RECENT sale per PIN for the same year as the assessment data.
  # We want our assessed value to be as close to the most recent sale
  training_data <- read_parquet(paths$input$training$local) %>%
    filter(meta_year == model_assessment_data_year) %>%
    group_by(meta_pin) %>%
    filter(meta_sale_date == max(meta_sale_date)) %>%
    distinct(
      meta_pin, meta_year, meta_sale_price,
      meta_sale_date, meta_sale_document_num
    ) %>%
    ungroup()
  
  # Load the data for assessment. This is the universe of IMPROVEMENTs (not
  # PINs) that needs values. Use the trained lightgbm model to estimate a value
  assessment_data_pred <- read_parquet(paths$input$assessment$local) %>%
    as_tibble() %>%
    mutate(
      initial_pred_fmv = predict(
        lgbm_final_full_fit,
        new_data = bake(
          lgbm_final_full_recipe,
          new_data = .,
          all_predictors()
        )
      )$.pred
    )
  
  # Join sales data to each PIN, then collapse the improvement-level assessment
  # data to the PIN level, summing the predicted value for multicard PINs. Keep
  # only columns needed for performance calculations. This data is used for
  # performance measurement
  assessment_data <- assessment_data_pred %>%
    select(-meta_sale_date) %>%
    left_join(training_data, by = c("meta_year", "meta_pin")) %>%
    group_by(
      meta_year, meta_pin, meta_triad_code,
      meta_class, ind_pin_is_multicard
    ) %>%
    summarize(
      meta_sale_price = first(meta_sale_price),
      char_bldg_sf = sum(char_bldg_sf),
      initial_pred_fmv = sum(initial_pred_fmv),
      across(
        c(any_of(c(rsf_column, rsn_column)),
          meta_township_code, meta_nbhd_code, loc_cook_municipality_name,
          loc_chicago_ward_num, loc_census_puma_geoid, loc_census_tract_geoid,
          loc_school_elementary_district_geoid,
          loc_school_secondary_district_geoid,
          loc_school_unified_district_geoid
        ),
        first
      )
    ) %>%
    ungroup() %>%
    write_parquet(paths$intermediate$assessment$local)
  
  
  ## Bunch of PIN-level stuff happens here (placeholder)
  assessment_data_pred %>%
    select(
      meta_year, meta_pin, meta_class, meta_card_num, loc_longitude,
      loc_latitude, initial_pred_fmv
    ) %>%
    write_parquet(paths$output$assessment$local)
  
}

# End the script timer and write the time elapsed to file
tictoc::toc(log = TRUE)
arrow::read_parquet(paths$output$timing$local) %>%
  bind_rows(., tictoc::tic.log(format = FALSE)) %>%
  arrow::write_parquet(paths$output$timing$local)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Setup #####
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

# Initialize a dictionary of file paths and URIs. See R/file_dict.csv
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

# Load the run type from the metadata file, if it exists
if (file.exists(paths$output$metadata$local)) {
  model_run_type <- read_parquet(paths$output$metadata$local, "run_type") %>%
    dplyr::pull(run_type)
} else {
  model_run_type <- "automated"
}




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Assess Improvements #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Only create the assessment data if running interactively (non-CI). Otherwise,
# use only the test set data for performance measurement. See README for details
if (interactive()) {
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ##### Generate Predictions #####
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  # Load the final lightgbm model object and recipe from file
  lgbm_final_full_fit <- lightsnip::lgbm_load(paths$output$workflow_fit$local)
  lgbm_final_full_recipe <- readRDS(paths$output$workflow_recipe$local)
  
  # Load the data for assessment. This is the universe of IMPROVEMENTs (not
  # PINs) that needs values. Use the trained lightgbm model to estimate a value
  assessment_data_pred <- read_parquet(paths$input$assessment$local) %>%
    as_tibble() %>%
    mutate(
      pred_card_initial_fmv = predict(
        lgbm_final_full_fit,
        new_data = bake(
          lgbm_final_full_recipe,
          new_data = .,
          all_predictors()
        )
      )$.pred
    )
  
  
  
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ##### Post-Modeling Adjustments #####
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  ### Negative predictions
  
  # Sometimes the model predicts the value of very low value properties as
  # negative, particularly if it hasn't had many training iterations. In this
  # case, we bump any negative predictions to $10K
  assessment_data_pred <- assessment_data_pred %>%
    mutate(
      pred_card_intermediate_fmv = ifelse(
        pred_card_initial_fmv >= 10000,
        pred_card_initial_fmv, 10000
      )
    )
  
  
  ### Multi-cards
  
  # Cards represent buildings/improvements. A PIN can have multiple cards, and
  # the total taxable value of the PIN is the sum of all cards
  assessment_data_mc <- assessment_data_pred %>%
    select(
      meta_pin, meta_class, char_bldg_sf, meta_card_num,
      meta_tieback_key_pin, meta_tieback_proration_rate,
      meta_1yr_pri_board_tot, pred_card_intermediate_fmv
    ) %>%
    
    # For prorated PINs with multiple cards, take the average of the
    # card (building) across PINs
    group_by(meta_tieback_key_pin, meta_card_num) %>%
    mutate(
      pred_card_intermediate_fmv = ifelse(
        is.na(meta_tieback_key_pin),
        pred_card_intermediate_fmv,
        mean(pred_card_intermediate_fmv)
      )
    ) %>%
    
    # Aggregate multi-cards to the PIN-level by summing the predictions
    # of all cards. We use a heuristic here to limit the PIN-level total
    # value, this is to prevent super-high-value back-buildings/ADUs from
    # blowing up the PIN-level AV
    group_by(meta_pin) %>%
    mutate(
      pred_pin_final_fmv = ifelse(
        sum(pred_card_intermediate_fmv) * meta_tieback_proration_rate <=
          2 * first(meta_1yr_pri_board_tot * 10) |
          is.na(meta_1yr_pri_board_tot),
        sum(pred_card_intermediate_fmv),
        max(pred_card_intermediate_fmv)
      )
    )
  

  ### Townhome complexes
  
  # For class 210 and 295s, we want all units in the same complex to
  # have the same value (assuming they are identical)
  
  # Load townhome/rowhome complex IDs
  complex_id_data <- read_parquet(paths$input$complex_id$local) %>%
    select(meta_pin, meta_complex_id)
  
  # Join complex IDs to the predictions, then for each complex, set the
  # prediction to the average prediction of the complex. Also, multiply
  # the PIN-level value by the PIN's proration rate
  assessment_data_cid <- assessment_data_mc %>%
    left_join(complex_id_data, by = "meta_pin") %>%
    group_by(meta_complex_id, meta_tieback_proration_rate) %>%
    mutate(
      pred_pin_final_fmv = ifelse(
        is.na(meta_complex_id),
        pred_pin_final_fmv * meta_tieback_proration_rate,
        mean(pred_pin_final_fmv) * meta_tieback_proration_rate
      )
    ) %>%
    ungroup()
  
  
  ### Finalize
  
  # Round PIN-level predictions to the nearest $500
  assessment_data_final <- assessment_data_cid %>%
    mutate(
      pred_pin_final_fmv_round = ccao::val_round_fmv(
        pred_pin_final_fmv,
        round_to = c(500, 500),
        type = "normal"
      )
    ) %>%
    
    # Apportion the PIN-level value back out to the card-level using
    # the square footage of each improvement
    group_by(meta_pin) %>%
    mutate(
      pred_card_final_fmv = pred_pin_final_fmv_round *
        (char_bldg_sf / sum(char_bldg_sf))
    ) %>%
    ungroup()
  
  # Merge the finalized card-level data back to the main tibble of predictions
  assessment_data_merged <- assessment_data_pred %>%
    left_join(
      assessment_data_final %>%
        select(
          meta_pin, meta_card_num, meta_complex_id,
          pred_card_final_fmv, pred_pin_final_fmv, pred_pin_final_fmv_round
        ),
      by = c("meta_pin", "meta_card_num")
    ) %>%
    relocate(meta_complex_id, .after = "meta_tax_code")
  
  # The test PINs below can be used to ensure that the order of operations
  # for the adjustments above results in a sensible outcome:
  # 17321110470000 05174150240000 05213220250000 08121220400000 06334030310000
  
  
  
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ##### Save Assessment Improvement Level Data #####
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  # Generate individual improvement-level values only for candidate and final
  # runs. This is to save space and avoid long computations for every run
  if (model_run_type %in% c("candidate", "final")) {
    
    ## Bunch of PIN-level stuff happens here (placeholder)
    assessment_data_merged %>%
      mutate(
        township_code = meta_township_code,
        meta_year = as.character(meta_year)
      ) %>%
      write_parquet(paths$output$assessment$local)
  }
  
  
  
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ##### Save Assessment Performance Data #####
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  # Load the MOST RECENT sale per PIN for the same year as the assessment data.
  # We want our assessed value to be as close as possible to the most
  # recent sale
  training_data <- read_parquet(paths$input$training$local) %>%
    filter(meta_year == model_assessment_data_year) %>%
    group_by(meta_pin) %>%
    filter(meta_sale_date == max(meta_sale_date)) %>%
    distinct(
      meta_pin, meta_year, meta_sale_price,
      meta_sale_date, meta_sale_document_num
    ) %>%
    ungroup()
  
  # Join sales data to each PIN, then collapse the improvement-level assessment
  # data to the PIN level, summing the predicted value for multicard PINs. Keep
  # only columns needed for performance calculations. This data is used for
  # performance measurement
  assessment_data_merged %>%
    select(-meta_sale_date) %>%
    left_join(training_data, by = c("meta_year", "meta_pin")) %>%
    group_by(
      meta_year, meta_pin, meta_triad_code,
      meta_class, ind_pin_is_multicard
    ) %>%
    summarize(
      meta_sale_price = first(meta_sale_price),
      char_bldg_sf = sum(char_bldg_sf),
      pred_pin_final_fmv_round = first(pred_pin_final_fmv_round),
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
}

# End the script timer and write the time elapsed to file
tictoc::toc(log = TRUE)
arrow::read_parquet(paths$intermediate$timing$local) %>%
  bind_rows(., tictoc::tic.log(format = FALSE)) %>%
  arrow::write_parquet(paths$intermediate$timing$local)
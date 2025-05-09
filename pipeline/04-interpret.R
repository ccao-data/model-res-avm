#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Setup ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# NOTE: See DESCRIPTION for library dependencies and R/setup.R for
# variables used in each pipeline stage

# Start the stage timer and clear logs from prior stage
tictoc::tic.clearlog()
tictoc::tic("Interpret")

# Load libraries, helpers, and recipes from files
purrr::walk(list.files("R/", "\\.R$", full.names = TRUE), source)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. Load Data -----------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Loading model fit and recipe")

# Load the final lightgbm model object and recipe from file
lgbm_final_full_fit <- lightsnip::lgbm_load(paths$output$workflow_fit$local)
lgbm_final_full_recipe <- readRDS(paths$output$workflow_recipe$local)

if (shap_enable || comp_enable) {
  message("Loading assessment data for SHAP and comp calculation")

  # Load the input data used for assessment. This is the universe of CARDs (not
  # PINs) that need values. Will use the the trained model to calc SHAP values
  assessment_data <- as_tibble(read_parquet(paths$input$assessment$local))

  # Aggregate square footage to the parcel level for small (2-3 card)
  # multi-cards. We do this to ensure consistent SHAP values for small
  # multi-card parcels, since we use aggregated parcel square footage when
  # predicting values for these parcels. More details in multi-card handling
  # step in the assess stage.

  # Start by persisting card sort order for the purposes of aggregating
  # building square footage. We use characteristics from the largest card
  # ("frankencard") in order to predict value, so we save the card sort order
  # as a way to reference this card later on
  assessment_data_ordered <- assessment_data %>%
    group_by(meta_pin) %>%
    arrange(desc(char_bldg_sf), meta_card_num) %>%
    mutate(sqft_card_num_sort = row_number()) %>%
    ungroup()

  assessment_data <- assessment_data_ordered %>%
    mutate(
      char_bldg_sf = ifelse(
        ind_pin_is_multicard & meta_pin_num_cards %in% c(2, 3),
        sum(char_bldg_sf),
        char_bldg_sf
      ),
      .by = meta_pin
    )

  # Run the saved recipe on the assessment data to format it for prediction
  assessment_data_prepped <- recipes::bake(
    object = lgbm_final_full_recipe,
    new_data = assessment_data %>% select(-sqft_card_num_sort),
    all_predictors()
  )
}

if (comp_enable) {
  message("Loading predicted values for comp calculation")

  assessment_card <- read_parquet(paths$output$assessment_card$local) %>%
    as_tibble()
}




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Calculate SHAP Values -----------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

if (shap_enable) {
  message("Calculating SHAP values")

  # Calculate a SHAP value for each observation for each feature in the
  # assessment data. Uses lightgbm's built-in method (predcontrib = TRUE)
  shap_values <- predict(
    object = lgbm_final_full_fit$fit,
    newdata = as.matrix(assessment_data_prepped),
    type = "contrib"
  )

  # Convert the SHAP value output from a matrix to a tibble and add column names
  shap_values_tbl <- shap_values %>%
    as_tibble(.name_repair = "unique") %>%
    purrr::set_names(c(
      colnames(assessment_data_prepped),
      "pred_card_shap_baseline_fmv"
    ))

  # Keep only the SHAP value columns from predictors + any ID and partition
  # columns, then add run ID and write to file
  shap_values_final <- assessment_data %>%
    select(
      meta_year, meta_pin, meta_card_num,
      meta_pin_num_cards,
      township_code = meta_township_code,
      sqft_card_num_sort
    ) %>%
    bind_cols(shap_values_tbl) %>%
    select(
      meta_year, meta_pin, meta_card_num, sqft_card_num_sort,
      meta_pin_num_cards, pred_card_shap_baseline_fmv,
      all_of(params$model$predictor$all), township_code
    ) %>%
    # Adjust small (2-3 card) multi-cards to copy the SHAPs from the
    # "frankencard" to all of the cards in the PIN. This aligns with the way
    # that we handle small multi-cards in the assess stage.
    # Start by grouping and sorting the same way we do in the assess stage
    # so that we can figure out which card is the frankencard
    group_by(meta_pin) %>%
    arrange(sqft_card_num_sort) %>%
    group_modify(~ {
      shap_cols <- c("pred_card_shap_baseline_fmv", params$model$predictor$all)
      # If the first row indicates 2 or 3 cards,
      # duplicate its SHAP values across the group
      if (.x$meta_pin_num_cards[1] %in% c(2, 3)) {
        .x[shap_cols] <- .x[rep(1, nrow(.x)), shap_cols]
      }
      .x
    }) %>%
    arrange(meta_pin, meta_card_num) %>%
    ungroup() %>%
    select(-meta_pin_num_cards, -sqft_card_num_sort) %>%
    write_parquet(paths$output$shap$local)
} else {
  # If SHAP creation is disabled, we still need to write an empty stub file
  # so DVC doesn't complain
  arrow::write_parquet(data.frame(), paths$output$shap$local)
}




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4. Calculate Feature Importance ----------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Calculating feature importance metrics")

# Calculate feature importance using LightGBM's built-in method
lightgbm::lgb.importance(lgbm_final_full_fit$fit) %>%
  as_tibble() %>%
  rename(model_predictor_all_name = Feature) %>%
  rename_with(tolower, Gain:Frequency) %>%
  mutate(across(
    gain:frequency,
    ~ order(order(.x, decreasing = TRUE)),
    .names = "{.col}_rank"
  )) %>%
  rename_with(~ paste0(.x, "_value"), gain:frequency) %>%
  write_parquet(paths$output$feature_importance$local)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 5. Find Comparables  ---------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

if (comp_enable) {
  message("Finding comparables")
  generate_comps(
    assessment_data = assessment_data,
    training_data = read_parquet(paths$input$training$local),
    model = lgbm_final_full_fit$fit,
    recipe = lgbm_final_full_recipe,
    triad = params$assessment$triad,
    num_comps = as.integer(params$comp$num_comps)
  ) %>%
    arrow::write_parquet(paths$output$comp$local)
} else {
  # If comp creation is disabled, we still need to write an empty stub file
  # so DVC doesn't complain
  arrow::write_parquet(data.frame(), paths$output$comp$local)
}

# End the stage timer and write the time elapsed to a temporary file
tictoc::toc(log = TRUE)
bind_rows(tictoc::tic.log(format = FALSE)) %>%
  arrow::write_parquet(gsub("//*", "/", file.path(
    paths$intermediate$timing$local,
    "model_timing_interpret.parquet"
  )))

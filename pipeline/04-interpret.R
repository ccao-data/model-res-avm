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

  # Filter target properties for only the current triad, to speed up the comps
  # algorithm
  comp_assessment_data_preprocess <- assessment_data %>%
    filter(
      meta_township_code %in% (
        ccao::town_dict %>%
          filter(triad_name == tools::toTitleCase(params$assessment$triad)) %>%
          pull(township_code)
      )
    )

  # Multi-card handling. For multi-card pins with 2-3 cards, we predict by
  # aggregating the bldg_sf to a single card, and using that card to predict
  # the value for the multi-card PIN as a whole. Since we don't predict on the
  # other cards, we set them aside for comp generation, to re-attach them later
  small_multicards <- comp_assessment_data_preprocess %>%
    filter(meta_pin_num_cards %in% c(2, 3))

  frankencards <- small_multicards %>%
    group_by(meta_pin) %>%
    arrange(sqft_card_num_sort) %>%
    slice(1) %>%
    ungroup()

  single_cards_and_large_multicards <- comp_assessment_data_preprocess %>%
    filter(!meta_pin %in% frankencards$meta_pin)

  comp_assessment_data <-
    bind_rows(single_cards_and_large_multicards, frankencards)

  comp_assessment_data_prepped <- recipes::bake(
    object = lgbm_final_full_recipe,
    new_data = comp_assessment_data,
    all_predictors()
  )

  # Calculate the leaf node assignments for every predicted value.
  # Due to integer overflow problems with leaf node assignment, we need to
  # chunk our data such that they are strictly less than the limit of 1073742
  # rows. More detail here: https://github.com/microsoft/LightGBM/issues/1884
  chunk_size <- 500000
  chunks <- split(
    comp_assessment_data_prepped,
    ceiling(seq_along(comp_assessment_data_prepped[[1]]) / chunk_size)
  )
  chunked_leaf_nodes <- chunks %>%
    map(\(chunk) {
      predict(
        object = lgbm_final_full_fit$fit,
        newdata = as.matrix(chunk),
        type = "leaf",
      )
    })
  # Prefer do.call(rbind, ...) over bind_rows() because the chunks are
  # not guaranteed to have the same number of rows, and bind_rows() will raise
  # an error in that case
  leaf_nodes <- do.call(rbind, chunked_leaf_nodes) %>% as_tibble()

  # Calculate weights representing feature importance, so that we can weight
  # leaf node assignments based on the most important features.
  # To do this, we need the training data so that we can compute the mean sale
  # price and use it as the base model error
  message("Extracting weights from training data")
  training_data <- read_parquet(paths$input$training$local) %>%
    filter(!ind_pin_is_multicard, !sv_is_outlier) %>%
    as_tibble()
  training_data_prepped <- recipes::bake(
    object = lgbm_final_full_recipe,
    new_data = training_data,
    all_predictors()
  )

  # Get predicted values and leaf node assignments for the training data
  training_leaf_nodes <- predict(
    object = lgbm_final_full_fit$fit,
    newdata = as.matrix(training_data_prepped),
    type = "leaf"
  ) %>%
    as_tibble()

  # Create row-wise weights for each observation in the training data
  # with columns representing each tree in the model.
  tree_weights <- extract_tree_weights(
    model      = lgbm_final_full_fit$fit,
    leaf_idx   = as.matrix(training_leaf_nodes),
    init_score = mean(training_data$meta_sale_price, na.rm = TRUE),
    outcome    = training_data$meta_sale_price
  )

  if (length(tree_weights) == 0) {
    message("Warning: tree_weights are empty")
  }
  if (all(rowSums(tree_weights) %in% c(0, 1))) {
    message("Warning: tree_weights do not sum to 1 or 0 for each row")
    message("First 5 weights:")
    print(head(tree_weights, 5))
  }


  # Make sure that the leaf node tibbles are all integers, which is what
  # the comps algorithm expects
  leaf_nodes <- leaf_nodes %>%
    mutate(across(everything(), ~ as.integer(.x)))
  training_leaf_nodes <- training_leaf_nodes %>%
    mutate(across(everything(), ~ as.integer(.x)))

  # Do the comps calculation in Python because the code is simpler and faster
  message("Calling out to python/comps.py to perform comps calculation")
  comps_module <- import("python.comps")
  tryCatch(
    {
      comps <- comps_module$get_comps(
        leaf_nodes, training_leaf_nodes, tree_weights,
        num_comps = as.integer(params$comp$num_comps)
      )
    },
    error = function(e) {
      # Log the full Python traceback in case of an error
      print(py_last_error())
      stop("Encountered error in python/comps.py")
    }
  )

  # Translate comp indexes to PINs and document numbers
  comps[[1]] <- comps[[1]] %>%
    mutate(
      # Correct for the fact that Python is 0-indexed by incrementing the
      # comp indexes by 1, and cast null indicators (-1) to null
      across(everything(), ~ ifelse(. == -1, NA, . + 1)),
      # Map comp index to PIN
      across(
        starts_with("comp_idx_"),
        \(idx_row) {
          ifelse(is.na(idx_row), NA, training_data[idx_row, ]$meta_pin)
        },
        .names = "comp_pin_{str_remove(col, 'comp_idx_')}"
      ),
      # Map comp index to sale doc number
      across(
        starts_with("comp_idx_"),
        \(idx_row) {
          ifelse(
            is.na(idx_row),
            NA,
            training_data[idx_row, ]$meta_sale_document_num
          )
        },
        .names = "comp_document_num_{str_remove(col, 'comp_idx_')}"
      )
    ) %>%
    select(-starts_with("comp_idx_")) %>%
    cbind(
      pin = comp_assessment_data$meta_pin,
      card = comp_assessment_data$meta_card_num
    ) %>%
    relocate(pin, card)

  comp_idxs_and_scores <- cbind(comps[[1]], comps[[2]])

  # Grab removed small multi-cards, re-add them, and assign them the comps data
  # that we calculated for the frankencard
  removed_cards <- small_multicards %>%
    anti_join(frankencards, by = c("meta_pin", "meta_card_num")) %>%
    select(meta_pin, meta_card_num)

  removed_cards_comps <- removed_cards %>%
    rename(pin = meta_pin, card = meta_card_num) %>%
    left_join(comp_idxs_and_scores %>% select(-card), by = "pin")

  # Save final combined comps data
  bind_rows(comp_idxs_and_scores, removed_cards_comps) %>%
    arrange(pin, card) %>%
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

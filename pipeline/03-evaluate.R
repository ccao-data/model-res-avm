#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Setup ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# NOTE: See DESCRIPTION for library dependencies and R/setup.R for
# variables used in each pipeline stage

# Start the stage timer and clear logs from prior stage
tictoc::tic.clearlog()
tictoc::tic("Evaluate")

# Load libraries, helpers, and recipes from files
purrr::walk(list.files("R/", "\\.R$", full.names = TRUE), source)

# Enable parallel backend for generating stats more quickly
plan(multisession, workers = num_threads)

# Renaming dictionary for input columns. We want the actual value of the column
# to become geography_id and the NAME of the column to become geography_name
col_rename_dict <- c(
  "triad_code" = "meta_triad_code",
  "class" = "meta_class",
  purrr::set_names(params$ratio_study$geographies, "geography_id")
)

# Get the triad of the run to use for filtering
run_triad <- tools::toTitleCase(params$assessment$triad)
run_triad_code <- ccao::town_dict %>%
  filter(triad_name == run_triad) %>%
  distinct(triad_code) %>%
  pull(triad_code)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. Load Data -----------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Loading evaluation data")

# Load the test results from the end of the train stage. This will be the most
# recent 10% of sales and already includes predictions. This data will NOT
# include multicard sales, so the unit of observation is PINs (1 PIN per row)
test_data_card <- read_parquet(paths$output$test_card$local)

# Load the assessment results from the previous stage. This will include every
# residential PIN that needs a value. It WILL include multicard properties
# aggregated to the PIN-level
assessment_data_pin <- read_parquet(paths$output$assessment_pin$local) %>%
  filter(meta_triad_code == run_triad_code) %>%
  select(
    meta_pin, meta_class, meta_triad_code,
    all_of(params$ratio_study$geographies),
    char_total_bldg_sf, prior_far_tot, prior_near_tot,
    pred_pin_final_fmv_round, sale_ratio_study_price
  )




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Define Stats Functions ----------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Function to take either test set results or assessment results and generate
# aggregate performance statistics for different levels of geography
gen_agg_stats <- function(data, truth, estimate, bldg_sqft,
                          rsn_col, rsf_col, triad, geography, class, col_dict) {
  # List of summary stat/performance functions applied within summarize() below
  # Each function is listed on the right while the name of the function is on
  # the left
  rs_fns_list <- list(
    cod_no_sop = ~ ifelse(sum(!is.na(.y)) > 1, cod(.x / .y, na.rm = TRUE), NA),
    prd_no_sop = ~ ifelse(sum(!is.na(.y)) > 1, prd(.x, .y, na.rm = TRUE), NA),
    prb_no_sop = ~ ifelse(sum(!is.na(.y)) > 1, prb(.x, .y, na.rm = TRUE), NA),
    mki_no_sop = ~ ifelse(sum(!is.na(.y)) > 1, mki(.x, .y, na.rm = TRUE), NA),
    cod = ~ ifelse(
      sum(!is.na(.y)) > 33,
      list(ccao_cod(.x / .y, na.rm = TRUE)),
      NA
    ),
    prd = ~ ifelse(
      sum(!is.na(.y)) > 33,
      list(ccao_prd(.x, .y, na.rm = TRUE)),
      NA
    ),
    prb = ~ ifelse(
      sum(!is.na(.y)) > 33,
      list(ccao_prb(.x, .y, na.rm = TRUE)),
      NA
    )
  )
  ys_fns_list <- list(
    rmse        = rmse_vec,
    r_squared   = rsq_vec,
    mae         = mae_vec,
    mpe         = mpe_vec,
    mape        = mape_vec,
    mdape       = mdape_vec
  )
  sum_fns_list <- list(
    min         = ~ min(.x, na.rm = TRUE),
    q25         = ~ quantile(.x, na.rm = TRUE, probs = 0.25),
    median      = ~ median(.x, na.rm = TRUE),
    q75         = ~ quantile(.x, na.rm = TRUE, probs = 0.75),
    max         = ~ max(.x, na.rm = TRUE)
  )
  sum_sqft_fns_list <- list(
    min         = ~ min(.x / .y, na.rm = TRUE),
    q25         = ~ quantile(.x / .y, na.rm = TRUE, probs = 0.25),
    median      = ~ median(.x / .y, na.rm = TRUE),
    q75         = ~ quantile(.x / .y, na.rm = TRUE, probs = 0.75),
    max         = ~ max(.x / .y, na.rm = TRUE)
  )
  yoy_fns_list <- list(
    min         = ~ min((.x - .y) / .y, na.rm = TRUE),
    q25         = ~ quantile((.x - .y) / .y, na.rm = TRUE, probs = 0.25),
    median      = ~ median((.x - .y) / .y, na.rm = TRUE),
    q75         = ~ quantile((.x - .y) / .y, na.rm = TRUE, probs = 0.75),
    max         = ~ max((.x - .y) / .y, na.rm = TRUE)
  )

  # Generate aggregate performance stats by geography
  df_stat <- data %>%
    # Aggregate to get counts by geography without class
    group_by({{ triad }}, {{ geography }}) %>%
    mutate(
      num_pin_no_class = n(),
      num_sale_no_class = sum(!is.na({{ truth }}))
    ) %>%
    # Aggregate including class
    group_by({{ triad }}, {{ geography }}, {{ class }}) %>%
    summarize(

      # Basic summary stats, counts, proportions, etc
      num_pin = n(),
      num_sale = sum(!is.na({{ truth }})),
      pct_of_total_pin_by_class = num_pin / first(num_pin_no_class),
      pct_of_total_sale_by_class = num_sale / first(num_sale_no_class),
      pct_of_pin_sold = num_sale / num_pin,
      prior_far_total_av = sum({{ rsf_col }} / 10, na.rm = TRUE),
      prior_near_total_av = sum({{ rsn_col }} / 10, na.rm = TRUE),
      estimate_total_av = sum({{ estimate }} / 10, na.rm = TRUE),

      # Assessment-specific statistics
      across(
        .fns = rs_fns_list, {{ estimate }}, {{ truth }},
        .names = "{.fn}"
      ),
      median_ratio = median({{ estimate }} / {{ truth }}, na.rm = TRUE),

      # Yardstick (ML-specific) performance stats
      across(.fns = ys_fns_list, {{ truth }}, {{ estimate }}, .names = "{.fn}"),

      # Summary stats of sale price and sale price per sqft
      across(.fns = sum_fns_list, {{ truth }}, .names = "sale_fmv_{.fn}"),
      across(
        .fns = sum_sqft_fns_list, {{ truth }}, {{ bldg_sqft }},
        .names = "sale_fmv_per_sqft_{.fn}"
      ),

      # Summary stats of prior values and value per sqft. Need to multiply
      # by 10 first since PIN history is in AV, not FMV
      prior_far_num_missing = sum(is.na({{ rsf_col }})),
      across(
        .fns = sum_fns_list, {{ rsf_col }},
        .names = "prior_far_fmv_{.fn}"
      ),
      across(
        .fns = sum_sqft_fns_list, {{ rsf_col }}, {{ bldg_sqft }},
        .names = "prior_far_fmv_per_sqft_{.fn}"
      ),
      across(
        .fns = yoy_fns_list, {{ estimate }}, {{ rsf_col }},
        .names = "prior_far_yoy_pct_chg_{.fn}"
      ),
      prior_near_num_missing = sum(is.na({{ rsn_col }})),
      across(
        .fns = sum_fns_list, {{ rsn_col }},
        .names = "prior_near_fmv_{.fn}"
      ),
      across(
        .fns = sum_sqft_fns_list, {{ rsn_col }}, {{ bldg_sqft }},
        .names = "prior_near_fmv_per_sqft_{.fn}"
      ),
      across(
        .fns = yoy_fns_list, {{ estimate }}, {{ rsn_col }},
        .names = "prior_near_yoy_pct_chg_{.fn}"
      ),

      # Summary stats of estimate value and estimate per sqft
      estimate_num_missing = sum(is.na({{ estimate }})),
      across(
        .fns = sum_fns_list, {{ estimate }},
        .names = "estimate_fmv_{.fn}"
      ),
      across(
        .fns = sum_sqft_fns_list, {{ estimate }}, {{ bldg_sqft }},
        .names = "estimate_fmv_per_sqft_{.fn}"
      ),
      .groups = "drop"
    ) %>%
    ungroup() %>%
    # COD, PRD, and PRB all output to a list. We can unnest each list to get
    # additional info for each stat (95% CI, sample count, etc)
    tidyr::unnest_wider(cod) %>%
    tidyr::unnest_wider(COD_CI, names_sep = "_") %>%
    tidyr::unnest_wider(prd) %>%
    tidyr::unnest_wider(PRD_CI, names_sep = "_") %>%
    tidyr::unnest_wider(prb) %>%
    tidyr::unnest_wider(PRB_CI, names_sep = "_") %>%
    # Rename columns resulting from unnesting
    rename_with(~ gsub("%", "", gsub("\\.", "_", tolower(.x))))

  # Clean up the stats output (rename cols, relocate cols, etc.)
  df_stat %>%
    mutate(
      by_class = !is.null({{ class }}),
      geography_type = ifelse(
        !is.null({{ geography }}),
        ccao::vars_rename(
          rlang::as_string(rlang::ensym(geography)),
          names_from = "model",
          names_to = "athena"
        ),
        "triad_code"
      )
    ) %>%
    rename(any_of(col_dict)) %>%
    relocate(
      any_of(c("geography_type", "geography_id", "by_class", "class")),
      .after = "triad_code"
    ) %>%
    mutate(across(
      -(contains("_max") & contains("yoy")) & where(is.numeric),
      ~ replace(.x, !is.finite(.x), NA)
    ))
}


# Same as the gen_agg_stats function, but with different statistics and broken
# out by quantile
gen_agg_stats_quantile <- function(data, truth, estimate,
                                   rsn_col, rsf_col, triad, geography,
                                   class, col_dict, num_quantile) {
  # Calculate the median ratio by quantile of sale price, plus the upper and
  # lower bounds of each quantile
  df_quantile <- data %>%
    group_by({{ triad }}, {{ geography }}, {{ class }}) %>%
    mutate(quantile = ntile({{ truth }}, n = num_quantile)) %>%
    group_by({{ triad }}, {{ geography }}, {{ class }}, quantile) %>%
    summarize(
      num_sale = sum(!is.na({{ truth }})),
      median_ratio = median(({{ estimate }} / {{ truth }}), na.rm = TRUE),
      lower_bound = min({{ truth }}, na.rm = TRUE),
      upper_bound = max({{ truth }}, na.rm = TRUE),
      prior_near_yoy_pct_chg_median = median(
        ({{ estimate }} - {{ rsn_col }}) / {{ rsn_col }},
        na.rm = TRUE
      ),
      prior_far_yoy_pct_chg_median = median(
        ({{ estimate }} - {{ rsf_col }}) / {{ rsf_col }},
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    ungroup()

  # Clean up the quantile output
  df_quantile %>%
    mutate(
      num_quantile = num_quantile,
      by_class = !is.null({{ class }}),
      geography_type = ifelse(
        !is.null({{ geography }}),
        ccao::vars_rename(
          rlang::as_string(rlang::ensym(geography)),
          names_from = "model",
          names_to = "athena"
        ),
        "triad_code"
      )
    ) %>%
    filter(!is.na(quantile)) %>%
    rename(any_of(col_dict)) %>%
    relocate(
      any_of(c(
        "geography_type", "geography_id",
        "by_class", "class", "num_quantile"
      )),
      .after = "triad_code"
    ) %>%
    mutate(across(
      -(contains("_max") & contains("yoy")) & where(is.numeric),
      ~ replace(.x, !is.finite(.x), NA)
    ))
}




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4. Generate Stats ------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Use fancy tidyeval to create a list of all the geography levels with a
# class or no class option for each level
geographies_quosures <- c(
  rlang::parse_quos(params$ratio_study$geographies, env = .GlobalEnv),
  rlang::quo(NULL)
)
geographies_list <- tidyr::expand_grid(
  geographies_quosures,
  rlang::quos(meta_class, NULL)
) %>%
  as.list() %>%
  unname()

# Same as above, but add quantile breakouts to the grid expansion
geographies_list_quantile <- tidyr::expand_grid(
  geographies_quosures,
  rlang::quos(meta_class, NULL),
  params$ratio_study$num_quantile
) %>%
  as.list() %>%
  unname()


## 4.1. Test Set ---------------------------------------------------------------

# Use parallel map to calculate aggregate stats for every geography level and
# class combination for the test set
message("Calculating test set aggregate statistics")
future_pmap(
  geographies_list,
  function(geo, cls) {
    gen_agg_stats(
      data = test_data_card,
      truth = meta_sale_price,
      estimate = pred_card_initial_fmv,
      bldg_sqft = char_bldg_sf,
      rsn_col = prior_near_tot,
      rsf_col = prior_far_tot,
      triad = meta_triad_code,
      geography = !!geo,
      class = !!cls,
      col_dict = col_rename_dict
    )
  },
  .options = furrr_options(seed = TRUE, stdout = FALSE),
  .progress = FALSE
) %>%
  purrr::list_rbind() %>%
  write_parquet(paths$output$performance_test$local)

# Same as above, but calculate stats per quantile of sale price
message("Calculating test set quantile statistics")
future_pmap(
  geographies_list_quantile,
  function(geo, cls, qnt) {
    gen_agg_stats_quantile(
      data = test_data_card,
      truth = meta_sale_price,
      estimate = pred_card_initial_fmv,
      rsn_col = prior_near_tot,
      rsf_col = prior_far_tot,
      triad = meta_triad_code,
      geography = !!geo,
      class = !!cls,
      col_dict = col_rename_dict,
      num_quantile = qnt
    )
  },
  .options = furrr_options(seed = TRUE, stdout = FALSE),
  .progress = FALSE
) %>%
  purrr::list_rbind() %>%
  write_parquet(paths$output$performance_quantile_test$local)


## 4.2. Assessment Set ---------------------------------------------------------

# Do the same thing for the assessment set. This will have accurate property
# counts and proportions, since it also includes unsold properties
message("Calculating assessment set aggregate statistics")
future_pmap(
  geographies_list,
  function(geo, cls) {
    gen_agg_stats(
      data = assessment_data_pin,
      truth = sale_ratio_study_price,
      estimate = pred_pin_final_fmv_round,
      bldg_sqft = char_total_bldg_sf,
      rsn_col = prior_near_tot,
      rsf_col = prior_far_tot,
      triad = meta_triad_code,
      geography = !!geo,
      class = !!cls,
      col_dict = col_rename_dict
    )
  },
  .options = furrr_options(seed = TRUE, stdout = FALSE),
  .progress = FALSE
) %>%
  purrr::list_rbind() %>%
  write_parquet(paths$output$performance_assessment$local)

# Same as above, but calculate stats per quantile of sale price
message("Calculating assessment set quantile statistics")
future_pmap(
  geographies_list_quantile,
  function(geo, cls, qnt) {
    gen_agg_stats_quantile(
      data = assessment_data_pin,
      truth = sale_ratio_study_price,
      estimate = pred_pin_final_fmv_round,
      rsn_col = prior_near_tot,
      rsf_col = prior_far_tot,
      triad = meta_triad_code,
      geography = !!geo,
      class = !!cls,
      col_dict = col_rename_dict,
      num_quantile = qnt
    )
  },
  .options = furrr_options(seed = TRUE, stdout = FALSE),
  .progress = FALSE
) %>%
  purrr::list_rbind() %>%
  write_parquet(paths$output$performance_quantile_assessment$local)

# End the stage timer and write the time elapsed to a temporary file
tictoc::toc(log = TRUE)
bind_rows(tictoc::tic.log(format = FALSE)) %>%
  arrow::write_parquet(gsub("//*", "/", file.path(
    paths$intermediate$timing$local,
    "model_timing_evaluate.parquet"
  )))

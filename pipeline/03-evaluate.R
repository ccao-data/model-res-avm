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

# Enable parallel backend for generating stats faster
if (supportsMulticore()) {
  # Limit to half the available cores to avoid hogging resources
  plan(multicore, workers = ceiling(num_threads / 2))
} else {
  # Multisession performance begins to degrade beyond 5 workers
  plan(multisession, workers = 5)
}

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

# Helper function to return NA when sale sample size is too small
gte_n <- \(n_sales, min_n, fn, na_type) {
  if (sum(!is.na(n_sales)) >= min_n) {
    return(fn)
  } else {
    return(na_type)
  }
}

# Helper function to add triad code as geography ID if it's not already present
add_triad_code <- \(data) {
  if (!"geography_id" %in% colnames(data)) {
    data$geography_id <- data$triad_code
  }
  return(data)
}




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Define Stats Functions ----------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Function to take either test set results or assessment results and generate
# aggregate performance statistics for different levels of geography
gen_agg_stats <- function(data, truth, estimate, bldg_sqft,
                          rsn_col, rsf_col, triad, geography,
                          class, col_dict, min_n) {
  # List of summary stat/performance functions applied within summarize() below
  # Each function is listed on the right while the name of the function is on
  # the left
  rs_fns_list <- list(
    cod_no_sop = \(x, y) gte_n(y, 2, cod(x / y, na.rm = TRUE), NA_real_),
    prd_no_sop = \(x, y) gte_n(y, 2, prd(x, y, na.rm = TRUE), NA_real_),
    prb_no_sop = \(x, y) gte_n(y, 2, prb(x, y, na.rm = TRUE), NA_real_),
    mki_no_sop = \(x, y) gte_n(y, 2, mki(x, y, na.rm = TRUE), NA_real_),
    cod = \(x, y) gte_n(y, min_n, cod(x / y, na.rm = TRUE), NA_real_),
    cod_met = \(x, y) gte_n(y, min_n, cod_met(cod(x / y, na.rm = TRUE)), NA),
    prd = \(x, y) gte_n(y, min_n, prd(x, y, na.rm = TRUE), NA_real_),
    prd_met = \(x, y) gte_n(y, min_n, prd_met(prd(x, y, na.rm = TRUE)), NA),
    prb = \(x, y) gte_n(y, min_n, prb(x, y, na.rm = TRUE), NA_real_),
    prb_met = \(x, y) gte_n(y, min_n, prb_met(prb(x, y, na.rm = TRUE)), NA),
    mki = \(x, y) gte_n(y, min_n, mki(x, y, na.rm = TRUE), NA_real_),
    mki_met = \(x, y) gte_n(y, min_n, mki_met(mki(x, y, na.rm = TRUE)), NA)
  )
  ys_fns_list <- list(
    rmse        = rmse_vec,
    # Necessary because sometimes all sales in a group will be the same,
    # resulting in a std. dev. of 0 (and thus a warning)
    r_squared   = \(y, x) suppressWarnings(rsq_vec(y, x)),
    mae         = mae_vec,
    mpe         = mpe_vec,
    mape        = mape_vec,
    mdape       = mdape_vec # From R/helpers.R
  )
  sum_fns_list <- list(
    min         = \(x) min(x, na.rm = TRUE),
    q25         = \(x) quantile(x, na.rm = TRUE, probs = 0.25),
    median      = \(x) median(x, na.rm = TRUE),
    q75         = \(x) quantile(x, na.rm = TRUE, probs = 0.75),
    max         = \(x) max(x, na.rm = TRUE)
  )
  sum_sqft_fns_list <- list(
    min         = \(x, y) min(x / y, na.rm = TRUE),
    q25         = \(x, y) quantile(x / y, na.rm = TRUE, probs = 0.25),
    median      = \(x, y) median(x / y, na.rm = TRUE),
    q75         = \(x, y) quantile(x / y, na.rm = TRUE, probs = 0.75),
    max         = \(x, y) max(x / y, na.rm = TRUE)
  )
  yoy_fns_list <- list(
    min         = \(x, y) min((x - y) / y, na.rm = TRUE),
    q25         = \(x, y) quantile((x - y) / y, na.rm = TRUE, probs = 0.25),
    median      = \(x, y) median((x - y) / y, na.rm = TRUE),
    q75         = \(x, y) quantile((x - y) / y, na.rm = TRUE, probs = 0.75),
    max         = \(x, y) max((x - y) / y, na.rm = TRUE)
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

      # Assessment-specific ratio stats
      rs_lst = rs_fns_list %>%
        map(., \(f) exec(f, pmax({{ estimate }}, 1), {{ truth }})) %>%
        list(),
      median_ratio = median({{ estimate }} / {{ truth }}, na.rm = TRUE),

      # Yardstick (ML-specific) performance stats
      ys_lst = ys_fns_list %>%
        map(., \(f) gte_n({{ truth }}, 2, exec(f, {{ truth }}, {{ estimate }}), NA_real_)) %>% # nolint
        list(),

      # Summary stats of sale price and sale price per sqft
      sum_sale_lst = sum_fns_list %>%
        set_names(paste0("sale_fmv_", names(.))) %>%
        map(., \(f) suppressWarnings(exec(f, {{ truth }}))) %>%
        list(),
      sum_sale_sf_lst = sum_sqft_fns_list %>%
        set_names(paste0("sale_fmv_per_sqft_", names(.))) %>%
        map(., \(f) suppressWarnings(exec(f, {{ truth }}, {{ bldg_sqft }}))) %>%
        list(),

      # Summary stats of prior values and value per sqft
      prior_far_num_missing = sum(is.na({{ rsf_col }})),
      sum_rsf_lst = sum_fns_list %>%
        set_names(paste0("prior_far_fmv_", names(.))) %>%
        map(., \(f) suppressWarnings(exec(f, {{ rsf_col }}))) %>%
        list(),
      sum_rsf_sf_lst = sum_sqft_fns_list %>%
        set_names(paste0("prior_far_fmv_per_sqft_", names(.))) %>%
        map(., \(f) suppressWarnings(exec(f, {{ rsf_col }}, {{ bldg_sqft }}))) %>% # nolint
        list(),
      yoy_rsf_lst = yoy_fns_list %>%
        set_names(paste0("prior_far_yoy_pct_chg_", names(.))) %>%
        map(., \(f) suppressWarnings(exec(f, {{ estimate }}, {{ rsf_col }}))) %>% # nolint
        list(),
      prior_near_num_missing = sum(is.na({{ rsn_col }})),
      sum_rsn_lst = sum_fns_list %>%
        set_names(paste0("prior_near_fmv_", names(.))) %>%
        map(., \(f) exec(f, {{ rsn_col }})) %>%
        list(),
      sum_rsn_sf_lst = sum_sqft_fns_list %>%
        set_names(paste0("prior_near_fmv_per_sqft_", names(.))) %>%
        map(., \(f) exec(f, {{ rsn_col }}, {{ bldg_sqft }})) %>%
        list(),
      yoy_rsn_lst = yoy_fns_list %>%
        set_names(paste0("prior_near_yoy_pct_chg_", names(.))) %>%
        map(., \(f) exec(f, {{ estimate }}, {{ rsn_col }})) %>%
        list(),

      # Summary stats of estimate value and estimate per sqft
      estimate_num_missing = sum(is.na({{ estimate }})),
      sum_est_lst = sum_fns_list %>%
        set_names(paste0("estimate_fmv_", names(.))) %>%
        map(., \(f) exec(f, {{ estimate }})) %>%
        list(),
      sum_est_sf_lst = sum_sqft_fns_list %>%
        set_names(paste0("estimate_fmv_per_sqft_", names(.))) %>%
        map(., \(f) exec(f, {{ estimate }}, {{ bldg_sqft }})) %>%
        list(),
      .groups = "drop"
    ) %>%
    ungroup() %>%
    unnest_wider(ends_with("_lst"))

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
    )) %>%
    add_triad_code()
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
      # Suppress warnings resulting from groups of size 0 or 1
      lower_bound = suppressWarnings(min({{ truth }}, na.rm = TRUE)),
      upper_bound = suppressWarnings(max({{ truth }}, na.rm = TRUE)),
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
    )) %>%
    add_triad_code()
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

# Use parallel map to calculate aggregate stats for every geography level,
# class combination, and model type for the test set
message("Calculating test set aggregate statistics")
pwalk(
  list(
    rlang::quos(pred_card_initial_fmv, pred_card_initial_fmv_lin),
    list(
      paths$output$performance_test$local,
      paths$output$performance_test_linear$local
    )
  ),
  function(pred, path) {
    future_pmap(
      geographies_list,
      function(geo, cls) {
        gen_agg_stats(
          data = test_data_card,
          truth = meta_sale_price,
          estimate = !!pred,
          bldg_sqft = char_bldg_sf,
          rsn_col = prior_near_tot,
          rsf_col = prior_far_tot,
          triad = meta_triad_code,
          geography = !!geo,
          class = !!cls,
          col_dict = col_rename_dict,
          min_n = params$ratio_study$min_n_sales
        )
      },
      .options = furrr_options(seed = TRUE, stdout = FALSE),
      .progress = FALSE
    ) %>%
      purrr::list_rbind() %>%
      write_parquet(path)
  }
)

# Same as above, but calculate stats per quantile of sale price
message("Calculating test set quantile statistics")
pwalk(
  list(
    rlang::quos(pred_card_initial_fmv, pred_card_initial_fmv_lin),
    list(
      paths$output$performance_quantile_test$local,
      paths$output$performance_quantile_test_linear$local
    )
  ),
  function(pred, path) {
    future_pmap(
      geographies_list_quantile,
      function(geo, cls, qnt) {
        gen_agg_stats_quantile(
          data = test_data_card,
          truth = meta_sale_price,
          estimate = !!pred,
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
      write_parquet(path)
  }
)


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
      col_dict = col_rename_dict,
      min_n = params$ratio_study$min_n_sales
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

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

# Get the triad of the run to use for filtering
run_triad <- tools::toTitleCase(params$assessment$triad)
run_triad_code <- ccao::town_dict %>%
  filter(triad_name == run_triad) %>%
  distinct(triad_code) %>%
  pull(triad_code)

# Load geometry name dictionary and columns to use for renaming
geography_dict <- ccao::vars_dict %>%
  filter(var_name_model %in% params$ratio_study$geographies)
geography_cols <- geography_dict %>%
  distinct(var_name_athena) %>%
  pull()



#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. Load Data -----------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Loading evaluation data")

# Load the test results from the end of the train stage. This will be the most
# recent 10% of sales and already includes predictions. This data will NOT
# include multicard sales, so the unit of observation is PINs (1 PIN per row)
test_data_card <- setDT(read_parquet(paths$output$test_card$local)) %>%
  ccao::vars_rename(
    names_from = "model",
    names_to = "athena",
    dict = geography_dict
  )

# Pivot the test data from wide to long format, such that there is a sale and
# estimate for each geography specified by the geographies parameter
test_data_long <- melt(
  test_data_card,
  id.vars = c("meta_pin", "meta_card_num", "meta_sale_document_num"),
  measure.vars = geography_cols,
  variable.name = "geography_type",
  value.name = "geography_id"
)[
  test_data_card,
  `:=`(
    meta_sale_price = i.meta_sale_price,
    pred_pin_final_fmv_round = i.pred_card_initial_fmv,
    char_bldg_sf = i.char_bldg_sf,
    prior_near_tot = i.prior_near_tot,
    prior_far_tot = i.prior_far_tot,
    triad_code = i.meta_triad_code,
    class = i.meta_class
  ),
  on = c("meta_pin", "meta_card_num", "meta_sale_document_num")
][!is.na(meta_sale_price), ]

# Load the assessment results from the previous stage. This will include every
# residential PIN that needs a value. It WILL include multicard properties
# aggregated to the PIN-level. Only runs for full runs due to compute cost
if (run_type == "full") {
  assessment_data_pin <- read_parquet(paths$output$assessment_pin$local) %>%
    filter(meta_triad_code == run_triad_code) %>%
    select(
      meta_pin, meta_class, meta_triad_code, char_total_bldg_sf, prior_far_tot,
      prior_near_tot, pred_pin_final_fmv_round, sale_ratio_study_price,
      all_of(params$ratio_study$geographies)
    ) %>%
    ccao::vars_rename(
      names_from = "model",
      names_to = "athena",
      dict = geography_dict
    ) %>%
    setDT()

  assessment_data_long <- melt(
    assessment_data_pin,
    id.vars = "meta_pin",
    measure.vars = geography_cols,
    variable.name = "geography_type",
    value.name = "geography_id"
  )[
    assessment_data_pin,
    `:=`(
      meta_sale_price = i.sale_ratio_study_price,
      pred_pin_final_fmv_round = i.pred_pin_final_fmv_round,
      char_bldg_sf = i.char_total_bldg_sf,
      prior_near_tot = i.prior_near_tot,
      prior_far_tot = i.prior_far_tot,
      triad_code = i.meta_triad_code,
      class = i.meta_class
    ),
    on = "meta_pin"
  ][!is.na(meta_sale_price), ]
}




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Define Stats Functions ----------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Function to take either test set results or assessment results and generate
# aggregate performance statistics for different levels of geography
gen_agg_stats <- function(data, truth, estimate, bldg_sqft,
                          rsn_col, rsf_col, groups, stage) {
  # Fill for summary stats without enough sales to generate an output
  rs_fill <- list(NA_real_, c(NA_real_, NA_real_), NA, NA, NA_real_)

  # List of columns that summary stat functions expand to (from nested list)
  rs_cols <- sapply(
    c("cod", "prd", "prb"),
    \(x) paste0(x, "_", c("ci_2_5", "ci_97_5", "met", "ci_met", "n")),
    simplify = FALSE
  )

  # List of summary stat/performance functions applied within function below
  # Each function is on the right while the name of the function is on the left
  rs_fns_list <- list(
    # nolint start
    cod_no_sop = \(x, y) ifelse(sum(!is.na(y)) > 1, cod(x / y, na.rm = TRUE), NA_real_),
    prd_no_sop = \(x, y) ifelse(sum(!is.na(y)) > 1, prd(x, y, na.rm = TRUE), NA_real_),
    prb_no_sop = \(x, y) ifelse(sum(!is.na(y)) > 1, prb(x, y, na.rm = TRUE), NA_real_),
    cod = \(x, y) ifelse(sum(!is.na(y)) > 33, list(ccao_cod(x / y, na.rm = TRUE)), list(rs_fill)),
    prd = \(x, y) ifelse(sum(!is.na(y)) > 33, list(ccao_prd(x, y, na.rm = TRUE)), list(rs_fill)),
    prb = \(x, y) ifelse(sum(!is.na(y)) > 33, list(ccao_prb(x, y, na.rm = TRUE)), list(rs_fill))
    # nolint end
  )
  ys_fns_list <- list(
    rmse        = rmse_vec,
    r_squared   = rsq_vec,
    mae         = mae_vec,
    mpe         = mpe_vec,
    mape        = mape_vec
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

  # Generate aggregate performance stats by group
  df_stat <- as.data.table(data)[
    ,
    # Aggregate to get counts by geography without class
    `:=`(
      num_pin_no_class = .N,
      num_sale_no_class = sum(!is.na(get(truth)))
    ),
    by = eval(groups[groups != "class"])
  ][
    ,
    {
      # Variables that get re-used inside the brackets are specified here
      num_pin <- .N
      num_sale <- sum(!is.na(get(truth)))

      # Aggregate to get counts by geography and class
      # Basic summary stats, counts, proportions, etc
      purrr::list_flatten(list(
        num_pin = num_pin,
        num_sale = num_sale,
        pct_of_total_pin_by_class = num_pin /
          data.table::first(num_pin_no_class),
        pct_of_total_sale_by_class = num_sale /
          data.table::first(num_sale_no_class),
        pct_of_pin_sold = num_sale / num_pin,
        prior_far_total_av = sum(get(rsf_col) / 10, na.rm = TRUE),
        prior_near_total_av = sum(get(rsn_col) / 10, na.rm = TRUE),
        estimate_total_av = sum(get(estimate) / 10, na.rm = TRUE),

        # Assessment-specific statistics such as ratio stats
        imap(rs_fns_list, ~ exec(.x, get(estimate), get(truth))),
        median_ratio = median(get(estimate) / get(truth), na.rm = TRUE),

        # Yardstick (ML-specific) performance stats
        imap(ys_fns_list, ~ exec(.x, get(truth), get(estimate))),

        # Summary stats of sale price and sale price per sqft
        imap(sum_fns_list, ~ exec(.x, get(truth))) %>%
          purrr::set_names(paste0("sale_fmv_", names(.))),
        imap(sum_sqft_fns_list, ~ exec(.x, get(truth), get(bldg_sqft))) %>%
          purrr::set_names(paste0("sale_fmv_per_sqft_", names(.))),

        # Summary stats of prior values and value per sqft
        prior_far_num_missing = sum(is.na(get(rsf_col))),
        imap(sum_fns_list, ~ exec(.x, get(rsf_col))) %>%
          purrr::set_names(paste0("prior_far_fmv_", names(.))),
        imap(sum_sqft_fns_list, ~ exec(.x, get(rsf_col), get(bldg_sqft))) %>%
          purrr::set_names(paste0("prior_far_fmv_per_sqft_", names(.))),
        imap(yoy_fns_list, ~ exec(.x, get(estimate), get(rsf_col))) %>%
          purrr::set_names(paste0("prior_far_yoy_pct_chg_", names(.))),
        prior_near_num_missing = sum(is.na(get(rsn_col))),
        imap(sum_fns_list, ~ exec(.x, get(rsn_col))) %>%
          purrr::set_names(paste0("prior_near_fmv_", names(.))),
        imap(sum_sqft_fns_list, ~ exec(.x, get(rsn_col), get(bldg_sqft))) %>%
          purrr::set_names(paste0("prior_near_fmv_per_sqft_", names(.))),
        imap(yoy_fns_list, ~ exec(.x, get(estimate), get(rsn_col))) %>%
          purrr::set_names(paste0("prior_near_yoy_pct_chg_", names(.))),

        # Summary stats of estimate value and estimate per sqft
        estimate_num_missing = sum(is.na(get(estimate))),
        imap(sum_fns_list, ~ exec(.x, get(estimate))) %>%
          purrr::set_names(paste0("estimate_fmv_", names(.))),
        imap(sum_sqft_fns_list, ~ exec(.x, get(estimate), get(bldg_sqft))) %>%
          purrr::set_names(paste0("estimate_fmv_per_sqft_", names(.)))
      ))
    },
    by = eval(groups)
  ][
    ,
      # Unnest ratio stat results from list to separate columns
      c("cod", rs_cols$cod, "prd", rs_cols$prd, "prb", rs_cols$prb) :=
      c(data.table::transpose(lapply(cod, \(x) unlist(x))),
        data.table::transpose(lapply(prd, \(x) unlist(x))),
        data.table::transpose(lapply(prb, \(x) unlist(x)))
      )
  ]

  # Include the class column and an indicator for whether it was used
  grp_by_class = "class" %in% groups
  df_stat[, by_class := grp_by_class]
  setcolorder(df_stat, c(
    groups[groups != "class"], "by_class", if (grp_by_class) "class" else NULL
  ))

  # Move ratio stat expanded columns to after their respective summary stat
  for (x in c("cod", "prd", "prb")) dt_move_after(df_stat, rs_cols[[x]], x)

  # Convert columns to expected types (i.e. _met columns should be logical)
  met_cols <- grep(names(df_stat), pattern = "_met$", value = TRUE)
  df_stat[ , (met_cols) := lapply(.SD, as.logical), .SDcols = met_cols]

  # Replace infinite and NaN values with NA
  inf_cols <- names(df_stat)[!(
    grepl(names(df_stat), pattern = "_max$") &
    grepl(names(df_stat), pattern = "_yoy")) &
    sapply(df_stat, is.numeric)
  ]
  df_stat[
    ,
    (inf_cols) := lapply(.SD, \(x) replace(x, !is.finite(x), NA)),
    .SDcols = inf_cols
  ]

  # Add the run stage so we can parition the collected results by filename
  df_stat[, stage := stage]
  return(df_stat)
}


# # Same as the gen_agg_stats function, but with different statistics and broken
# # out by quantile
# gen_agg_stats_quantile <- function(data, truth, estimate,
#                                    rsn_col, rsf_col, triad, geography,
#                                    class, col_dict, num_quantile) {
#   # Calculate the median ratio by quantile of sale price, plus the upper and
#   # lower bounds of each quantile
#   df_quantile <- data %>%
#     group_by({{ triad }}, {{ geography }}, {{ class }}) %>%
#     mutate(quantile = ntile({{ truth }}, n = num_quantile)) %>%
#     group_by({{ triad }}, {{ geography }}, {{ class }}, quantile) %>%
#     summarize(
#       num_sale = sum(!is.na({{ truth }})),
#       median_ratio = median(({{ estimate }} / {{ truth }}), na.rm = TRUE),
#       lower_bound = min({{ truth }}, na.rm = TRUE),
#       upper_bound = max({{ truth }}, na.rm = TRUE),
#       prior_near_yoy_pct_chg_median = median(
#         ({{ estimate }} - {{ rsn_col }}) / {{ rsn_col }},
#         na.rm = TRUE
#       ),
#       prior_far_yoy_pct_chg_median = median(
#         ({{ estimate }} - {{ rsf_col }}) / {{ rsf_col }},
#         na.rm = TRUE
#       ),
#       .groups = "drop"
#     ) %>%
#     ungroup()
#
#   # Clean up the quantile output
#   df_quantile %>%
#     mutate(
#       num_quantile = num_quantile,
#       by_class = !is.null({{ class }}),
#       geography_type = ifelse(
#         !is.null({{ geography }}),
#         ccao::vars_rename(
#           rlang::as_string(rlang::ensym(geography)),
#           names_from = "model",
#           names_to = "athena"
#         ),
#         "triad_code"
#       )
#     ) %>%
#     filter(!is.na(quantile)) %>%
#     rename(any_of(col_dict)) %>%
#     relocate(
#       any_of(c(
#         "geography_type", "geography_id",
#         "by_class", "class", "num_quantile"
#       )),
#       .after = "triad_code"
#     ) %>%
#     mutate(across(
#       -(contains("_max") & contains("yoy")) & where(is.numeric),
#       ~ replace(.x, !is.finite(.x), NA)
#     ))
# }



#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4. Generate Stats ------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Enable parallel backend for generating stats more quickly
plan(multisession, workers = num_threads / 2)

# Create a list of the data and class combinations to iterate over
map_list <- list(
  data = list(rep(test_data_long, 2), rep(assessment_data_long, 2)),
  class = list("class", NULL, "class", NULL),
  stage = list(rep("test", 2), rep("assessment", 2))
)

## 4.1. Performance ------------------------------------------------------------

# Use parallel map to calculate aggregate stats for every geography level and
# class combination for both the test and assessment set
message("Calculating aggregate performance statistics")
tic()
performance <- future_pmap(
  map_list,
  function(data, class, stage) {
    gen_agg_stats(
      data = data,
      truth = "meta_sale_price",
      estimate = "pred_pin_final_fmv_round",
      bldg_sqft = "char_bldg_sf",
      rsn_col = "prior_near_tot",
      rsf_col = "prior_far_tot",
      groups = c("triad_code", "geography_type", "geography_id", class),
      stage = stage
    )
  },
  .options = furrr_options(seed = TRUE, stdout = FALSE),
  .progress = FALSE
)
toc()


## 4.2. Performance Quantile ---------------------------------------------------

# Same as above, but calculate stats per quantile of sale price
message("Calculating assessment set quantile statistics")
future_map_dfr(
  geographies_list_quantile,
  ~ gen_agg_stats_quantile(
    data = assessment_data_pin,
    truth = sale_ratio_study_price,
    estimate = pred_pin_final_fmv_round,
    rsn_col = prior_near_tot,
    rsf_col = prior_far_tot,
    triad = meta_triad_code,
    geography = !!.x[[1]],
    class = !!.x[[2]],
    col_dict = col_rename_dict,
    num_quantile = .x[[3]]
  ),
  .options = furrr_options(seed = TRUE, stdout = FALSE),
  .progress = FALSE
) %>%
  write_parquet(paths$output$performance_quantile_assessment$local)

# End the stage timer and write the time elapsed to a temporary file
tictoc::toc(log = TRUE)
bind_rows(tictoc::tic.log(format = FALSE)) %>%
  arrow::write_parquet(gsub("//*", "/", file.path(
    paths$intermediate$timing$local,
    "model_timing_evaluate.parquet"
  )))

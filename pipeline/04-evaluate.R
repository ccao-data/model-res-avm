# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Setup ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Start the script timer and clear logs from prior script
tictoc::tic.clearlog()
tictoc::tic("Evaluate")

# Load R libraries
library(arrow)
library(assessr)
library(ccao)
library(dplyr)
library(here)
library(furrr)
library(lightsnip)
library(purrr)
library(rlang)
library(recipes)
library(stringr)
library(tictoc)
library(tidyr)
library(yardstick)

# Enable parallel backend for generating stats more quickly
plan(multisession, workers = parallel::detectCores(logical = FALSE))

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

# Renaming dictionary for input columns. We want actual value of the column
# to become geography_id and the NAME of the column to become geography_name
col_rename_dict <- c(
  "triad_code" = "meta_triad_code",
  "class" = "meta_class",
  "geography_id" = "meta_township_code",
  "geography_id" = "meta_township_name",
  "geography_id" = "meta_nbhd_code",
  "geography_id" = "loc_cook_municipality_name",
  "geography_id" = "loc_chicago_ward_num",
  "geography_id" = "loc_census_puma_geoid",
  "geography_id" = "loc_census_tract_geoid",
  "geography_id" = "loc_school_elementary_district_geoid",
  "geography_id" = "loc_school_secondary_district_geoid",
  "geography_id" = "loc_school_unified_district_geoid"
)

rs_num_quantile <- as.integer(strsplit(Sys.getenv(
  "MODEL_RATIO_STUDY_NUM_QUANTILE", "3,5"
), ",")[[1]])



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Load Data ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load the test results from the end of 02-train.R. This will be the most recent
# 10% of sales and already includes predictions. This data will NOT include
# multicard sales. The unit of observation is improvements (1 imprv per row)
test_data <- read_parquet(paths$intermediate$test$local) %>%
  as_tibble()

# Load the assessment results from the previous stage. This will include every
# residential PIN that needs a value. It WILL include multicard properties. Only
# runs for local (non-CI) runs
if (interactive()) {
  assessment_data <- read_parquet(paths$intermediate$assessment$local) %>%
    as_tibble()
}




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Define Stats Functions ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Function to take either test set results or assessment results and generate
# aggregate performance statistics for different levels of geography
gen_agg_stats <- function(data, truth, estimate, bldg_sqft,
                          rsn_col, rsf_col, triad, geography, class, col_dict) {

  # List of summary stat/performance functions applied within summarize() below
  # Each function is listed on the right while the name of the function is on
  # the left
  rs_fns_list <- list(
    cod_no_sop = ~ ifelse(sum(!is.na(.y)) > 1, cod(.x/ .y, na.rm = T), NA),
    prd_no_sop = ~ ifelse(sum(!is.na(.y)) > 1, prd(.x, .y, na.rm = T), NA),
    prb_no_sop = ~ ifelse(sum(!is.na(.y)) > 1, prb(.x, .y, na.rm = T), NA),
    cod = ~ ifelse(sum(!is.na(.y)) > 33, list(ccao_cod(.x/ .y, na.rm = T)), NA),
    prd = ~ ifelse(sum(!is.na(.y)) > 33, list(ccao_prd(.x, .y, na.rm = T)), NA),
    prb = ~ ifelse(sum(!is.na(.y)) > 33, list(ccao_prb(.x, .y, na.rm = T)), NA)
  )
  ys_fns_list <- list(
    rmse        = rmse_vec,
    r_squared   = rsq_vec,
    mae         = mae_vec,
    mpe         = mpe_vec,
    mape        = mape_vec
  )
  sum_fns_list <- list(
    min         = ~ min(.x, na.rm = T),
    q25         = ~ quantile(.x, na.rm = T, probs = 0.25),
    median      = ~ median(.x, na.rm = T),
    q75         = ~ quantile(.x, na.rm = T, probs = 0.75),
    max         = ~ max(.x, na.rm = T)
  )
  sum_sqft_fns_list <- list(
    min         = ~ min(.x / .y, na.rm = T),
    q25         = ~ quantile(.x / .y, na.rm = T, probs = 0.25),
    median      = ~ median(.x / .y, na.rm = T),
    q75         = ~ quantile(.x / .y, na.rm = T, probs = 0.75),
    max         = ~ max(.x / .y, na.rm = T)
  )
  yoy_fns_list <- list(
    min         = ~ min((.x - .y) / .y, na.rm = T),
    q25         = ~ quantile((.x - .y) / .y, na.rm = T, probs = 0.25),
    median      = ~ median((.x - .y) / .y, na.rm = T),
    q75         = ~ quantile((.x - .y) / .y, na.rm = T, probs = 0.75),
    max         = ~ max((.x - .y) / .y, na.rm = T)
  )
  
  # Generate aggregate performance stats by group
  df_stat <- data %>%
    mutate(rsf_x10 = .[[rsf_col]] * 10, rsn_x10 = .[[rsn_col]] * 10) %>%
    
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
      pct_of_total_impr_by_class = num_pin / first(num_pin_no_class),
      pct_of_total_sale_by_class = num_sale / first(num_sale_no_class),
      pct_of_pin_sold =  num_sale / num_pin,
      
      prior_far_total_av = sum(rsf_x10 / 10, na.rm = TRUE),
      prior_near_total_av = sum(rsn_x10 / 10, na.rm = TRUE),
      estimate_total_av = sum({{ estimate }}, na.rm = TRUE),
      
      # Assessment-specific statistics
      across(.fns = rs_fns_list, {{ estimate }}, {{ truth }}, .names = "{.fn}"),

      # Yardstick (ML-specific) performance stats
      across(.fns = ys_fns_list, {{ truth }}, {{ estimate }}, .names = "{.fn}"),

      # Summary stats of sale price and sale price per sqft
      across(.fns = sum_fns_list, {{ truth }}, .names = "sale_fmv_{.fn}"),
      across(
        .fns = sum_sqft_fns_list, {{ truth }}, {{ bldg_sqft }},
        .names = "sale_fmv_per_sqft_{.fn}"
      ),

      # Summary stats of prior values and value per sqft. Need to multiply
      # by 10 first since pin history is in AV, not FMV
      prior_far_source_col = rsf_col,
      prior_far_num_missing = sum(is.na(rsf_x10)),
      across(.fns = sum_fns_list, rsf_x10, .names = "prior_far_fmv_{.fn}"),
      across(
        .fns = sum_sqft_fns_list, rsf_x10, {{ bldg_sqft }},
        .names = "prior_far_fmv_per_sqft_{.fn}"
      ),
      across(
        .fns = yoy_fns_list, {{ estimate }}, rsf_x10,
        .names = "prior_far_yoy_pct_chg_{.fn}"
      ),
      prior_near_source_col = rsn_col,
      prior_near_num_missing = sum(is.na(rsn_x10)),
      across(.fns = sum_fns_list, rsn_x10, .names = "prior_near_fmv_{.fn}"),
      across(
        .fns = sum_sqft_fns_list, rsn_x10, {{ bldg_sqft }},
        .names = "prior_near_fmv_per_sqft_{.fn}"
      ),
      across(
        .fns = yoy_fns_list, {{ estimate }}, rsn_x10,
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
      )
    ) %>%

    # COD, PRD, and PRB all output to a list. We can unnest each list to get
    # additional info for each stat (95% CI, sample count, etc)
    tidyr::unnest_wider(cod) %>%
    tidyr::unnest_wider(COD_CI, names_sep = "_") %>%
    tidyr::unnest_wider(prd) %>%
    tidyr::unnest_wider(PRD_CI, names_sep = "_") %>%
    tidyr::unnest_wider(prb) %>%
    tidyr::unnest_wider(PRB_CI, names_sep = "_") %>%

    # Rename columns resulting from unnesting
    rename_with(~ gsub("%", "", gsub("\\.", "_", tolower(.x)))) %>%
    ungroup()
  
  # Clean up the stats output (rename cols, relocate cols, etc.)
  df_stat %>%
    mutate(
      by_class = !is.null( {{ class }}),
      geography_type = ifelse(
        !is.null( {{ geography }}),
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
    mutate(rsf_x10 = .[[rsf_col]] * 10, rsn_x10 = .[[rsn_col]] * 10) %>%
    group_by({{ triad }}, {{ geography }}, {{ class }}) %>%
    mutate(quantile = ntile({{ truth }}, n = num_quantile)) %>%
    group_by({{ triad }}, {{ geography }}, {{ class }}, quantile) %>%
    summarize(
      num_sale = sum(!is.na({{ truth }})),
      median_ratio = median( ({{ estimate }} / {{ truth }}), na.rm = TRUE),
      lower_bound = min( {{ truth }}, na.rm = TRUE),
      upper_bound = max( {{ truth }}, na.rm = TRUE),
      prior_near_yoy_pct_chg_median = median(
        ({{ estimate }} - rsn_x10) / rsn_x10, na.rm = TRUE
      ),
      prior_far_yoy_pct_chg_median = median(
        ({{ estimate }} - rsf_x10) / rsf_x10, na.rm = TRUE
      )
    )
  
  # Clean up the quantile output
  df_quantile %>%
    mutate(
      num_quantile = num_quantile, 
      by_class = !is.null( {{ class }}),
      geography_type = ifelse(
        !is.null( {{ geography }}),
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




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Generate Stats ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Use fancy tidyeval to create a list of all the geography levels with a
# class or no class option for each level
geographies_quosures <- rlang::quos(
  meta_township_code,
  # meta_nbhd_code, loc_cook_municipality_name,
  # loc_chicago_ward_num, loc_census_puma_geoid, loc_census_tract_geoid,
  # loc_school_elementary_district_geoid, loc_school_secondary_district_geoid,
  loc_school_unified_district_geoid,
  NULL
)
geographies_list <- purrr::cross2(
  geographies_quosures,
  rlang::quos(meta_class, NULL)
)

# Same as above, but add quantile breakouts to the grid expansion
geographies_list_quantile <- purrr::cross3(
  geographies_quosures,
  rlang::quos(meta_class, NULL),
  rs_num_quantile
)


### Test set

# Use parallel map to calculate aggregate stats for every geography level and
# class combination for the test set
future_map_dfr(
  geographies_list,
  ~ gen_agg_stats(
    data = test_data,
    truth = meta_sale_price,
    estimate = initial_pred_fmv,
    bldg_sqft = char_bldg_sf,
    rsn_col = rsn_column,
    rsf_col = rsf_column,
    triad = meta_triad_code,
    geography = !!.x[[1]],
    class = !!.x[[2]],
    col_dict = col_rename_dict
  ),
  .options = furrr_options(seed = TRUE, stdout = FALSE),
  .progress = TRUE
) %>%
  write_parquet(paths$output$performance_test$local)

# Same as above, but calculate stats per quantile of sale price
future_map_dfr(
  geographies_list_quantile,
  ~ gen_agg_stats_quantile(
    data = test_data,
    truth = meta_sale_price,
    estimate = initial_pred_fmv,
    rsn_col = rsn_column,
    rsf_col = rsf_column,
    triad = meta_triad_code,
    geography = !!.x[[1]],
    class = !!.x[[2]],
    col_dict = col_rename_dict,
    num_quantile = .x[[3]]
  ),
  .options = furrr_options(seed = TRUE, stdout = FALSE),
  .progress = TRUE
) %>%
  write_parquet(paths$output$performance_quantile_test$local)


### Assessment set

# Only value the assessment set for non-CI (local) runs
if (interactive()) {
  
  # Do the same thing for the assessment set. This will have accurate property
  # counts and proportions, since it also has unsold properties
  future_map_dfr(
    geographies_list,
    ~ gen_agg_stats(
      data = assessment_data,
      truth = meta_sale_price,
      estimate = initial_pred_fmv,
      bldg_sqft = char_bldg_sf,
      rsn_col = rsn_column,
      rsf_col = rsf_column,
      triad = meta_triad_code,
      geography = !!.x[[1]],
      class = !!.x[[2]],
      col_dict = col_rename_dict
    ),
    .options = furrr_options(seed = TRUE, stdout = FALSE),
    .progress = TRUE
  ) %>%
    write_parquet(paths$output$performance_assessment$local)
  
  # Same as above, but calculate stats per quantile of sale price
  future_map_dfr(
    geographies_list_quantile,
    ~ gen_agg_stats_quantile(
      data = assessment_data,
      truth = meta_sale_price,
      estimate = initial_pred_fmv,
      rsn_col = rsn_column,
      rsf_col = rsf_column,
      triad = meta_triad_code,
      geography = !!.x[[1]],
      class = !!.x[[2]],
      col_dict = col_rename_dict,
      num_quantile = .x[[3]]
    ),
    .options = furrr_options(seed = TRUE, stdout = FALSE),
    .progress = TRUE
  ) %>%
    write_parquet(paths$output$performance_quantile_assessment$local)
}

# End the script timer and write the time elapsed to file
tictoc::toc(log = TRUE)
arrow::read_parquet(paths$output$timing$local) %>%
  bind_rows(., tictoc::tic.log(format = FALSE)) %>%
  arrow::write_parquet(paths$output$timing$local)

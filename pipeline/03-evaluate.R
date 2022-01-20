# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Setup ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Start full script timer
tictoc::tic(msg = "Evaluate test set")

# Load R libraries
library(arrow)
library(assessr)
library(ccao)
library(dplyr)
library(here)
library(purrr)
library(recipes)
library(stringr)
library(tictoc)
library(tidyr)
library(treesnip)
library(yardstick)

# Load helpers, recipes, and lightgbm parsnip bindings from files
walk(list.files("R", full.names = TRUE), source)

# Initialize a dictionary of file paths and URIs. See R/helpers.R
paths <- model_file_dict()

# Get year/stage to use for previous comparison
model_assessment_data_year <- Sys.getenv(
  "MODEL_ASSESSMENT_DATA_YEAR", unset = lubridate::year(Sys.Date())
)
model_ratio_study_year <- Sys.getenv("MODEL_RATIO_STUDY_YEAR", "2020")
model_ratio_study_stage <- Sys.getenv("MODEL_RATIO_STUDY_STAGE", "board")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Generate Stats ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load the test data created by 02-train.R. This will be the most recent 10%
# of sales and already includes predictions
test_data <- as_tibble(read_parquet(paths$output$test$local))

generate_performance_stat <- function(data,
                                      truth,
                                      estimate,
                                      bldg_sqft,
                                      ratio_study_source,
                                      ratio_study_col,
                                      triad,
                                      geography,
                                      class
                                      ) {

  # List of summary stat/performance functions applied within summarize()
  ys_fns_list <- list(
    rmse = rmse_vec, r_squared = rsq_vec, mae = mae_vec,
    mpe = mpe_vec, mape = mape_vec
  )
  sum_fns_list <- list(
    min =    ~ min(.x, na.rm = TRUE),
    q25 =    ~ quantile(.x, na.rm = TRUE, probs = 0.25),
    median = ~ median(.x, na.rm = TRUE),
    q75 =    ~ quantile(.x, na.rm = TRUE, probs = 0.75),
    max =    ~ max(.x, na.rm = TRUE)
  )
  sum_sqft_fns_list <- list(
    min =    ~ min(.x / .y, na.rm = TRUE),
    q25 =    ~ quantile(.x / .y, na.rm = TRUE, probs = 0.25),
    median = ~ median(.x / .y, na.rm = TRUE),
    q75 =    ~ quantile(.x / .y, na.rm = TRUE, probs = 0.75),
    max =    ~ max(.x / .y, na.rm = TRUE)
  )
  
  # Generate aggregate performance stats by group
  df_stat <- data %>%
    mutate(pv_x10 = .[[ratio_study_col]] * 10) %>%
    group_by({{ triad }}, {{ geography }}, {{ class }}) %>%
    summarize(
      cnt = n(),
      cod = ifelse(cnt >= 34, list(ccao_cod({{ estimate }} / {{ truth }})), NA),
      prd = ifelse(cnt >= 34, list(ccao_prd({{ estimate }}, {{ truth }})), NA),
      prb = ifelse(cnt >= 34, list(ccao_prb({{ estimate }}, {{ truth }})), NA),

      # Yardstick (ML-specific) performance stats
      across(.fns = ys_fns_list, {{ truth }}, {{ estimate }}, .names = "{.fn}"),

      # Summary stats of sale price and sale price per sqft
      across(.fns = sum_fns_list, {{ truth }}, .names = "sale_{.fn}"),
      across(
        .fns = sum_sqft_fns_list, {{ truth }}, {{ bldg_sqft }},
        .names = "sale_per_sqft_{.fn}"
      ),
      sale_num_missing = sum(is.na({{ truth }})),

      # Summary stats of prior year value and value per sqft. Need to multiply
      # by 10 first since pin history is in AV, not FMV
      prior_val_source = ratio_study_source,
      across(.fns = sum_fns_list, pv_x10, .names = "prior_val_{.fn}"),
      across(
        .fns = sum_sqft_fns_list, pv_x10, {{ bldg_sqft }},
        .names = "prior_val_per_sqft_{.fn}"
      ),
      prior_val_num_missing = sum(is.na(pv_x10)),

      # Summary stats of estimate value and estimate per sqft
      across(.fns = sum_fns_list, {{ estimate }}, .names = "estimate_{.fn}"),
      across(
        .fns = sum_sqft_fns_list, {{ estimate }}, {{ bldg_sqft }},
        .names = "estimate_per_sqft_{.fn}"
      ),
      estimate_num_missing = sum(is.na({{ estimate }}))
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
    rename_with(~ gsub("%", "", gsub("\\.", "_", tolower(.x))))
  
  # Calculate the median ratio by ntile of sale price, plus the upper and lower
  # bounds of each ntile
  df_ntile <- data %>%
    group_by({{ triad }}, {{ geography }}, {{ class }}) %>%
    mutate(ntile = ntile({{ truth }}, n = 5)) %>%
    group_by({{ triad }}, {{ geography }}, {{ class }}, ntile) %>%
    summarize(
      median_ratio = median( ({{ estimate }} / {{ truth }}), na.rm = TRUE),
      lower_bound = min( {{ truth }}, na.rm = TRUE),
      upper_bound = max( {{ truth }}, na.rm = TRUE)
    ) %>%
    pivot_wider(
      names_from = ntile,
      names_glue = "q{ntile}_{.value}",
      values_from = c(median_ratio, lower_bound, upper_bound)
    )
  
  # Renaming dictionary for input columns
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
  
  # Combine both data sets and add identification variables
  df_stat %>%
    left_join(df_ntile) %>%
    ungroup() %>%
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
    rename(any_of(col_rename_dict)) %>%
    relocate(
      any_of(c("geography_type", "geography_id", "by_class", "class")),
      .after = "triad_code"
    )
}


study_source <- str_to_title(paste(
  model_ratio_study_year,
  model_ratio_study_stage
))
study_col <- get_rs_col_name(
  model_assessment_data_year,
  model_ratio_study_year,
  model_ratio_study_stage
)

temp <- future_map_dfr(
  quos(meta_township_code, meta_nbhd_code, loc_cook_municipality_name,
       loc_chicago_ward_num, loc_census_puma_geoid, loc_census_tract_geoid,
       loc_school_elementary_district_geoid, loc_school_secondary_district_geoid,
       loc_school_unified_district_geoid, NULL),
  ~ generate_performance_stat(
      data = test_data,
      truth = meta_sale_price,
      estimate = lgbm,
      bldg_sqft = char_bldg_sf,
      ratio_study_source = study_source,
      ratio_study_col = study_col,
      triad = meta_triad_code,
      geography = !!.x,
      class = NULL
    )
  )



# End the script timer
tictoc::toc(log = TRUE)

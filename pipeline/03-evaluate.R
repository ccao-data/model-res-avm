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
library(furrr)
library(purrr)
library(rlang)
library(recipes)
library(stringr)
library(tictoc)
library(tidyr)
library(treesnip)
library(yardstick)

# Enable parallel backend for generating stats more quickly
plan(multisession, workers = parallel::detectCores(logical = FALSE))

# Load helpers, recipes, and lightgbm parsnip bindings from files
walk(list.files("R", full.names = TRUE), source)

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
##### Load Data ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

### Test set

# Load the test results at the end of 02-train.R. This will be the most recent
# 10% of sales and already includes predictions. This data will NOT include
# multicard sales. The unit of observation is improvements (1 imprv per row)
test_data <- read_parquet(paths$output$test$local) %>% as_tibble()


### Assessment set

# Load the final lightgbm model object and recipe from file
lgbm_final_full_fit <- ccao::model_lgbm_load(paths$output$workflow$fit$local)
lgbm_final_full_recipe <- readRDS(paths$output$workflow$recipe$local)

# Load the MOST RECENT sale per PIN for the same year as the assessment data. We
# want our assessed value to be as close to the most recent sale as possible
training_data <- read_parquet(paths$input$training$local) %>%
  filter(meta_year == model_assessment_data_year) %>%
  group_by(meta_pin) %>%
  filter(meta_sale_date == max(meta_sale_date)) %>%
  distinct(
    meta_pin, meta_year, meta_sale_price,
    meta_sale_date, meta_sale_document_num
  ) %>%
  ungroup()

# Load the data for assessment. This is the universe of IMPROVEMENTs (not PINs)
# that needs values. Use the trained lightgbm model to estimate a value
assessment_data_pred <- read_parquet(paths$input$assessment$local) %>%
  as_tibble() %>%
  mutate(
    lgbm = model_predict(
      spec = lgbm_final_full_fit,
      recipe = lgbm_final_full_recipe,
      data = .
    )
  )

# Join sales data to each PIN, then collapse the improvement-level assessment
# data to the PIN level, summing the predicted value for multicard PINs. Keep
# only columns needed for performance calculations
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
    lgbm = sum(lgbm),
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
  ungroup()

  


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Define Stats Function ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Function to take either test set results or assessment results and generate
# aggregate performance statistics for different levels of geography
gen_agg_stats <- function(data, truth, estimate, bldg_sqft,
                          rsn_col, rsf_col, triad, geography, class) {

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
      across(.fns = sum_fns_list, {{ estimate }}, .names = "estimate_fmv_{.fn}"),
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
    rename_with(~ gsub("%", "", gsub("\\.", "_", tolower(.x))))
  
  # Calculate the median ratio by ntile of sale price, plus the upper and lower
  # bounds of each ntile
  df_ntile <- data %>%
    mutate(rsf_x10 = .[[rsf_col]] * 10, rsn_x10 = .[[rsn_col]] * 10) %>%
    group_by({{ triad }}, {{ geography }}, {{ class }}) %>%
    mutate(ntile = ntile({{ truth }}, n = 5)) %>%
    group_by({{ triad }}, {{ geography }}, {{ class }}, ntile) %>%
    summarize(
      median_ratio = median( ({{ estimate }} / {{ truth }}), na.rm = TRUE),
      lower_bound = min( {{ truth }}, na.rm = TRUE),
      upper_bound = max( {{ truth }}, na.rm = TRUE),
      prior_near_yoy_pct_chg_median = median(
        ({{ estimate }} - rsn_x10) / rsn_x10, na.rm = TRUE
      ),
      prior_far_yoy_pct_chg_median = median(
        ({{ estimate }} - rsf_x10) / rsf_x10, na.rm = TRUE
      )
    ) %>%
    pivot_wider(
      names_from = ntile,
      names_glue = "q{ntile}_{.value}",
      values_from = c(
        median_ratio, lower_bound, upper_bound,
        prior_near_yoy_pct_chg_median, prior_far_yoy_pct_chg_median
      )
    )
  
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
  
  # Combine agg stats data with ntile data and add identification variables
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




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Generate Stats ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Use fancy tidyeval to create a list of all the geography levels with a
# class or no class option for each level
geographies_list <- purrr::cross2(
  rlang::quos(
    meta_township_code,
    meta_nbhd_code,
    loc_cook_municipality_name,
    loc_chicago_ward_num, loc_census_puma_geoid, loc_census_tract_geoid,
    loc_school_elementary_district_geoid, loc_school_secondary_district_geoid,
    loc_school_unified_district_geoid,
    NULL
  ),
  rlang::quos(
    meta_class,
    NULL
  )
)


### Test set

# Use parallel map to calculate aggregate stats for every geography level and
# class combination for the test set
test_performance <- future_map_dfr(
  geographies_list,
  ~ gen_agg_stats(
    data = test_data,
    truth = meta_sale_price,
    estimate = lgbm,
    bldg_sqft = char_bldg_sf,
    rsn_col = rsn_column,
    rsf_col = rsf_column,
    triad = meta_triad_code,
    geography = !!.x[[1]],
    class = !!.x[[2]]
  ),
  .options = furrr_options(seed = TRUE, stdout = FALSE),
  .progress = TRUE
)

# Save test set performance to file
write_parquet(
  test_performance,
  paths$output$performance$test$local
)


### Assessment set

# Do the same thing for the assessment set. This will have accurate property
# counts and proportions, since it also has unsold properties
assessment_performance <- future_map_dfr(
  geographies_list,
  ~ gen_agg_stats(
    data = assessment_data,
    truth = meta_sale_price,
    estimate = lgbm,
    bldg_sqft = char_bldg_sf,
    rsn_col = rsn_column,
    rsf_col = rsf_column,
    triad = meta_triad_code,
    geography = !!.x[[1]],
    class = !!.x[[2]]
  ),
  .options = furrr_options(seed = TRUE, stdout = FALSE),
  .progress = TRUE
)

# Save assessment set performance to file
write_parquet(
  assessment_performance,
  paths$output$performance$assessment$local
)

# End the script timer
tictoc::toc(log = TRUE)

#–– PARAMETERS ––#
run_id_val   <- "2025-05-30-exciting-dan"
grouping_col <- "char_bldg_sf"

# 2025-02-11-charming-eric


#–– SETUP ––#
tictoc::tic.clearlog()
tictoc::tic("Ingest")

purrr::walk(list.files("R/", "\\.R$", full.names = TRUE), source)

suppressPackageStartupMessages({
  library(DBI)
  library(igraph)
  library(noctua)
  library(glue)
  library(dplyr)
})

noctua_options(unload = TRUE)

AWS_ATHENA_CONN_NOCTUA <- dbConnect(
  noctua::athena(),
  rstudio_conn_tab = FALSE
)


#–– LOAD DATA WITH PARAMETERIZED run_id ––#
data <- dbGetQuery(
  conn = AWS_ATHENA_CONN_NOCTUA,
  glue("
    SELECT
      v.meta_tieback_key_pin,
      card.meta_pin,
      card.meta_card_num,
      card.char_bldg_sf,
      card.char_land_sf,
      v.meta_tieback_proration_rate,
      card.pred_card_initial_fmv   AS pred_card_initial_fmv,
      card.pred_card_final_fmv     AS pred_card_final_fmv,
      pin.pred_pin_initial_fmv     AS pred_pin_initial_fmv,
      pin.pred_pin_final_fmv       AS pred_pin_final_fmv
    FROM model.assessment_card AS card
    JOIN model.vw_card_res_input AS v
      ON CAST(card.meta_card_num AS VARCHAR) = CAST(v.meta_card_num AS VARCHAR)
      AND CAST(card.meta_pin        AS VARCHAR) = v.meta_pin
      AND v.year = '2025'
    JOIN model.assessment_pin AS pin
      ON card.meta_pin = pin.meta_pin
      AND card.run_id  = pin.run_id
    WHERE
      v.meta_tieback_key_pin IS NOT NULL
      AND card.run_id       = '{run_id_val}'
  ")
)

data <- data %>%
  group_by(meta_pin) %>%
  mutate(cards_per_pin = n_distinct(meta_card_num)) %>%
  ungroup()

group_cols <- c("meta_tieback_key_pin", grouping_col, "meta_card_num")


#–– TEST 1: pred_pin_initial_fmv – all values in the group must match (n_distinct == 1) ––#
test <- data %>%
  group_by(across(all_of(group_cols))) %>%
  filter(n_distinct(pred_pin_initial_fmv) != 1) %>%
  mutate(group_label = cur_group_id()) %>%
  ungroup()


#–– TEST 2: pred_card_initial_fmv – all values in the group must match ––#
test_1 <- data %>%
  group_by(across(all_of(group_cols))) %>%
  filter(n_distinct(pred_card_initial_fmv) != 1) %>%
  mutate(group_label = cur_group_id()) %>%
  ungroup()


#–– TEST 3: pred_card_final_fmv – all values in the group must match ––#
test_2 <- data %>%
  group_by(across(all_of(group_cols))) %>%
  filter(n_distinct(pred_card_final_fmv) != 1) %>%
  mutate(group_label = cur_group_id()) %>%
  ungroup()


#–– TEST 4: pred_pin_final_fmv – keep groups where values do NOT all match (n_distinct != 1) ––#
test_3 <- data %>%
  group_by(across(all_of(group_cols))) %>%
  filter(n_distinct(pred_pin_final_fmv) != 1) %>%
  mutate(group_label = cur_group_id()) %>%
  ungroup()


tictoc::toc()


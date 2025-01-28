#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Setup ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# NOTE: See DESCRIPTION for library dependencies and R/setup.R for
# variables used in each pipeline stage

# Start the stage timer and clear logs from prior stage
tictoc::tic.clearlog()
tictoc::tic("Finalize")

# Load libraries, helpers, and recipes from files
purrr::walk(list.files("R/", "\\.R$", full.names = TRUE), source)

# Unset the seed from setup.R to ensure a random run ID
set.seed(NULL)



#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. Save Metadata -------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Saving run metadata")

## 2.1. Run Info ---------------------------------------------------------------

# Generate a random identifier for this run. This will serve as the primary key/
# identifier for in perpetuity. See ?ccao_generate_id for details
run_id <- ccao::ccao_generate_id()

# Get the current timestamp for when the run ended
run_end_timestamp <- lubridate::now()

# Get the commit of the current reference
git_commit <- git2r::revparse_single(git2r::repository(), "HEAD")


## 2.2. DVC Hashes -------------------------------------------------------------

# Read the MD5 hash of each input dataset. These are created by DVC and used to
# version and share the input data
dvc_md5_df <- bind_rows(read_yaml("dvc.lock")$stages$ingest$outs) %>%
  mutate(path = paste0("dvc_md5_", gsub("input/|.parquet", "", path))) %>%
  select(path, md5) %>%
  pivot_wider(names_from = path, values_from = md5)


## 2.3. Parameters -------------------------------------------------------------

# Save most parameters from params.yaml to a metadata file, along with
# run info, git stuff, etc.
metadata <- tibble::tibble(
  run_id = run_id,
  run_end_timestamp = run_end_timestamp,
  run_type = run_type,
  run_note = run_note,
  git_sha_short = substr(git_commit$sha, 1, 8),
  git_sha_long = git_commit$sha,
  git_message = gsub("\n", "", git_commit$message),
  git_author = git_commit$author$name,
  git_email = git_commit$author$email,
  assessment_year = params$assessment$year,
  assessment_date = params$assessment$date,
  assessment_triad = params$assessment$triad,
  assessment_group = params$assessment$group,
  assessment_data_year = params$assessment$data_year,
  input_min_sale_year = params$input$min_sale_year,
  input_max_sale_year = params$input$max_sale_year,
  input_n_years_prior = params$input$n_years_prior,
  input_complex_match_exact = list(params$input$complex$match_exact),
  input_complex_match_fuzzy_name = list(
    names(params$input$complex$match_fuzzy)
  ),
  input_complex_match_fuzzy_value = list(
    as.numeric(params$input$complex$match_fuzzy)
  ),
  input_sale_validation_stat_groups = list(
    params$input$sale_validation$stat_groups
  ),
  input_sale_validation_iso_forest = list(
    params$input$sale_validation$iso_forest
  ),
  input_sale_validation_dev_bounds = list(
    params$input$sale_validation$dev_bounds
  ),
  ratio_study_far_year = params$ratio_study$far_year,
  ratio_study_far_stage = params$ratio_study$far_stage,
  ratio_study_far_column = params$ratio_study$far_column,
  ratio_study_near_year = params$ratio_study$near_year,
  ratio_study_near_stage = params$ratio_study$near_stage,
  ratio_study_near_column = params$ratio_study$near_column,
  ratio_study_num_quantile = list(params$ratio_study$num_quantile),
  shap_enable = shap_enable,
  comp_enable = comp_enable,
  comp_num_comps = params$comp$num_comps,
  cv_enable = cv_enable,
  cv_num_folds = params$cv$num_folds,
  cv_fold_overlap = params$cv$fold_overlap,
  cv_initial_set = params$cv$initial_set,
  cv_max_iterations = params$cv$max_iterations,
  cv_no_improve = params$cv$no_improve,
  cv_split_prop = params$cv$split_prop,
  cv_best_metric = params$cv$best_metric,
  pv_land_pct_of_total_cap = params$pv$land_pct_of_total_cap,
  pv_round_break = list(params$pv$round_break),
  pv_round_to_nearest = list(params$pv$round_to_nearest),
  pv_round_type = params$pv$round_type,
  model_predictor_id_count = length(params$model$predictor$id),
  model_predictor_id_name = list(params$model$predictor$id),
  model_predictor_all_count = length(params$model$predictor$all),
  model_predictor_all_name = list(params$model$predictor$all),
  model_predictor_categorical_count =
    length(params$model$predictor$categorical),
  model_predictor_categorical_name = list(params$model$predictor$categorical)
) %>%
  bind_cols(dvc_md5_df) %>%
  relocate(
    starts_with("dvc_id_"),
    .after = "input_complex_match_fuzzy_value"
  ) %>%
  arrow::write_parquet(paths$output$metadata$local)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Generate reports ----------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

## 3.1. Performance Report -----------------------------------------------------

# Wrap this block in an error handler so that the pipeline continues execution
# even if report generation fails. This is important because the report file is
# defined separately, so this script can't be sure that it is error-free
tryCatch(
  {
    suppressPackageStartupMessages({
      library(quarto)
    })

    message("Generating performance report")

    here("reports", "performance", "performance.qmd") %>%
      quarto_render(
        execute_params = list(
          run_id = run_id,
          year = params$assessment$year
        )
      )
  },
  error = function(func) {
    message("Encountered error during report generation:")
    message(conditionMessage(func))

    # Save an empty report so that this pipeline step produces the required
    # output even in cases of failure
    message("Saving an empty report file in order to continue execution")
    sink(paths$output$report_performance$local)
    cat("Encountered error in report generation:\n\n")
    cat(conditionMessage(func))
    sink()
  }
)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4. Save Timings --------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Saving run timings")

# End the stage timer and write the time elapsed to a temporary file
tictoc::toc(log = TRUE)
bind_rows(tictoc::tic.log(format = FALSE)) %>%
  arrow::write_parquet(gsub("//*", "/", file.path(
    paths$intermediate$timing$local,
    "model_timing_finalize.parquet"
  )))

# Load the intermediate timing logs
timings <- list.files(
  paste0(paths$intermediate$timing, "/"),
  full.names = TRUE
)

# Convert the intermediate timing logs to a wide data frame, then save to file
timings_df <- purrr::map_dfr(timings, read_parquet) %>%
  mutate(
    run_id = !!run_id,
    run_end_timestamp = run_end_timestamp,
    elapsed = round(toc - tic, 2),
    stage = paste0(tolower(stringr::word(msg, 1)), "_sec_elapsed"),
    order = recode(
      msg,
      "Train" = "01", "Assess" = "02", "Evaluate" = "03",
      "Interpret" = "04", "Finalize" = "05"
    )
  ) %>%
  arrange(order) %>%
  select(-c(tic:toc, msg)) %>%
  tidyr::pivot_wider(
    id_cols = c(run_id, run_end_timestamp),
    names_from = stage,
    values_from = elapsed
  ) %>%
  mutate(overall_sec_elapsed = rowSums(across(ends_with("_sec_elapsed")))) %>%
  mutate(across(ends_with("_sec_elapsed"), function(x) round(x, 2))) %>%
  write_parquet(paths$output$timing$local)

# Clear any remaining logs from tictoc
tictoc::tic.clearlog()

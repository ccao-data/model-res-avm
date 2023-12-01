#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Setup ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load libraries and scripts
suppressPackageStartupMessages({
  library(arrow)
  library(ccao)
  library(dplyr)
  library(here)
  library(lubridate)
  library(purrr)
  library(tidyr)
  library(tune)
  library(yaml)
})
source(here("R", "helpers.R"))

# Initialize a dictionary of file paths. See misc/file_dict.csv for details
paths <- model_file_dict()

# Load the parameters file containing the run settings
params <- read_yaml("params.yaml")

# Override CV toggle, SHAP toggle, and run_type, used for CI or limited runs
cv_enable <- as.logical(
  Sys.getenv("CV_ENABLE_OVERRIDE", unset = params$toggle$cv_enable)
)
shap_enable <- as.logical(
  Sys.getenv("SHAP_ENABLE_OVERRIDE", unset = params$toggle$shap_enable)
)
run_type <- as.character(
  Sys.getenv("RUN_TYPE_OVERRIDE", unset = params$run_type)
)




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

# For full runs, use the run note included in params.yaml, otherwise use the
# commit message
if (run_type == "full") {
  run_note <- params$run_note
} else {
  run_note <- gsub("\n", "", git_commit$message)
}


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
  cv_enable = cv_enable,
  cv_num_folds = params$cv$num_folds,
  cv_initial_set = params$cv$initial_set,
  cv_max_iterations = params$cv$max_iterations,
  cv_no_improve = params$cv$no_improve,
  cv_split_prop = params$cv$split_prop,
  cv_best_metric = params$cv$best_metric,
  pv_multicard_yoy_cap = params$pv$multicard_yoy_cap,
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
# 3. Save Timings --------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Saving run timings")

# Filter ensure we only get timing files for stages that actually ran
if (run_type == "full") {
  timings <- list.files(
    paste0(paths$intermediate$timing, "/"),
    full.names = TRUE
  )
} else {
  timings <- list.files(
    paste0(paths$intermediate$timing, "/"),
    pattern = "train|evaluate",
    full.names = TRUE
  )
}

# Convert the intermediate timing logs to a wide data frame, then save to file
timings_df <- purrr::map_dfr(timings, read_parquet) %>%
  mutate(
    run_id = run_id,
    run_end_timestamp = run_end_timestamp,
    elapsed = round(toc - tic, 2),
    stage = paste0(tolower(stringr::word(msg, 1)), "_sec_elapsed"),
    order = recode(
      msg,
      "Train" = "01", "Assess" = "02",
      "Evaluate" = "03", "Interpret" = "04"
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




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4. Generate performance report -----------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Wrap this block in an error handler so that the pipeline continues execution
# even if report generation fails. This is important because the report file is
# defined separately, so this script can't be sure that it is error-free, and
#
tryCatch(
  {
    suppressPackageStartupMessages({
      library(quarto)
    })

    message("Generating performance report")

    here("reports", "performance.qmd") %>%
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
    sink(here("reports", "performance.html"))
    cat("Encountered error in report generation:\n\n")
    cat(conditionMessage(func))
    sink()
  }
)

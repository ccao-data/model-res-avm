# Script that calculates and uploads comps for an existing model based on its
# run ID.
#
# The script expects one positional argument representing the existing model
# run ID to fetch.

# Load libraries, helpers, and recipes from files
purrr::walk(list.files("R/", "\\.R$", full.names = TRUE), source)

run_id <- commandArgs(trailingOnly = TRUE)
stopifnot(
  "Script must have exactly one argument, a run ID" = (length(run_id) == 1)
)

print(glue("Querying metadata for model run {run_id}"))
conn <- dbConnect(noctua::athena(), rstudio_conn_tab = FALSE)
metadata <- dbGetQuery(
  conn,
  glue_sql(
    "
    SELECT
      year,
      assessment_triad,
      comp_num_comps,
      dvc_md5_assessment_data,
      dvc_md5_training_data
    FROM model.metadata
    WHERE run_id = {run_id}
    -- Query should always return <= 1 row, but use a limit to be extra sure
    LIMIT 1
    ",
    .con = conn
  )
)
stopifnot(
  "Run ID does not exist in the data lake" = (nrow(metadata) == 1)
)

print("Fetching training and assessment data")
assessment_data <- download_dvc_data(metadata$dvc_md5_assessment_data)
training_data <- download_dvc_data(metadata$dvc_md5_training_data)

print("Fetching model run artifacts")
model_fetch_run(run_id = run_id, year = year)

print("Loading model fit and recipe from file")
lgbm_final_full_fit <- lightsnip::lgbm_load(paths$output$workflow_fit$local)
lgbm_final_full_recipe <- readRDS(paths$output$workflow_recipe$local)

print("Calculating comps")
comps <- generate_comps(
  assessment_data = assessment_data,
  training_data = training_data,
  model = lgbm_final_full_fit$fit,
  recipe = lgbm_final_full_recipe,
  triad = metadata$assessment_triad,
  num_comps = comp_num_comps
)

print("Uploading comps")
upload_comps(
  comps = comps,
  s3_uri = paths$output$comp$s3,
  run_id = run_id,
  year = year
)

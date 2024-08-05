base_paths <- model_file_dict(params$run_id, params$run_id_year)
comparison_paths <- model_file_dict(
  params$comparison_run_id,
  params$comparison_run_id_year
)
run_id <- params$run_id
comparison_run_id <- params$comparison_run_id

analyses_paths <- list(
  output = list(
    list(
      s3 = base_paths$output$assessment_card$s3,
      key = "assessment_card"
    ),
    list(
      s3 = base_paths$output$assessment_pin$s3,
      key = "assessment_pin"
    ),
    list(
      s3 = base_paths$output$metadata$s3,
      key = "metadata"
    ),
    list(
      s3 = base_paths$output$performance_test$s3,
      key = "performance_test"
    ),
    list(
      s3 = base_paths$output$shap$s3,
      key = "shap"
    )
  )
)

source("analyses/Ingest_script.qmd")
data_new <- model_fetch_run_subset(
  params$run_id,
  params$run_id_year,
  analyses_paths, TRUE
)

list2env(data_new, envir = .GlobalEnv)

rm(data_new)

comparison_paths <- list(
  output = list(
    list(
      s3 = base_paths$output$assessment_card$s3,
      key = "assessment_card"
    ),
    list(
      s3 = base_paths$output$assessment_pin$s3,
      key = "assessment_pin"
    ),
    list(
      s3 = base_paths$output$metadata$s3,
      key = "metadata"
    ),
    list(
      s3 = base_paths$output$performance_test$s3,
      key = "performance_test"
    ),
    list(
      s3 = base_paths$output$shap$s3,
      key = "shap"
    )
  )
)

data_comparison <- model_fetch_run_subset(
  params$comparison_run_id,
  params$comparison_run_id_year,
  comparison_paths, TRUE
)

list2env(data_comparison, envir = .GlobalEnv)

rm(data_comparison)

all_vars <- ls()

rename_var <- function(var_name, suffix, new_suffix) {
  if (exists(var_name) && is.data.frame(get(var_name))) {
    if (grepl(paste0("_", suffix, "$"), var_name)) {
      new_name <- sub(paste0("_", suffix, "$"), new_suffix, var_name)
      assign(new_name, get(var_name), envir = .GlobalEnv)
      rm(list = var_name, envir = .GlobalEnv)
    }
  }
}

# Iterate over all variables and rename if necessary
for (var_name in all_vars) {
  rename_var(var_name, params$run_id, "_new")
  rename_var(var_name, params$comparison_run_id, "_comparison")
}


lockfile_assessment <- metadata_new$dvc_md5_assessment_data

# Define S3 paths for assessment' data
s3_path_assessment <- paste0(
  "s3://ccao-data-dvc-us-east-1/files/md5/",
  substr(lockfile_assessment, 1, 2), "/",
  substr(lockfile_assessment, 3, nchar(lockfile_assessment))
)

assessment_data_new <- s3read_using(s3_path_assessment)

base_paths <- model_file_dict(params$run_id, params$run_id_year)
comp_paths <- model_file_dict(params$comparison_run_id, params$comparison_run_id_year)

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
      key = "performance"
    ),
    list(
      s3 = base_paths$output$shap$s3,
      key = "shap"
    )
  )
)

#!/usr/bin/env Rscript
# Script to check that the data dictionary file is up to date with the
# latest feature set
library(yaml)

params_filename <- "params.yaml"
data_dict_filename <- "docs/data-dict.csv"

params <- read_yaml(params_filename)
data_dict <- read.csv(data_dict_filename)

symmetric_diff <- c(
  setdiff(data_dict$variable_name, params$model$predictor$all),
  setdiff(params$model$predictor$all, data_dict$variable_name)
)
symmetric_diff_len <- length(symmetric_diff)

if (symmetric_diff_len > 0) {
  err_msg_prefix <- ifelse(symmetric_diff_len == 1, "Param is", "Params are")
  err_msg <- paste0(
    err_msg_prefix,
    " not present in both ",
    params_filename,
    " and ",
    data_dict_filename,
    ": ",
    paste(symmetric_diff, collapse = ", "),
    ". ",
    "Did you forget to reknit README.Rmd after updating ",
    params_filename,
    "?"
  )
  stop(err_msg)
}

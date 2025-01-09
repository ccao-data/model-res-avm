#!/usr/bin/env Rscript
# Script to check that the data dictionary file is up to date with the
# latest feature set
library(readr)
library(yaml)

params <- read_yaml("params.yaml")
data_dict <- read_csv("docs/data-dict.csv", show_col_types = FALSE)
stopifnot(setequal(data_dict$variable_name, params$model$predictor$all))

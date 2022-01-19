# Create custom dials:: lamdda_l2 hyperparameter to use for tuning
# See: https://lightgbm.readthedocs.io/en/latest/Parameters.html#lambda_l2
lambda_l2 <- function(range = c(0.0, 100.0), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(lambda_l2 = "L2 regularization"),
    finalize = NULL
  )
}


# Create custom dials:: min_data_per_group hyperparameter to use for tuning
# See: https://lightgbm.readthedocs.io/en/latest/Parameters.html#min_data_per_group
# And: https://github.com/microsoft/LightGBM/issues/699#issuecomment-319565499
min_data_per_group <- function(range = c(4L, 200L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(min_data_per_group = "Min. Rows of Categorical Group"),
    finalize = NULL
  )
}


# Create custom dials:: cat_smooth hyperparameter to use for tuning
# See: https://lightgbm.readthedocs.io/en/latest/Parameters.html#cat_smooth
cat_smooth <- function(range = c(10.0, 100.0), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(max_cat_threshold = "Categorical Smoothing"),
    finalize = NULL
  )
}


# Create custom dials:: cat_l2 hyperparameter to use for tuning
# See: https://lightgbm.readthedocs.io/en/latest/Parameters.html#cat_l2
# And: https://github.com/microsoft/LightGBM/issues/1934
cat_l2 <- function(range = c(10.0, 100.0), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(max_cat_threshold = "L2 Regularization in Categorical Split"),
    finalize = NULL
  )
}


# Register custom tuning parameters to lightgbm parsnip model
parsnip::set_model_arg(
  model = "boost_tree",
  eng = "lightgbm",
  parsnip = "lambda_l2",
  original = "lambda_l2",
  func = list(fun = "lambda_l2"),
  has_submodel = FALSE
)
parsnip::set_model_arg(
  model = "boost_tree",
  eng = "lightgbm",
  parsnip = "min_data_per_group",
  original = "min_data_per_group",
  func = list(fun = "min_data_per_group"),
  has_submodel = FALSE
)
parsnip::set_model_arg(
  model = "boost_tree",
  eng = "lightgbm",
  parsnip = "cat_smooth",
  original = "cat_smooth",
  func = list(fun = "cat_smooth"),
  has_submodel = FALSE
)
parsnip::set_model_arg(
  model = "boost_tree",
  eng = "lightgbm",
  parsnip = "cat_l2",
  original = "cat_l2",
  func = list(fun = "cat_l2"),
  has_submodel = FALSE
)

# Replicated parsnip::boost_tree() function with additional arg for taking
# custom hyperparameters
lgbm_tree <- function(
    mode = "unknown", mtry = NULL, trees = NULL,
    min_n = NULL, tree_depth = NULL, learn_rate = NULL,
    loss_reduction = NULL, sample_size = NULL,
    
    # Custom added parameters
    lambda_l2 = NULL, min_data_per_group = NULL,
    cat_smooth = NULL, cat_l2 = NULL) {
  args <- list(
    mtry = enquo(mtry), trees = enquo(trees), min_n = enquo(min_n),
    tree_depth = enquo(tree_depth), learn_rate = enquo(learn_rate),
    loss_reduction = enquo(loss_reduction), sample_size = enquo(sample_size),
    
    # Custom parameters
    lambda_l2 = enquo(lambda_l2),
    min_data_per_group = enquo(min_data_per_group),
    cat_smooth = enquo(cat_smooth),
    cat_l2 = enquo(cat_l2)
  )
  new_model_spec(
    "boost_tree", args,
    eng_args = NULL,
    mode, method = NULL, engine = NULL
  )
}

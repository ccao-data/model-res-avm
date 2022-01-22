# Create custom dials:: lamdda_l1 hyperparameter to use for tuning
# See: https://lightgbm.readthedocs.io/en/latest/Parameters.html#lambda_l1
lambda_l1 <- function(range = c(0.0, 100.0), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(lambda_l1 = "L1 Regularization"),
    finalize = NULL
  )
}

# Create custom dials:: lamdda_l2 hyperparameter to use for tuning
# See: https://lightgbm.readthedocs.io/en/latest/Parameters.html#lambda_l2
lambda_l2 <- function(range = c(0.0, 100.0), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(lambda_l2 = "L2 Regularization"),
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


# Register custom tuning parameters to lightgbm parsnip model
parsnip::set_model_arg(
  model = "boost_tree",
  eng = "lightgbm",
  parsnip = "lambda_l1",
  original = "lambda_l1",
  func = list(fun = "lambda_l1"),
  has_submodel = FALSE
)
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
  parsnip = "cat_smooth",
  original = "cat_smooth",
  func = list(fun = "cat_smooth"),
  has_submodel = FALSE
)

# Replicated parsnip::boost_tree() function with additional arguments for taking
# custom hyperparameters
lgbm_tree <- function(
    mode = "unknown", mtry = NULL, trees = NULL, min_n = NULL,
    tree_depth = NULL, learn_rate = NULL, loss_reduction = NULL,
    sample_size = NULL, lambda_l1 = NULL, lambda_l2 = NULL, cat_smooth = NULL) {
  args <- list(
    mtry = enquo(mtry), trees = enquo(trees), min_n = enquo(min_n),
    tree_depth = enquo(tree_depth), learn_rate = enquo(learn_rate),
    loss_reduction = enquo(loss_reduction), sample_size = enquo(sample_size),
    lambda_l1 = enquo(lambda_l1), lambda_l2 = enquo(lambda_l2),
    cat_smooth = enquo(cat_smooth)
  )
  new_model_spec(
    "boost_tree", args,
    eng_args = NULL,
    mode, method = NULL, engine = NULL
  )
}

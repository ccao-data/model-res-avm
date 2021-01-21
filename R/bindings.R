# Create custom dials:: hyperparameter to use for tuning lightgbm. This gets
# added to the custom boost_tree() function below
num_leaves <- function(range = c(32L, 2^15L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, FALSE),
    trans = trans,
    label = c(num_leaves = "# Of Leaves In One Tree"),
    finalize = NULL
  )
}


# Register custom num_leaves tuning parameter to lightgbm parsnip model
parsnip::set_model_arg(
  model = "boost_tree",
  eng = "lightgbm",
  parsnip = "num_leaves",
  original = "num_leaves",
  func = list(fun = "num_leaves"),
  has_submodel = FALSE
)


# Replicated parsnip::boost_tree() function with additional arg for taking
# num_leaves as a hyperparameter. num_leaves is a hyperparameter exclusive
# to lightgbm
lgbm_tree <- function(
    mode = "unknown", mtry = NULL, trees = NULL,
    min_n = NULL, tree_depth = NULL, learn_rate = NULL,
    loss_reduction = NULL, sample_size = NULL, stop_iter = NULL,
    num_leaves = NULL) {
  args <- list(
    mtry = enquo(mtry), trees = enquo(trees), min_n = enquo(min_n),
    tree_depth = enquo(tree_depth), learn_rate = enquo(learn_rate),
    loss_reduction = enquo(loss_reduction), sample_size = enquo(sample_size),
    stop_iter = enquo(stop_iter), num_leaves = enquo(num_leaves)
  )
  new_model_spec(
    "boost_tree", args,
    eng_args = NULL,
    mode, method = NULL, engine = NULL
  )
}

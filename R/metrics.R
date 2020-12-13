# Custom yardstick package metric for measuring COD within tidymodels
# See: https://yardstick.tidymodels.org/articles/custom-metrics.html
codm <- function(data, ...) {
  UseMethod("codm")
}
codm <- new_numeric_metric(
  codm,
  direction = "minimize"
)


# Method for data frame calculation of COD. Is applied during cross-validation,
# but is not actually used in optimizing (only for reporting)
codm.data.frame <- function(data, truth, estimate, na_rm = TRUE, ...) {
  metric_summarizer(
    metric_nm = "codm",
    metric_fn = codm_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    ... = ...
  )
}


# Vector version of COD calculation. Exponentiation here so that the ratio isn't
# log / log (prices in the model are log-transformed)
codm_vec <- function(truth, estimate, na_rm = TRUE, ...) {
  codm_impl <- function(truth, estimate) {
    assessr::cod(exp(estimate) / exp(truth))
  }

  metric_vec_template(
    metric_impl = codm_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
}


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


# Hacky function to used in lieu of finalize_workflow() to update final model
# parameters. Necessary since finalize_workflow() performs a check against the
# boost_tree() specification, which does not yet contain num_leaves as a
# parameter. Likely to be fixed by treesnip/parsnip in the future
model_update_params <- function(wflow, params) {
  wflow$fit$actions$model$spec <- list_modify(
    wflow$fit$actions$model$spec,
    params %>% select(-any_of(".config")) %>% as.list()
  )
  wflow
}


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

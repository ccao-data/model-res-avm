# Custom yardstick package metric for COD
# See: https://yardstick.tidymodels.org/articles/custom-metrics.html
codm <- function(data, ...) {
  UseMethod("codm")
}
codm <- new_numeric_metric(
  codm,
  direction = "minimize"
)


# Method for data frame calculation of COD
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


# Vector version of COD calculation
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


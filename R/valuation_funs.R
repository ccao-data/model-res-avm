val_med_pct_adj <- function(truth, estimate, min_n = 20) {
  num_sales <- sum(!is.na(truth))
  
  med_pct_adj <- median((truth - estimate) / estimate, na.rm = TRUE)
  
  output <- ifelse(num_sales >= min_n, med_pct_adj, NA_real_)
  
  return(output)
}


val_limit_ratios <- function(truth, estimate, lower = 0.7, upper = 2.0) {
  
  ratio <- estimate / truth
  
  ratio_high <- !is.na(ratio) & ratio > upper
  ratio_low <-  !is.na(ratio) & ratio < lower
  
  estimate[ratio_high] <- truth[ratio_high] * upper
  estimate[ratio_low]  <- truth[ratio_low] * lower
  
  return(estimate)
}


postval_model <- function(data, truth, estimate, ...) {
  
  med_adjustments <- data %>%
    group_by(...) %>%
    summarize(
      med_pct_adj = val_med_pct_adj({{truth}}, {{estimate}}),
      num_sales = sum(!is.na({{truth}}))
    )
  
  output <- list(
    grouping_vars = lapply(substitute(list(...))[-1], deparse),
    med_adjustments = med_adjustments
  )
  class(output) <- "postval_model"
  
  return(output)
}

predict.postval_model <- function(object, new_data, truth, estimate) {
  
  new_data %>%
    left_join(object$med_adjustments) %>%
    rowwise() %>%
    mutate(
      {{estimate}} := sum({{estimate}}, {{estimate}} * med_pct_adj, na.rm = T),
      {{estimate}} := na_if({{estimate}}, 0),
      {{estimate}} := val_limit_ratios({{truth}}, {{estimate}})
    ) %>%
    pull({{estimate}})
}
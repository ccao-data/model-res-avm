# These functions are used in valuation.R to do post-modeling adjustments to
# predicted values. They primarily limit extremely large sales ratios and shift
# ratio distributions toward 1 when necessary to correct for model bias

# Shift the distribution of ratios toward 1 to account for bias in modeling
# Not ideal but all we can do with bad data. This technique is not ideal, but
# is necessary to correct model prediction bias at the very high and very low
# ends of the price spectrum
val_med_pct_adj <- function(truth, estimate, min_n = 20, max_abs_adj = 0.4) {
  num_sales <- sum(!is.na(truth))

  med_pct_adj <- median((truth - estimate) / estimate, na.rm = TRUE)
  output <- ifelse(
    abs(med_pct_adj) > max_abs_adj,
    0.4 * sign(med_pct_adj),
    med_pct_adj
  )
  output <- ifelse(num_sales >= min_n, med_pct_adj, NA_real_)

  return(output)
}


# Limit excessively high and low ratios by capping them to a fixed boundary
# If a property has a sale, is should never have a sales ratio outside these
# boundaries
val_limit_ratios <- function(truth, estimate, lower = 0.7, upper = 2.0) {
  ratio <- estimate / truth

  ratio_high <- !is.na(ratio) & ratio > upper
  ratio_low <- !is.na(ratio) & ratio < lower

  estimate[ratio_high] <- truth[ratio_high] * upper
  estimate[ratio_low] <- truth[ratio_low] * lower

  return(estimate)
}


# Within the same development, townhomes with the same age and building size
# (nearly identical) should have the same value, so we manually set their value
# to the median of their prediction
val_townhomes_by_group <- function(data, class, estimate, townhome_adj_cols) {
  data %>%
    filter({{ class }} %in% c("210", "295")) %>%
    group_by(across(all_of(townhome_adj_cols))) %>%
    summarize(
      med_townhome_adj = median({{ estimate }}, na.rm = TRUE),
      num_in_group = n()
    ) %>%
    filter(num_in_group > 1)
}


# Post-valuation adjustment class that saves all of the above adjustments and
# can be used to predict new/unseen values
postval_model <- function(data, truth, class, estimate, med_adj_cols, townhome_adj_cols) {
  med_adjustments <- data %>%
    group_by(across(all_of(med_adj_cols))) %>%
    summarize(
      med_pct_adj = val_med_pct_adj({{ truth }}, {{ estimate }}),
      num_sales = sum(!is.na({{ truth }}))
    ) %>%
    ungroup()

  townhome_adjustments <- data %>%
    val_townhomes_by_group(
      class = meta_class,
      estimate = {{ estimate }},
      townhome_adj_cols = townhome_adj_cols
    ) %>%
    ungroup()

  output <- list(
    med_adjustments = med_adjustments,
    townhome_adjustments = townhome_adjustments
  )
  class(output) <- "postval_model"

  return(output)
}


# Predict method for S3 postval_model object
predict.postval_model <- function(object, new_data, truth, estimate) {
  new_data %>%
    left_join(object$townhome_adjustments) %>%
    mutate(
      {{ estimate }} := ifelse(!is.na(med_townhome_adj), med_townhome_adj, {{ estimate }})
    ) %>%
    left_join(object$med_adjustments) %>%
    mutate(
      {{ estimate }} := rowSums(tibble({{ estimate }}, {{ estimate }} * med_pct_adj), na.rm = T),
      {{ estimate }} := na_if({{ estimate }}, 0),
      {{ estimate }} := val_limit_ratios({{ truth }}, {{ estimate }})
    ) %>%
    pull({{ estimate }})
}

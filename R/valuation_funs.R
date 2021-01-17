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
    num_sales >= min_n,
    ifelse(
      abs(med_pct_adj) > max_abs_adj,
      0.4 * sign(med_pct_adj),
      med_pct_adj
    ),
    NA_real_
  )

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


# Within the same development, townhomes with the same characteristics
# (nearly identical) should have the same value, so we manually set their value
# to the median of their prediction + adjustment if there are any sales
val_townhomes_by_group <- function(data, truth, estimate, class, townhome_adj_cols) {
  data %>%
    filter({{ class }} %in% c("210", "295")) %>%
    group_by(across(all_of(townhome_adj_cols))) %>%
    filter(n() > 1) %>%
    summarize(
      med_townhome_est = median({{ estimate }}, na.rm = TRUE),
      med_townhome_sale = median({{ truth }}, na.rm = TRUE),
      num_in_group = n(),
      num_sales = sum(!is.na({{ truth }})),
      med_pct_adj = val_med_pct_adj({{ truth }}, {{ estimate }}, min_n = 6)
    ) %>%
    mutate(
      med_pct_adj = replace_na(med_pct_adj, 0),
      final_townhome_adj = rowSums(tibble(med_townhome_est, med_townhome_est * med_pct_adj), na.rm = T)
    )
}


# Post-valuation adjustment class that saves all of the above adjustments and
# can be used to predict new/unseen values
postval_model <- function(data, truth, estimate, class, med_adj_cols, townhome_adj_cols) {
  
  # For every unique modeling group within neighborhood, calculate quartile of
  # sale prices
  quartiles_df <- data %>%
    group_by(across(all_of(med_adj_cols))) %>%
    summarize(
      quartiles_lst = list(c(-Inf, unique(quantile(
        {{truth}},
        probs = c(0.25, 0.5, 0.75),
        na.rm = TRUE, names = FALSE
      )), Inf)
    )) %>%
    rowwise() %>%
    filter(
      !is.null(quartiles_lst),
      length(quartiles_lst) >= 5,
      !any(is.na(quartiles_lst))
    ) %>%
    ungroup()
  
  # For each sale, assign a quartile within neighborhood and modeling group,
  # then calculate the median adjustment for that quartile
  med_adjustments <- data %>%
    left_join(quartiles_df) %>%
    mutate(quartile = ifelse(
      !is.na({{ truth }}),
      pmap_chr(
        list({{ truth }}, quartiles_lst),
        function(x, y) as.character(cut(x, breaks = y, dig.lab = 10))
      ),
      NA
    )) %>%
    group_by(across(all_of(c(med_adj_cols, "quartile")))) %>%
    summarize(
      med_pct_adj = val_med_pct_adj({{ truth }}, {{ estimate }}),
      num_sales = sum(!is.na({{ truth }}))
    ) %>%
    ungroup()

  # Get the median adjusted value for townhomes, grouped by townhome_adj_cols
  # This value will override quartile-based adjustments for these properties
  townhome_adjustments <- data %>%
    val_townhomes_by_group(
      truth = {{ truth }},
      estimate = {{ estimate }},
      class = {{ class }},
      townhome_adj_cols = townhome_adj_cols
    ) %>%
    ungroup()

  # Output "trained" data frames and set class of object
  output <- list(
    quartiles = quartiles_df,
    med_adjustments = med_adjustments,
    townhome_adjustments = townhome_adjustments
  )
  class(output) <- "postval_model"

  return(output)
}


# Predict method for S3 postval_model object
predict.postval_model <- function(object, new_data, truth, estimate) {
  new_data %>%
    ungroup() %>%
    
    # Join quartile range to unseen data and assign a quartile
    left_join(object$quartiles) %>%
    mutate(quartile = ifelse(
      !is.na({{ truth }}),
      pmap_chr(
        list({{ truth }}, quartiles_lst),
        function(x, y) as.character(cut(x, breaks = y, dig.lab = 10))
      ),
      NA
    )) %>%
    
    # Join percentage adjustments by neighborhood, modeling group, and quartile
    # then apply the adjustment to all properties
    left_join(object$med_adjustments) %>%
    mutate(
      {{ estimate }} := rowSums(tibble({{ estimate }}, {{ estimate }} * med_pct_adj), na.rm = T),
      {{ estimate }} := na_if({{ estimate }}, 0),
      {{ estimate }} := val_limit_ratios({{ truth }}, {{ estimate }})
    ) %>%
    
    # Override townhomes with their own median adjusted estimated value from
    # within the same set of properties
    left_join(object$townhome_adjustments) %>%
    mutate(
      {{ estimate }} := ifelse(!is.na(final_townhome_adj), final_townhome_adj, {{ estimate }}),
    ) %>%
    
    # Return the final estimated value
    pull({{ estimate }})
}

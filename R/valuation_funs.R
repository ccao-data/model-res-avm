# These functions are used in valuation.R to do post-modeling adjustments to
# predicted values. They primarily limit extremely large sales ratios and shift
# ratio distributions toward 1 when necessary to correct for model bias

# Shift the distribution of ratios toward 1 to account for bias in modeling
# Not ideal but all we can do with bad data. This technique is not ideal, but
# is necessary to correct model prediction bias at the very high and very low
# ends of the price spectrum
val_med_pct_adj <- function(truth, estimate, min_n, max_abs_adj = 0.4) {
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
val_townhomes_by_group <- function(data, truth, estimate, class,
                                   townhome_adj_cols, min_townhome_sales) {
  data %>%
    filter({{ class }} %in% c("210", "295")) %>%
    group_by(across(all_of(townhome_adj_cols))) %>%
    filter(n() > 1) %>%
    summarize(
      th_med_est = median({{ estimate }}, na.rm = TRUE),
      th_med_sale = median({{ truth }}, na.rm = TRUE),
      th_num_in_group = n(),
      th_num_sales = sum(!is.na({{ truth }})),
      th_med_pct_adj = val_med_pct_adj(
        truth = {{ truth }}, 
        estimate = {{ estimate }},
        min_n = min_townhome_sales
      ),
      th_med_pct_adj = replace_na(th_med_pct_adj, 0)
    ) %>%
    mutate(
      th_final_value = rowSums(
        tibble(th_med_est, th_med_est * th_med_pct_adj),
        na.rm = TRUE
      )
    )
}


# Create a list of quartiles based on an input vector. Min and max are replaced
# with -Inf and Inf so that new values outside the current range can be assigned
val_create_quartiles <- function(x, na.rm = TRUE) {
  list(c(
    -Inf,
    unique(quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = na.rm, names = F)),
    Inf
  ))
}


# Assign a quartile to a value based on a list of quartiles
val_assign_quartile <- function(x, qrts) {
  ifelse(
    !is.na(x),
    pmap_chr(
      list(x, qrts),
      function(x, y) as.character(cut(x, breaks = y, dig.lab = 10))
    ),
    NA
  )
}


# Post-valuation adjustment class that saves all of the above adjustments and
# can be used to predict new/unseen values
postval_model <- function(data, truth, estimate, class, quartile_adj_cols,
                          townhome_adj_cols, min_quartile_sales,
                          min_townhome_sales) {
  
  # For every unique modeling group within neighborhood, calculate quartile of
  # sale prices
  quartiles_df <- data %>%
    group_by(across(all_of(quartile_adj_cols))) %>%
    summarize(quartiles_lst = val_create_quartiles({{ truth }})) %>%
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
    mutate(quartile = val_assign_quartile({{ truth }}, .data$quartiles_lst)) %>%
    group_by(across(all_of(c(quartile_adj_cols, "quartile")))) %>%
    summarize(
      qrt_num_sales = sum(!is.na({{ truth }})),
      qrt_med_pct_adj = val_med_pct_adj(
        truth = {{ truth }}, 
        estimate = {{ estimate }},
        min_n = min_quartile_sales
      ),
      qrt_med_pct_adj = replace_na(qrt_med_pct_adj, 0),
    ) %>%
    ungroup()

  # Get the median adjusted value for townhomes, grouped by townhome_adj_cols
  # This value will override quartile-based adjustments for these properties
  townhome_adjustments <- data %>%
    val_townhomes_by_group(
      truth = {{ truth }},
      estimate = {{ estimate }},
      class = {{ class }},
      townhome_adj_cols = townhome_adj_cols,
      min_townhome_sales = min_townhome_sales
    ) %>%
    ungroup()

  # Output "trained" data frames and set class of object
  output <- list(
    quartiles = quartiles_df,
    quartile_adj_cols = quartile_adj_cols,
    quartile_med_adjustments = med_adjustments,
    townhome_adjustments = townhome_adjustments,
    townhome_adj_cols = townhome_adj_cols,
    min_quartile_sales = min_quartile_sales,
    min_townhome_sales = min_townhome_sales
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
      val_assign_quartile({{ truth }}, .data$quartiles_lst),
      val_assign_quartile({{ estimate }}, .data$quartiles_lst)
    )) %>%
    
    # Join percentage adjustments by neighborhood, modeling group, and quartile
    # then apply the adjustment to all properties
    left_join(object$quartile_med_adjustments) %>%
    mutate(
      {{ estimate }} := rowSums(
        tibble({{ estimate }}, {{ estimate }} * qrt_med_pct_adj),
        na.rm = TRUE
      ),
      {{ estimate }} := val_limit_ratios(
        truth = {{ truth }}, 
        estimate = {{ estimate }}
      )
    ) %>%
    
    # Override townhomes with their own median adjusted estimated value from
    # within the same set of properties
    left_join(object$townhome_adjustments) %>%
    mutate(
      {{ estimate }} := ifelse(
        !is.na(th_final_value),
        th_final_value,
        {{ estimate }}
      ),
    ) %>%
    
    # Return the final estimated value
    pull({{ estimate }})
}

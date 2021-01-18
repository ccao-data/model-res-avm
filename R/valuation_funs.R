# These functions are used in valuation.R to do post-modeling adjustments to
# predicted values. They primarily limit extremely large sales ratios and shift
# ratio distributions toward 1 when necessary to correct for model bias

# Shift the distribution of ratios toward 1 to account for bias in modeling
# This technique is not ideal, but is necessary to correct model prediction
# bias at the very high and very low ends of the price spectrum
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
                                   townhome_group_cols, townhome_min_sales) {
  data %>%
    filter({{ class }} %in% c("210", "295")) %>%
    group_by(across(all_of(townhome_group_cols))) %>%
    filter(n() > 1) %>%
    summarize(
      th_med_est = median({{ estimate }}, na.rm = TRUE),
      th_med_sale = median({{ truth }}, na.rm = TRUE),
      th_num_in_group = n(),
      th_num_sales = sum(!is.na({{ truth }})),
      th_med_pct_adj = val_med_pct_adj(
        truth = {{ truth }}, 
        estimate = {{ estimate }},
        min_n = townhome_min_sales
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


# Create a list of ntiles based on an input vector. Min and max are replaced
# with -Inf and Inf so that new values outside the initial range can be assigned
val_create_ntiles <- function(x, probs, na.rm = TRUE) {
  list(c(
    -Inf,
    unique(quantile(x, probs = probs, na.rm = na.rm, names = FALSE)),
    Inf
  ))
}


# Assign a ntile to a value based on a list of ntiles created by the
# function above. Output is the character representation the the ntile range
val_assign_ntile <- function(x, ntiles) {
  ifelse(
    !is.na(x),
    pmap_chr(
      list(x, ntiles),
      function(x, y) as.character(cut(x, breaks = y, dig.lab = 10))
    ),
    NA
  )
}


# Post-valuation adjustment class that saves all of the above adjustments and
# can be used to predict new/unseen values
postval_model <- function(
  data, truth, estimate, class,
  ntile_group_cols, ntile_probs, ntile_min_sales, ntile_min_turnover,
  townhome_group_cols, townhome_min_sales) {
  
  # For every unique modeling group within neighborhood, calculate ntiles of
  # sale prices
  ntiles_df <- data %>%
    group_by(across(all_of(ntile_group_cols))) %>%
    summarize(ntiles_lst = val_create_ntiles(
      x = {{ truth }},
      probs = ntile_probs
    )) %>%
    rowwise() %>%
    filter(
      !is.null(ntiles_lst),
      length(ntiles_lst) >= length(ntile_probs + 2),
      !any(is.na(ntiles_lst))
    ) %>%
    ungroup()
  
  
  ntile_prop_counts <- data %>%
    left_join(ntiles_df) %>%
    mutate(ntile = val_assign_ntile(
      x = {{ estimate }}, 
      ntiles = .data$ntiles_lst
    )) %>%
    group_by(across(all_of(c(ntile_group_cols, "ntile")))) %>%
    summarize(ntile_num_props = n())
  
  # For each property, assign an ntile within neighborhood and modeling group,
  # then calculate the median adjustment for that ntile
  ntile_adjustments <- data %>%
    left_join(ntiles_df) %>%
    mutate(ntile = val_assign_ntile(
      x = {{ truth }}, 
      ntiles = .data$ntiles_lst
    )) %>%
    group_by(across(all_of(c(ntile_group_cols, "ntile")))) %>%
    summarize(
      ntile_num_sales = sum(!is.na({{ truth }})),
      ntile_med_pct_adj = val_med_pct_adj(
        truth = {{ truth }}, 
        estimate = {{ estimate }},
        min_n = ntile_min_sales
      ),
      ntile_med_pct_adj = replace_na(ntile_med_pct_adj, 0)
    ) %>%
    ungroup() %>%
    left_join(ntile_prop_counts) %>%
    mutate(
      ntile_med_pct_adj = ifelse(
        ntile_num_sales / ntile_num_props >= ntile_min_turnover,
        ntile_med_pct_adj, 0
      )
    ) 

  # Get the median adjusted value for townhomes, grouped by townhome_group_cols
  # This value will override ntile-based adjustments for these properties
  townhome_adjustments <- data %>%
    val_townhomes_by_group(
      truth = {{ truth }},
      estimate = {{ estimate }},
      class = {{ class }},
      townhome_group_cols = townhome_group_cols,
      townhome_min_sales = townhome_min_sales
    ) %>%
    ungroup()

  # Output "trained" data frames and set class of object
  output <- list(
    ntiles = ntiles_df,
    ntile_adjustments = ntile_adjustments,
    ntile_group_cols = ntile_group_cols,
    ntile_probs = ntile_probs,
    ntile_min_sales = ntile_min_sales,
    ntile_min_turnover = ntile_min_turnover,
    townhome_adjustments = townhome_adjustments,
    townhome_group_cols = townhome_group_cols,
    townhome_min_sales = townhome_min_sales
  )
  class(output) <- "postval_model"

  return(output)
}


# Predict method for S3 postval_model object
predict.postval_model <- function(object, new_data, truth, estimate) {
  new_data %>%
    ungroup() %>%
    
    # Join ntile ranges to unseen data and assign a ntile
    left_join(object$ntiles) %>%
    mutate(ntile = val_assign_ntile(
      x = {{ estimate }},
      ntiles = .data$ntiles_lst)
    ) %>%
    
    # Join percentage adjustments by neighborhood, modeling group, and ntile
    # then apply the adjustment to all properties
    left_join(object$ntile_adjustments) %>%
    mutate(
      {{ estimate }} := rowSums(
        tibble({{ estimate }}, {{ estimate }} * ntile_med_pct_adj),
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

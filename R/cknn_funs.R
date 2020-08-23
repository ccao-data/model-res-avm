# Helper function to create CV grid search plot
cknn_grid_plot <- function(data, m, k, l, metric) {
  clst_lab <- as_labeller(function(x) paste("m =", x))
  ggplot(data) +
    geom_tile(aes(x = factor({{k}}), y = factor({{l}}), fill = {{metric}})) +
    viridis::scale_fill_viridis(option = "viridis") +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(x = "k (num. comparables)", y = "l (distance trade-off)") +
    facet_wrap(vars({{m}}), labeller = clst_lab) +
    theme_bw() +
    theme(
      legend.title = element_text(margin = margin(b = 10)),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10))
    )
}


# Helper to fit cknn models
cknn_fit <- function(data, recipe, model, sale_prices, col_name) {
  data %>%
  mutate(col_name := map_dbl(
    predict(
      model, 
      bake(prep(recipe), data) %>%
        select(-meta_sale_price, -geo_longitude, -geo_latitude) %>%
        as.data.frame(),
      lon = data %>% pull(geo_longitude),
      lat = data %>% pull(geo_latitude)
    )$knn,
    ~ median(sale_prices[.x])
  ))
}


# Helper function to fit a cknn model then return predicted values
cknn_fit_fold <- function(data, dlon, dlat, newdata, nlon, nlat, m, k, l, weights) {
  
  # Create initial cknn model
  model <- assessr::cknn(
    data = data,
    lon  = dlon,
    lat = dlat,
    m = m,
    k = k,
    l = l,
    var_weights = weights,
    keep_data = FALSE
  )
  
  # Get return predictions for new data
  preds <- predict(
    object = model,
    newdata = newdata,
    lon  = nlon,
    lat = nlat,
    data = data
  )
  
  return(preds)
}


# Helper function to fit cknn model to a set of CV folds and grid of parameters
cknn_search <- function(analysis, assessment, param_grid, noncluster_vars, weights) {
  
  # Start the full timer
  tictoc::tic(msg = "CkNN CV model fitting complete!")
  
  # Map through and calc full param grid for each fold, evaluate against the
  # holdout set, then calc summary stats
  m_out <- purrr::pmap(list(analysis, assessment), function(ana, ass) {
      
    # Start the per fold timer
    tictoc::tic(msg = "Finished CV fold")
    
    # Prepare the data for modeling
    ana_lon <- ana %>% pull(geo_longitude)
    ana_lat <- ana %>% pull(geo_latitude)
    ana_cv <- ana %>%
      dplyr::select(-dplyr::any_of(noncluster_vars)) %>%
      as.data.frame()
    
    ass_lon <- ass %>% pull(geo_longitude)
    ass_lat <- ass %>% pull(geo_latitude)
    ass_cv <- ass %>%
      dplyr::select(-dplyr::any_of(noncluster_vars)) %>%
      as.data.frame()
    
    # Loop through params for this fold
    results <- furrr::future_pmap(param_grid, function(m, k, l) {
      
      # Return predictions for validation data
      preds <- cknn_fit_fold(
        ana_cv, ana_lon, ana_lat,
        ass_cv, ass_lon, ass_lat,
        m, k, l, weights
      )

      # Estimate clustering performance
      ass %>%
        select(actual = meta_sale_price) %>%
        dplyr::mutate(
          estimate = purrr::map_dbl(preds$knn, ~ median(ana$meta_sale_price[.x]))
        ) %>%
        dplyr::summarize(
          rmse = yardstick::rmse_vec(actual, estimate),
          rsq = yardstick::rsq_trad_vec(actual, estimate),
          cod = assessr::cod(estimate / actual),
          prd = assessr::prd(estimate, actual),
          prb = assessr::prb(estimate, actual),
          m = preds$m,
          k = preds$k,
          l = preds$l
        )
    })
    
    # Stop fold timer and return models for fold
    tictoc::toc()
    return(results)
  })
  
  # Stop timer
  tictoc::toc()
  return(m_out)
}


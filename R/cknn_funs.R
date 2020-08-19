# Helper function to prep data for cknn
cknn_recp_prep <- function(data) {
  recipe(meta_sale_price ~ ., data = data) %>%
    step_rm(-any_of(cknn_predictors), -all_outcomes()) %>%
    step_unknown(all_nominal()) %>%
    step_other(all_nominal(), threshold = 0.05) %>%
    step_naomit(all_predictors())
}


# Helper function to create CV grid search plot
cknn_grid_plot <- function(data, m, k, l, metric) {
  clst_lab <- as_labeller(function(x) paste("m (num. clusters) =", x))
  data %>%
  ggplot() +
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


# Helper function to fit cknn model to a set of CV folds and grid of parameters
cknn_fit <- function(folds, param_grid, rm_vars, w) {
  
  # Start the timer
  tictoc::tic(msg = "CkNN CV model fitting complete!")
  
  # Map through and calc full param grid for each fold
  m_out <- purrr::map(folds, function(fold) {
    furrr::future_pmap(param_grid, function(m, k, l) {
      assessr::cknn(
        data = as.data.frame(dplyr::select(fold, -dplyr::any_of(rm_vars))),
        lon  = fold %>% dplyr::pull(geo_longitude),
        lat = fold %>% dplyr::pull(geo_latitude),
        m = m,
        k = k,
        l = l,
        var_weights = w,
        keep_data = FALSE
      )
    })
  })
  
  # Stop timer
  tictoc::toc()
  return(m_out)
}


# Helper function to predict on validation data for each cknn CV fold
cknn_predict <- function(analysis, assessment, model, rm_vars) {
  
  # Start the timer
  tictoc::tic(msg = "CkNN CV model prediction complete!")
  
  # Map through and calc estimate values on validation set for each fold
  m_out <- purrr::pmap(list(analysis, assessment, model), function(og, new, modls) {
    purrr::map(modls, function(obj) {
      predict(
        object = obj,
        newdata = as.data.frame(dplyr::select(new, -dplyr::any_of(rm_vars))),
        lon  = new %>% dplyr::pull(geo_longitude),
        lat = new %>% dplyr::pull(geo_latitude),
        data = as.data.frame(dplyr::select(og, -dplyr::any_of(rm_vars)))
      )
    })
  })
  
  # Stop timer
  tictoc::toc()
  return(m_out)
}


# Evaluate the peformance of cknn data for each CV fold
cknn_eval <- function(analysis, assessment, knn_preds) {
  
  # Start the timer
  tictoc::tic(msg = "CkNN CV model evaluation complete!")

  m_out <- purrr::pmap(list(analysis, assessment, knn_preds), function(og, new, knn_ps) {
    purrr::map(knn_ps, function(knn_p) {
      dplyr::select(new, actual = meta_sale_price) %>%
      dplyr::mutate(
        estimate = purrr::map_dbl(knn_p$knn, ~ median(og$meta_sale_price[.x]))
      ) %>%
      dplyr::summarize(
        count = dplyr::n(),
        rmse = yardstick::rmse_vec(actual, estimate),
        rsq = yardstick::rsq_trad_vec(actual, estimate),
        cod = assessr::cod(estimate / actual),
        prd = assessr::prd(estimate, actual),
        prb = assessr::prb(estimate, actual),
        m = knn_p$m,
        k = knn_p$k,
        l = knn_p$l
      )
    })
  })
  
  # Stop timer
  tictoc::toc()
  return(m_out)
}
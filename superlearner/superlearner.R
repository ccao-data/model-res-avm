# Superlearner code adapted from: 
# https://www.alexpghayes.com/blog/implementing-the-super-learner-with-tidymodels/

# Currently WAY too memory intensive to actually be useful

expand_model_grid <- function(models, params) {

  map2(models, params, merge) %>%
    bind_rows() %>%
    rename(spec = x) %>%
    mutate(model_id = rep(x = names(models), times = sapply(params, nrow))) %>%
    group_by(model_id) %>%
    mutate(param_set = row_number())
}


expand_fold_grid <- function(recipes, folds) {
  
  tibble(
    model_id = rep(
      x = names(recipes),
      times = rep(nrow(folds), length(recipes))
    ),
    fold_id = paste0("Fold", rep(seq_len(nrow(folds)), length(recipes))),
    prepped = unlist(
      future_map(recipes, ~ map(folds$splits, prepper, .x)),
      recursive = FALSE
    ),
    assessment = rep(map(folds$splits, assessment), length(recipes)),
    idx = rep(
      map(folds$splits, ~ as.integer(.x, data = "assessment")),
      length(recipes)
    )
  )
}


fit_on_fold <- function(spec, prepped) {
  
  x <- juice(prepped, all_predictors())
  y <- juice(prepped, all_outcomes())
  
  fit_xy(spec, x, y)
}


predict_helper <- function(fit, new_data, idx, recipe) {
  
  new_data <- bake(recipe, new_data %>% select(-outcome_names(recipe)))
  predict(fit, new_data, type = "numeric") %>% 
    tibble::add_column(idx = idx, .before = TRUE)
}


spread_nested_predictions <- function(data) {
  
  if (any(map_lgl(data$preds, ~ !is_tibble(.x)))) data <- data %>% unnest(preds)
  data %>% 
    unnest(preds) %>%
    pivot_wider(
      id_cols = idx,
      names_from = c(model_id, param_set),
      values_from = contains(".pred")
    )
}


super_learner <- function(models, params, recipes, data, meta_spec, meta_recipe) {
  
  full <- map(recipes, ~ prep(.x, training = data))
  folds <- expand_fold_grid(recipes, vfold_cv(data, v = 5))
  specs <- expand_model_grid(models, params)
  
  full_fits <- specs %>%
    mutate(fit = map2(spec, model_id, function(sp, id) {
      x <- juice(full[[id]], all_predictors())
      y <- juice(full[[id]], all_outcomes())
      
      fit_xy(sp, x, y)
    } ))
  
  cv_fits <- specs %>%
    mutate(
      fold_id = map(model_id, function(id) {
        folds %>% filter(model_id == id) %>% pull(fold_id)
      }),
      fit = future_pmap(list(spec, model_id), function(spec, id) {
        pdata <- folds %>% filter(model_id == id) %>% pull(prepped)
        map(pdata, ~ fit_on_fold(spec, .x))
      }, .progress = TRUE)
    )
  
  holdout_preds <- cv_fits %>% 
    unnest(cols = c(fit, fold_id)) %>%
    mutate(
      preds = future_pmap(
        list(fit, model_id, fold_id), function(fit, m_id, f_id) {
          
        id_idx <- folds$model_id == m_id & folds$fold_id == f_id

        assessment <- folds$assessment[id_idx]
        recipe <- folds$prepped[id_idx]
        idx <- unlist(folds$idx[id_idx], recursive = FALSE)
        
        map2(assessment, recipe, ~ predict_helper(fit, .x, idx, .y))
      }, .progress = TRUE)
    ) %>%
    spread_nested_predictions() %>%
    arrange(idx) %>%
    select(-idx)
  
  y <- juice(prep(meta_recipe, training = data), all_outcomes())
  metalearner <- fit_xy(meta_spec, holdout_preds, y)

  sl <- list(full_fits = full_fits, metalearner = metalearner, recipes = full)
  class(sl) <- "super_learner"
  sl
}


predict.super_learner <- function(x, new_data) {
  
  idx <- seq_len(nrow(new_data))
  new_preds <- x$full_fits %>% 
    mutate(
      preds = future_map2(fit, model_id, ~ predict_helper(.x, new_data, idx, x$recipes[[.y]])),
    ) %>%
    spread_nested_predictions() %>%
    select(-idx)

  predict(x$metalearner, new_preds, type = "numeric") %>%
    unlist() %>%
    as.numeric()
}


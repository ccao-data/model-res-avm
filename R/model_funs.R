# Remove intermediate objects from model creation
rm_intermediate <- function(x, keep = NULL) {
  env <- ls(envir = .GlobalEnv)
  rm_list <- env[
    str_starts(env, x) &
    (!str_detect(env, "final") | str_detect(env, "wflow")) &
    !env %in% keep
  ]
  if (length(rm_list) > 0) {
    message(paste(
      "Removing intermediate objects:",
      paste(rm_list, collapse = ", ")
    ))
    rm(list = rm_list, envir = .GlobalEnv); gc()
  }
}

# Return prediction from a model and recipe
model_predict <- function(model, recipe, data) {
  exp(predict(
    model,
    new_data = bake(recipe, data) %>%
      select(-ends_with("_sale_price"))
  )$.pred)
}


# Get environmental variable else a default value
model_get_env <- function(x, default) {
  env <- Sys.getenv(x, unset = NA)
  ifelse(!is.na(env), env, default)
} 


# Remove data from iteration results objects created by tune_grid and tune_bayes
model_strip_data <- function(x) {
  stripped <- x %>% select(-any_of("splits"))
  
  attrs <- attributes(x) %>%
    purrr::list_modify("names" = names(stripped))
  attributes(stripped) <- attrs
  stripped
}


# Create a stacked model object containing each fitted model, its corresponding
# recipe, a metamodel, and the training data
stack_model <- function(models, recipes, meta_spec, data, add_vars = NULL) {
  
  # Check that names match for models and recipes
  stopifnot(
    all(names(models) %in% names(recipes)),
    all(names(recipes) %in% names(models))
  )

  # Get index of non-cknn models
  non_cknn <- !names(models) %in% c("cknn")
  
  # Get predictions only for cknn model
  if ("cknn" %in% names(models)) {
    sales <- pull(juice(prep(recipes$cknn), all_outcomes()))
    cknn_preds <- map_dbl(models$cknn$knn, ~ median(sales[.x]))
  } else {
    sales <- NULL
    cknn_preds <- NULL
  }
  
  # Get full fits for all non-cknn models
  meta_train <- pmap(
    list(models[non_cknn], recipes[non_cknn]),
    ~ model_predict(.x, .y, data)
  )
  
  # Create a recipe for the meta model
  meta_recipe <- stack_recp_prep(
    bind_cols(meta_train, data, cknn = cknn_preds),
    keep_vars = c(names(models), add_vars)
  )
 
  # Prep the data for fitting in the meta model
  prepped <- prep(meta_recipe)
  x <- juice(prepped, all_predictors())
  y <- juice(prepped, all_outcomes())
  
  # Fit the meta model on the predictions of the other models
  meta_fit <-  fit_xy(meta_spec, x = x, y = y)

  # Return original models, recipes, and meta fit
  meta <- list(
    models = models,
    recipes = recipes,
    meta_fit = meta_fit,
    meta_recipe = meta_recipe,
    sale_prices = sales
  )
  class(meta) <- "stack_model"
  meta
}


# S3 predict method for stack model object
predict.stack_model <- function(object, new_data) {
  
  # Get index of non-cknn models
  non_cknn <- !names(object$models) %in% c("cknn")
  
  # Predict new data using previously fitted models
  new_preds <- pmap(
    list(object$models[non_cknn], object$recipes[non_cknn]),
    ~ model_predict(.x, .y, new_data)
  )
  
  # Get cknn predictions
  if ("cknn" %in% names(object$models)) {
    cknn_preds <- list(unname(cknn_predict(
      object$models$cknn, object$recipes$cknn, new_data, object$sale_prices
    )))
  } else {
    cknn_preds <- NULL
  }
  new_preds <- c(new_preds, cknn = cknn_preds)
   
  # Get predictions from all other models
  stack_preds <- exp(predict(
    object$meta_fit,
    bake(prep(object$meta_recipe), bind_cols(new_preds, new_data)) %>%
      select(-ends_with("_sale_price")) # Fix glmnet not removing outcome var
  ))
  
  # Output other model predictions + stacked preds
  c(new_preds, stack = list(stack_preds$.pred))
}

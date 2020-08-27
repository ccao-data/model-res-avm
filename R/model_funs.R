# Helper to return prediction from a model and recipe
model_predict <- function(model, recipe, data) {
  exp(predict(
    model,
    new_data = bake(recipe, data) %>% select(-any_of("meta_sale_price"))
  )$.pred)
}


# Get environmental variable else default value
model_get_env <- function(x, default) {
  env <- Sys.getenv(x, unset = NA)
  ifelse(!is.na(env), env, default)
} 


# Remove data from iteration results objects
model_strip_data <- function(x) {
  stripped <- x %>% select(-any_of("splits"))
  
  attrs <- attributes(x) %>%
    purrr::list_modify("names" = names(stripped))
  attributes(stripped) <- attrs
  stripped
}


# Create a stacked model object class
stack_model <- function(models, recipes, meta_spec, add_vars = NULL, data) {

  model_names <- c(names(models), names(recipes))
  
  # Separate CkNN models from main list 
  cknn_model <- models$cknn
  cknn_recipe <- recipes$cknn
  models["cknn"] <- NULL
  recipes["cknn"] <- NULL
  
  sales <- pull(juice(prep(cknn_recipe), all_outcomes()))
  cknn_preds <- map(cknn_model$knn, ~ median(sales[.x]))
  
  # Get full fits for each trained input model
  meta_train <- pmap(list(models, recipes), ~ model_predict(.x, .y, data)) 
  
  # Create a recipe based on the inputs  
  meta_recipe <- stack_recp_prep(
    bind_cols(meta_train, data, cknn = cknn_preds),
    keep_vars = c(model_names, add_vars)
  )
 
  # Prep the predictions for fitting in the meta model 
  prepped <- prep(meta_recipe)
  x <- juice(prepped, all_predictors())
  y <- juice(prepped, all_outcomes())
  
  # Fit the meta model
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

predict.stack_model <- function(object, new_data) {
  
  # Predict new data using previously fitted models
  new_preds <- pmap(
    list(object$models, object$recipes),
    ~ model_predict(.x, .y, new_data)
  )
    
  # Predict new data using the meta model
  stack_preds <- exp(predict(
    object$meta_fit,
    bake(prep(object$meta_recipe), bind_cols(new_preds, new_data))
  ))
  
  # Output other model predictions + stacked preds
  c(new_preds, stack = list(stack_preds$.pred))
}

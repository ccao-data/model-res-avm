# Helper to return prediction from a model and recipe
model_fit <- function(data, recipe, model, col_name) {
  data <- data %>%
    mutate(
      {{col_name}} := exp(predict(
        model,
        new_data = bake(recipe, data) %>% select(-any_of("meta_sale_price"))
      )$.pred)
    )
  
  return(data)
}


# Helper to get best params from parquet file
model_get_stored_params <- function(file) {
  read_parquet(file) %>%
    filter(.metric == "codm") %>%
    filter(mean == min(mean)) %>%
    distinct(mean, .keep_all = TRUE)
}
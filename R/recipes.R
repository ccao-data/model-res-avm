# Helper to return prediction from a model and recipe
model_fit <- function(data, recipe, model, col_name) {
  data %>%
    mutate(
        col_name := exp(predict(
        model,
        new_data = bake(recipe, df))$.pred
      )
    )
}

# Helper function to prep data for cknn
cknn_recp_prep <- function(data, keep_vars) {
  recipe(meta_sale_price ~ ., data = data) %>%
    step_rm(-any_of(keep_vars), -all_outcomes()) %>%
    step_unknown(all_nominal()) %>%
    step_other(all_nominal(), threshold = 0.005) %>%
    step_naomit(all_predictors())
}


# Helper function to prep data for all non-cknn models
mod_recp_prep <- function(data, keep_vars) {
  recipe(meta_sale_price ~ ., data = data) %>%
    step_rm(-any_of(keep_vars), -all_outcomes()) %>%
    step_mutate(town_nbhd = paste0(
      meta_town_code,
      str_pad(meta_nbhd, 3, "left", "0")
    )) %>%
    step_rm(any_of(c("meta_town_code", "meta_nbhd"))) %>%
    step_unknown(all_nominal()) %>%
    step_other(all_nominal(), -any_of("town_nbhd"), threshold = 0.005) %>%
    step_naomit(all_predictors()) %>%
    step_log(all_outcomes(), ends_with("_price"))
    # step_dummy(all_nominal())
}


# Function to create recipe for stacking model
stack_recp_prep <- function(data) {
  recipe(meta_sale_price ~ ., data = data) %>%
    step_rm(-ends_with("_sale_price"), -all_outcomes()) %>%
    step_naomit(all_predictors()) %>%
    step_log(all_predictors(), all_outcomes())
}


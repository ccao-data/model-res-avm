
library(tidymodels)


# Bake the data

model_xgb <- boost_tree(
    trees = tune(),
    learn_rate = tune(),
    tree_depth = tune(),
    min_n = tune(),
    loss_reduction = tune(), 
    sample_size = tune(),
    mtry = tune()
  ) %>% 
  set_mode("regression") %>% 
  set_engine("xgboost", nthread = parallel::detectCores())


model_workflow <- workflow() %>%
  add_recipe(model_recipe) %>%
  add_model(model_xgb)

xgboost_params <- parameters(
  trees(), learn_rate(),
  tree_depth(), min_n(), 
  loss_reduction(),
  sample_size = sample_prop(), finalize(mtry(), modeldata)  
)

xgboost_params <- xgboost_params %>%
  update(trees = trees(c(100, 500))) 


set.seed(123)
model_vfolds <- vfold_cv(modeldata, v = 3, strata = sale_price)

xgboost_tune <-
  model_workflow %>%
  tune_bayes(
    resamples = model_vfolds,
    param_info = xgboost_params,
    iter = 30, 
    metrics = metric_set(rmse, mape),
    control = control_bayes(no_improve = 10, 
                            save_pred = T, verbose = T)
  )

autoplot(xgboost_tune)

# 
# rf_res1 <- ranger::ranger(SalePrice ~ ., , data = model_recipe_baked,
#                           importance = "impurity_corrected")
# 
# ranger::importance(rf_res1) %>% 
#   enframe("Variable", "Importance") %>%
#   mutate(Variable = fct_reorder(Variable, Importance),
#          New = Variable  %in% setdiff(names(ames_training), names(ames_df))) %>% 
#   arrange(desc(Importance)) %>% 
#   slice(1:25) %>% 
#   ggplot(aes(x = Variable, y = Importance, fill = New)) +
#   geom_col() +
#   coord_flip() +
#   scale_fill_viridis_d(end = .7) +
#   labs(title = "Variable Importance", subtitle = "Original Variables")

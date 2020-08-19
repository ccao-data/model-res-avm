
cknn_final_pred <- predict(
  cknn_final,
  newdata = cknn_final_test %>%
    select(-meta_sale_price, -geo_latitude, -geo_longitude) %>%
    as.data.frame(),
  lon = cknn_final_test %>% pull(geo_longitude),
  lat = cknn_final_test %>% pull(geo_latitude)
)

town_sum <- cknn_test %>%
  mutate(knn = cknn_final_pred$knn) %>%
  mutate(estimate = map_dbl(knn, ~ median(cknn_train$meta_sale_price[.x]))) %>%
  mutate(town = town_convert(meta_town_code)) %>%
  rename(actual = meta_sale_price) %>%
  group_by(town) %>%
  summarize(
    count = n(),
    rmse = rmse_vec(actual, estimate),
    rsq = rsq_trad_vec(actual, estimate),
    cod = cod(estimate / actual),
    prd = prd(estimate, actual),
    prb = prb(estimate, actual)
  )


cknn_final_recp <- cknn_train %>%
  prep(cknn_recp, training = .) 

cknn_final_data <- juice(cknn_final_recp) 
cknn_final_test <- bake(cknn_final_recp, new_data = cknn_test)

cknn_final <- cknn(
  data = cknn_final_data %>%
    select(-meta_sale_price, -geo_latitude, -geo_longitude) %>%
    as.data.frame(),
  lon = cknn_final_data %>% pull(geo_longitude),
  lat = cknn_final_data %>% pull(geo_latitude),
  m = 12,
  k = 10,
  l = 0.5
)


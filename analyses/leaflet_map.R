create_leaflet_map <- function(dataset, legend_value, legend_title, order_scheme = "high", longitude = "loc_longitude", latitude = "loc_latitude") {
  # Filter neighborhoods that have at least one observation
  nbhd_borders <- nbhd %>%
    right_join(dataset, by = c("town_nbhd" = "meta_nbhd_code"))

  # Create the color palette based on order_scheme
  if (order_scheme == "low") {
    pal <- colorNumeric(palette = "Reds", domain = dataset[[legend_value]], reverse = TRUE)
  } else {
    pal <- colorNumeric(palette = "Reds", domain = dataset[[legend_value]])
  }

  # Group by pin and create popup content
  dataset_grouped <- dataset %>%
    group_by(meta_pin, !!sym(longitude), !!sym(latitude)) %>%
    summarize(
      popup_content = paste(
        "<br>", "Pin: ", meta_pin,
        "<br>", "SHAP:", dollar(!!sym(params$added_feature_shap)),
        "<br>", "Relative SHAP", shap_relative,
        "<br>", "Feature:", sprintf("%.2f", !!sym(params$added_feature)),
        "<br>", "New FMV:", pred_pin_final_fmv_new,
        "<br>", "Comparison FMV: ", pred_pin_final_fmv_comparison,
        "<br>", "FMV Difference: ", dollar(diff_pred_pin_final_fmv),
        collapse = "<br>---<br>"
      ),
      .groups = "drop"
    )

  # Create the leaflet map
  leaflet(dataset_grouped) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(
      lng = ~ get(longitude),
      lat = ~ get(latitude),
      radius = 5,
      color = ~ pal(dataset_grouped[[legend_value]]),
      popup = ~popup_content
    ) %>%
    addPolygons(
      data = nbhd_borders,
      color = "black",
      weight = 2,
      fill = FALSE
    ) %>%
    addLegend(
      "bottomright",
      pal = pal,
      values = dataset_grouped[[legend_value]],
      title = legend_title
    )
}


highest_100 <- leaflet_data %>%
  arrange(desc({{ target_feature_value }})) %>%
  slice(1:100)

create_leaflet_map(highest_100, {{ target_feature_value }}, "Largest 100 Values", order_scheme = "high")

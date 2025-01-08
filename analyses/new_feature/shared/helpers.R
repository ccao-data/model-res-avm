model_fetch_run_subset <- function(
    run_id, year, analyses_paths, append_run_id = FALSE) {
  s3_objs <- grep("s3://", unlist(analyses_paths$output), value = TRUE)
  bucket <- strsplit(s3_objs[1], "/")[[1]][3]

  data_list <- list()

  for (analyses_path in analyses_paths$output) {
    is_directory <- endsWith(analyses_path$s3, "/")
    if (is_directory) {
      partitioned_by_run <- endsWith(
        analyses_path$s3,
        paste0("run_id=", run_id, "/")
      )
      dir_path <- if (partitioned_by_run) {
        analyses_path$s3
      } else {
        paste0(analyses_path$s3, "year=", year, "/run_id=", run_id, "/")
      }

      message("Now fetching directory: ", dir_path)
      objs_prefix <- sub(paste0("s3://", bucket, "/"), "", dir_path)
      objs <- aws.s3::get_bucket_df(bucket, objs_prefix)
      objs <- dplyr::filter(objs, Size > 0)

      combined_data <- purrr::map_dfr(objs$Key, function(key) {
        message("Now fetching file: ", key)
        local_temp_path <- file.path(tempdir(), basename(key))
        aws.s3::save_object(key, bucket = bucket, file = local_temp_path)
        arrow::read_parquet(local_temp_path)
      })

      if (nrow(objs) > 0) {
        data_key <- if (append_run_id) {
          paste0(analyses_path$key, "_", run_id)
        } else {
          analyses_path$key
        }
        data_list[[data_key]] <- combined_data
      } else {
        warning(analyses_path$key, " does not exist for this run")
      }
    } else {
      message("Now fetching file: ", analyses_path$s3)
      if (aws.s3::object_exists(analyses_path$s3, bucket = bucket)) {
        local_temp_path <- file.path(tempdir(), basename(analyses_path$s3))
        aws.s3::save_object(
          analyses_path$s3,
          bucket = bucket, file = local_temp_path
        )
        data_key <- if (append_run_id) {
          paste0(analyses_path$key, "_", run_id)
        } else {
          analyses_path$key
        }
        data_list[[data_key]] <- arrow::read_parquet(local_temp_path)
      } else {
        warning(analyses_path$key, " does not exist for this run")
      }
    }
  }

  return(data_list)
}


rename_var <- function(var_name, suffix, new_suffix) {
  if (exists(var_name) && is.data.frame(get(var_name))) {
    if (grepl(paste0("_", suffix, "$"), var_name)) {
      new_name <- sub(paste0("_", suffix, "$"), new_suffix, var_name)
      assign(new_name, get(var_name), envir = .GlobalEnv)
      rm(list = var_name, envir = .GlobalEnv)
    }
  }
}

clean_column_values <- function(df, column_name) {
  df[[column_name]] <- df[[column_name]] %>%
    gsub("^meta_|^prox_|^other_|^loc_|^char_|^acs5|^acs_|^ccao_", "", .) %>%
    gsub("_", " ", .) %>%
    stringr::str_to_title()
  return(df)
}

s3_data_download <- function(dvc_md5_assessment_data) {
  # Define the S3 path for assessment data
  s3_path <- paste0(
    "s3://ccao-data-dvc-us-east-1/files/md5/",
    substr(dvc_md5_assessment_data, 1, 2), "/",
    substr(dvc_md5_assessment_data, 3, nchar(dvc_md5_assessment_data))
  )

  # Read and return the parquet data
  read_parquet(s3_path)
}


create_leaflet_map <- function(dataset, legend_value, legend_title,
                               order_scheme = "high",
                               longitude = "loc_longitude",
                               latitude = "loc_latitude",
                               display_as_percent = FALSE) {
  # Filter neighborhoods that have at least one observation
  nbhd_borders <- nbhd %>%
    right_join(dataset, by = c("town_nbhd" = "meta_nbhd_code"))

  # Adjust the dataset values if display_as_percent is TRUE
  if (display_as_percent) {
    dataset[[legend_value]] <- dataset[[legend_value]] * 100
  }

  # Create the color palette based on order_scheme
  if (order_scheme == "low") {
    pal <- colorNumeric(
      palette = "Reds",
      domain = dataset[[legend_value]], reverse = TRUE
    )
  } else {
    pal <- colorNumeric(
      palette = "Reds",
      domain = dataset[[legend_value]]
    )
  }

  # Calculate the bounding box of the filtered neighborhoods
  bbox <- st_bbox(nbhd_borders)

  # Create the leaflet map
  leaflet(dataset) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(
      lng = ~ get(longitude),
      lat = ~ get(latitude),
      radius = 5,
      color = ~ pal(dataset[[legend_value]]),
      popup = ~ {
        shap_values <- dataset %>%
          select(starts_with("target_feature_shap_")) %>%
          summarise_all(~ ifelse(!is.na(.), sprintf(
            "SHAP: %s",
            scales::dollar(.)
          ), NA)) %>%
          apply(1, function(row) {
            paste(na.omit(row), collapse = "<br>")
          })
        paste(
          "Pin: ", meta_pin,
          ifelse(shap_values == "",
            "", paste0("<br>", shap_values)
          ),
          "<br>", "Relative SHAP: ",
          scales::percent(relative_shap, accuracy = 0.01),
          "<br>", "Feature: ", get(params$added_feature),
          "<br>", "New FMV: ", pred_pin_final_fmv_new,
          "<br>", "Comparison FMV: ", pred_pin_final_fmv_comp,
          "<br>", "FMV Difference: ",
          scales::percent(diff_pred_pin_final_fmv, accuracy = 0.01)
        )
      }
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
      values = dataset[[legend_value]],
      title = legend_title,
      labFormat = if (display_as_percent) {
        labelFormat(suffix = "%")
      } else {
        labelFormat()
      }
    )
}

create_neighborhood_map <- function(data,
                                    fill_column,
                                    palette,
                                    legend_title,
                                    fill_type = c(
                                      "continuous",
                                      "categorical"
                                    ),
                                    top_n = NULL) {
  fill_type <- match.arg(fill_type)

  # Filter to top N most common values if fill type is categorical
  if (fill_type == "categorical" && !is.null(top_n)) {
    top_values <- data %>%
      count(!!sym(fill_column)) %>%
      arrange(desc(n)) %>%
      slice_head(n = top_n) %>%
      pull(!!sym(fill_column))

    # Filter data to keep only rows with top values
    data <- data %>% filter(!!sym(fill_column) %in% top_values)
  }

  # Define palette based on fill type
  if (fill_type == "continuous") {
    fill_palette <- colorNumeric(palette, domain = data[[fill_column]])
  } else if (fill_type == "categorical") {
    fill_palette <- colorFactor(brewer.pal(n = 8, name = palette),
      domain = unique(data[[fill_column]])
    )
  }

  # Generate popup content dynamically
  popup_content <- ~ paste0(
    "<strong>Neighborhood Code:</strong> ", meta_nbhd_code, "<br>",
    "<strong>Primary Mode Percentage:</strong> ",
    round(primary_mode_percentage, 2), "%<br>",
    "<strong>Secondary Mode Percentage:</strong> ",
    round(secondary_mode_percentage, 2), "%<br>",
    "<strong>Primary Mode:</strong> ", primary_mode, "<br>",
    "<strong>Secondary Mode:</strong> ", secondary_mode
  )

  # Build the map
  map <- leaflet(data) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(
      fillColor = ~ fill_palette(data[[fill_column]]),
      fillOpacity = 0.7,
      color = "black",
      weight = 1,
      popup = popup_content
    ) %>%
    addLegend(
      position = "bottomright",
      pal = fill_palette,
      values = ~ data[[fill_column]],
      title = legend_title,
      opacity = 0.7
    )

  return(map)
}

create_histogram_with_statistics <- function(data, target_feature, x_label, y_label = "Frequency", filter_outliers = FALSE, filter_column = NULL) {
  # Conditionally filter outliers if requested
  if (filter_outliers && !is.null(filter_column)) {
    data <- data %>%
      filter(
        !!sym(filter_column) >= quantile(!!sym(filter_column), 0.025, na.rm = TRUE) &
          !!sym(filter_column) <= quantile(!!sym(filter_column), 0.975, na.rm = TRUE)
      )
  }

  # Calculate mean and median
  mean_value <- mean(data[[target_feature]], na.rm = TRUE)
  median_value <- median(data[[target_feature]], na.rm = TRUE)


  # Calculate dynamic binwidth based on data range
  range_value <- range(data[[target_feature]], na.rm = TRUE)
  dynamic_binwidth <- (range_value[2] - range_value[1]) / 30

  htmltools::tagList(
    plot <- data %>%
      ggplot(aes(x = !!sym(target_feature))) +
      geom_histogram(
        fill = "blue",
        color = "black",
        alpha = 0.7,
        binwidth = dynamic_binwidth
      ) +
      geom_vline(aes(xintercept = mean_value, color = "Mean"), linetype = "dashed", linewidth = 1, show.legend = TRUE) +
      geom_vline(aes(xintercept = median_value, color = "Median"), linetype = "dashed", linewidth = 1, show.legend = TRUE) +
      scale_color_manual(
        name = "Statistics",
        values = c(Mean = "red", Median = "green"),
        labels = c("Mean", "Median")
      ) +
      labs(
        x = x_label,
        y = y_label
      ) +
      theme_minimal()
  )



  return(plot)
}

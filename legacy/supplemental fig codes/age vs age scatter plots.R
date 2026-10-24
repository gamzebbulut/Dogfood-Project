## age vs age scatter plots

# run "pre processing code" first.

#need function here:

# Define the function PUTS LABEL TO THE RIGHT
# Revised Function 2
scatter_plot_by_type_label_right <- function(data, x, y, z = NULL) {
  x <- as.character(substitute(x))
  y <- as.character(substitute(y))
  
  if (!is.null(z)) {
    z <- as.character(substitute(z))
    filtered_data <- data[data$Type == z, ]
  } else {
    filtered_data <- data
  }
  
  # Filter out NA values for x and y
  filtered_data <- filtered_data[!is.na(filtered_data[[x]]) & !is.na(filtered_data[[y]]), ]
  n_count <- nrow(filtered_data)
  
  # Calculate correlation only if there are enough observations
  if (n_count > 2) {
    cor_test <- cor.test(filtered_data[[x]], filtered_data[[y]], method = "spearman")
    correlation <- cor_test$estimate
    p_value <- cor_test$p.value
  } else {
    correlation <- NA
    p_value <- NA
  }
  
  # Determine annotation label
  annotation_label <- if (!is.na(correlation)) {
    paste0("Spearman r = ", round(correlation, 2))
  } else {
    "Insufficient data"
  }
  
  # Determine the dynamic position for annotation
  x_max <- max(filtered_data[[x]], na.rm = TRUE)
  y_max <- max(filtered_data[[y]], na.rm = TRUE)
  
  # Create the scatter plot
  p <- ggplot(filtered_data, aes_string(x = x, y = y)) +
    geom_point(size = 3) +
    labs(
      title = if (!is.null(z)) {
        paste("Scatter Plot of", x, "vs", y, "for type", z)
      } else {
        paste("Scatter Plot of", x, "vs", y, "for all food types")
      },
      x = x,
      y = y
    ) +
    geom_smooth(method = "lm", col = "red", lty = 2) +
    labs(subtitle = paste("Number of Dog Foods Tested: n =", n_count)) +
    annotate(
      "text", x = x_max, y = y_max,
      label = annotation_label,
      hjust = 1.1, vjust = 1.1, size = 10, color = "blue"
    ) +
    theme(
      axis.title.x = element_text(size = 22, face = "bold"),
      axis.title.y = element_text(size = 22, face = "bold"),
      axis.text.x = element_text(size = 16),
      axis.text.y = element_text(size = 16)
    )
  
  print(p)
}


# Define the function (puts labels to the left) 
# Revised Function 2
scatter_plot_by_type_label_left <- function(data, x, y, z = NULL) {
  x <- as.character(substitute(x))
  y <- as.character(substitute(y))
  
  if (!is.null(z)) {
    z <- as.character(substitute(z))
    filtered_data <- data[data$Type == z, ]
  } else {
    filtered_data <- data
  }
  
  # Filter out NA values for x and y
  filtered_data <- filtered_data[!is.na(filtered_data[[x]]) & !is.na(filtered_data[[y]]), ]
  n_count <- nrow(filtered_data)
  
  # Calculate correlation only if there are enough observations
  if (n_count > 2) {
    cor_test <- cor.test(filtered_data[[x]], filtered_data[[y]], method = "spearman")
    correlation <- cor_test$estimate
    p_value <- cor_test$p.value
  } else {
    correlation <- NA
    p_value <- NA
  }
  
  # Create the scatter plot
  p <- ggplot(filtered_data, aes_string(x = x, y = y)) +
    geom_point(size = 3) +
    labs(
      title = if (!is.null(z)) {
        paste("Scatter Plot of", x, "vs", y, "for type", z)
      } else {
        paste("Scatter Plot of", x, "vs", y, "for all food types")
      },
      x = x,
      y = y
    ) +
    geom_smooth(method = "lm", col = "red", lty = 2) +
    labs(subtitle = paste("Number of Dog Foods Tested: n =", n_count)) +
    annotate(
      "text", x = -Inf, y = Inf,
      label = paste0("\nSpearman r =", round(correlation, 2)),
      hjust = -0.1, vjust = 0.8, size = 10, color = "blue"
    ) +
    theme(
      axis.title.x = element_text(size = 22, face = "bold"),
      axis.title.y = element_text(size = 22, face = "bold"),
      axis.text.x = element_text(size = 16),
      axis.text.y = element_text(size = 16)
    )
  
  print(p)
}


# CML versus others in canned
scatter_plot_by_type_label_left(data_filtered_moist,"CML_ug_per_g_food_moist", "MG_ug_per_g_food_moist","Canned")
scatter_plot_by_type_label_left(data_filtered_moist,"CML_ug_per_g_food_moist", "SF_ug_per_g_food_moist","Canned") # 
scatter_plot_by_type_label_left(data_filtered_moist,"CML_ug_per_g_food_moist", "LF_AGE_ug_per_g_food_moist","Canned") # 

# MG versus others in canned etc
scatter_plot_by_type_label_left(data_filtered_moist,"MG_ug_per_g_food_moist", "SF_ug_per_g_food_moist","Canned") # 
scatter_plot_by_type_label_left(data_filtered_moist,"MG_ug_per_g_food_moist", "LF_AGE_ug_per_g_food_moist","Canned")
scatter_plot_by_type_label_left(data_filtered_moist,"SF_ug_per_g_food_moist", "LF_AGE_ug_per_g_food_moist","Canned")
scatter_plot_by_type_label_left(data_filtered_moist,"SF_ug_per_g_food_moist", "CML_plus_MG_g_moist","Canned")
scatter_plot_by_type_label_left(data_filtered_moist,"LF_AGE_ug_per_g_food_moist", "CML_plus_MG_g_moist","Canned")
scatter_plot_by_type_label_left(data_filtered_moist,"LF_AGE_ug_per_g_food_moist", "Combined_g_moist","Canned")

# CML versus others in kibble
scatter_plot_by_type_label_left(data_filtered_moist,"CML_ug_per_g_food_moist", "MG_ug_per_g_food_moist","Kibble")
scatter_plot_by_type_label_left(data_filtered_moist,"CML_ug_per_g_food_moist", "SF_ug_per_g_food_moist","Kibble") # 
scatter_plot_by_type_label_left(data_filtered_moist,"CML_ug_per_g_food_moist", "LF_AGE_ug_per_g_food_moist","Kibble") # 

# MG versus others in canned etc
scatter_plot_by_type_label_left(data_filtered_moist,"MG_ug_per_g_food_moist", "SF_ug_per_g_food_moist","Kibble") #
scatter_plot_by_type_label_left(data_filtered_moist,"MG_ug_per_g_food_moist", "LF_AGE_ug_per_g_food_moist","Kibble")
scatter_plot_by_type_label_left(data_filtered_moist,"SF_ug_per_g_food_moist", "LF_AGE_ug_per_g_food_moist","Kibble")
scatter_plot_by_type_label_left(data_filtered_moist,"SF_ug_per_g_food_moist", "CML_plus_MG_g_moist","Kibble")
scatter_plot_by_type_label_left(data_filtered_moist,"LF_AGE_ug_per_g_food_moist", "CML_plus_MG_g_moist","Kibble")
scatter_plot_by_type_label_left(data_filtered_moist,"LF_AGE_ug_per_g_food_moist", "Combined_g_moist","Kibble")


##================================================================================================


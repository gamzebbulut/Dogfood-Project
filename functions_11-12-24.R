# only keep functions here for the R code for dogfood analysis
# data 11-12-2024

# make R remember the function.

#need function here:
#September 19 2024 update, make pearson --> spearman
# Define the function get help from chat gpt # this is like "fat option 3" (puts labels to the right)
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
      hjust = 1.1, vjust = 0.8, size = 10, color = "blue"
    ) +
    theme(
      axis.title.x = element_text(size = 22, face = "bold"),
      axis.title.y = element_text(size = 22, face = "bold"),
      axis.text.x = element_text(size = 16),
      axis.text.y = element_text(size = 16)
    )
  
  print(p)
}

#need function here:
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

#=================================================================
scatter_plot_by_type_label_leftp <- function(data, x, y, z = NULL) {
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
      label = paste0("\nSpearman r =", round(correlation, 2), "\np -value=", round(p_value, 4)),
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
#=================================================================
# how about we make a ginormous table of all correlations
## I need to make sure these spearman values are the same as above.
#this is a nice function but does not give exactly same p value as above functions so stop using it 11-12-24

# Load required libraries
library(readxl)
library(dplyr)
library(tidyverse)

# Load the dataset
data <- ELISA_and_fluorescence_restruc_GB1_norm_kcal_3CML_NAed_VJF_edits_moisture_normalization

# Remove Fresh foods
# data <- data[data$Type != 'Fresh', ]

data <- data %>%
  mutate(CML_plus_MG_g_moist = CML_ug_per_g_food_moist + MG_ug_per_g_food_moist) %>%
  mutate(Combined_g_moist= CML_ug_per_g_food_moist + MG_ug_per_g_food_moist + SF_ug_per_g_food_moist) 

# Ensure the relevant columns are numeric
columns_to_convert <- c('CML_ug_per_g_food_moist', 'MG_ug_per_g_food_moist', 'SF_ug_per_g_food_moist', 'LF_AGE_ug_per_g_food_moist', 
                        'CML_plus_MG_g_moist','Combined_g_moist',
                        'Percent_moisture', 'Percent_max_Crude_protein', 'Percent_max_Crude_fat', 'Percent_Crude_fiber')

data[columns_to_convert] <- lapply(data[columns_to_convert], function(x) as.numeric(as.character(x)))

# Remove rows with any NA values in the relevant columns
data <- data %>% drop_na(all_of(columns_to_convert))

# Revised Function 1
compute_correlations <- function(data, x, y) {
  data_x <- data[[x]]
  data_y <- data[[y]]
  
  # Filter out NA values
  valid_indices <- !is.na(data_x) & !is.na(data_y)
  data_x <- data_x[valid_indices]
  data_y <- data_y[valid_indices]
  
  # Check if there are enough observations
  if (length(data_x) > 2 & length(data_y) > 2) {
    cor_test <- cor.test(data_x, data_y, method = "spearman")
    correlation <- cor_test$estimate
    p_value <- cor_test$p.value
  } else {
    correlation <- NA
    p_value <- NA
  }
  
  return(data.frame(Variable1 = x, Variable2 = y, Correlation = correlation, P_Value = p_value))
}

# List of AGE measurements and ingredients
age_measurements <- c('CML_ug_per_g_food_moist', 'MG_ug_per_g_food_moist', 'SF_ug_per_g_food_moist', 'LF_AGE_ug_per_g_food_moist', 
                      'CML_plus_MG_g_moist', 'Combined_g_moist')
ingredients <- c('Percent_moisture', 'Percent_max_Crude_protein', 'Percent_max_Crude_fat', 'Percent_Crude_fiber')

# Initialize an empty data frame to store the results
results <- data.frame()

# Compute correlations for all food types
for (age_measure in age_measurements) {
  for (ingredient in ingredients) {
    print(paste("Computing correlation for:", age_measure, "and", ingredient))
    result <- compute_correlations(data, age_measure, ingredient)
    result$Type <- 'All'
    print(result)
    results <- rbind(results, result)
  }
}

# Compute correlations for canned food only
data_canned <- data %>% filter(Type == 'Canned') %>% drop_na(all_of(columns_to_convert))
for (age_measure in age_measurements) {
  for (ingredient in ingredients) {
    print(paste("Computing correlation for Canned:", age_measure, "and", ingredient))
    result <- compute_correlations(data_canned, age_measure, ingredient)
    result$Type <- 'Canned'
    print(result)
    results <- rbind(results, result)
  }
}

# Compute correlations for kibble food only
data_kibble <- data %>% filter(Type == 'Kibble') %>% drop_na(all_of(columns_to_convert))
for (age_measure in age_measurements) {
  for (ingredient in ingredients) {
    print(paste("Computing correlation for Kibble:", age_measure, "and", ingredient))
    result <- compute_correlations(data_kibble, age_measure, ingredient)
    result$Type <- 'Kibble'
    print(result)
    results <- rbind(results, result)
  }
}

# Save the results to a CSV file
write.csv(results, 'correlations_results11-12-24.csv', row.names = FALSE)

# Print the results
print(results)



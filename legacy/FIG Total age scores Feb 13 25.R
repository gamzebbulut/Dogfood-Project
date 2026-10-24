# Figure TOTAL AGE SCORES

# continue after running the "PRE PROCESSING FILE"

data <- data_filtered_moist %>%
  mutate(Total_AGE_Score_dry = CML_ug_per_g_food + MG_ug_per_g_food + LF_AGE_ug_per_g_food + SF_ug_per_g_food) %>%
  mutate(Total_AGE_Score_g_moist = CML_ug_per_g_food_moist + MG_ug_per_g_food_moist + LF_AGE_ug_per_g_food_moist + SF_ug_per_g_food_moist) %>%
  mutate(Total_AGE_Score_kcal_moist = CML_ug_per_kcal_food_moist + MG_ug_per_kcal_food_moist + LF_ug_per_kcal_food_moist + SF_ug_per_kcal_food_moist) 
  

# Function to generate individual plots
generate_plot <- function(data, measure, remove_title = FALSE) {
  data_long <- data %>%
    pivot_longer(cols = all_of(measure), names_to = "Measure", values_to = "Value") %>%
    mutate(Type = as.factor(Type)) # Ensure Type is treated as a factor
  
  # Perform Kruskal-Wallis test to get the overall p-value
  kruskal_result <- kruskal.test(Value ~ Type, data = data_long)
  p_value <- signif(kruskal_result$p.value, 3) # Round p-value to 3 significant digits
  
  # Create a custom subtitle showing only the p-value
  custom_subtitle <- paste("p-value:", p_value)
  
  # Generate plot
  ggbetweenstats(
    data = data_long,
    x = Type,
    y = Value,
    type = "nonparametric",
    pairwise.comparisons = TRUE, # Enable pairwise comparisons
    pairwise.annotation = "p.value", # Annotate pairwise comparisons with p-values
    pairwise.display = "significant", # Only show significant pairwise comparisons
    outlier.tagging = TRUE,
    mean.plotting = TRUE,
    mean.color = "red",
    mean.label.size = 12,
    title = if (!remove_title) paste("Distribution of", measure, "by Type") else NULL,
    xlab = NULL, # Remove x-axis label
    ylab = measure,
    violin.alpha = 0.8,
    violin.linewidth = 3, # Thicker violin plot lines
    boxplot.width = 0.5,
    boxplot.linewidth = 3, # Thicker boxplot lines
    jitter.size = 5,
    jitter.alpha = 0.8,
    ggtheme = theme_minimal() +
      theme(
        # Make text bold and readable
        axis.title.x = element_blank(), # Suppress x-axis label
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.subtitle = element_text(size = 14, face = "bold", hjust = 0.5) # Customize subtitle font
      ),
    p.adjust.method = "BH", # Apply Benjamini-Hochberg correction
    results.subtitle = FALSE # Suppress the default subtitle
  ) + 
    # Add custom subtitle with the overall p-value
    labs(subtitle = custom_subtitle) + 
    # Apply log2 transformation to the y-axis
    scale_y_continuous(trans = "log2")
}

Total_AGE_Score_dry <- c("Total_AGE_Score_dry")
Total_AGE_Score_g_moist <- c("Total_AGE_Score_g_moist")
Total_AGE_Score_kcal_moist <- c("Total_AGE_Score_kcal_moist")

#generate plot for dry weight
Total_ug_per_g_dry <- lapply(Total_AGE_Score_dry, function(measure) {
  generate_plot(data, measure, remove_title = TRUE) # Remove titles for top row
})[[1]]

# Generate plots for ug_per_g_measures_moist
Total_ug_per_g_moist <- lapply(Total_AGE_Score_g_moist, function(measure) {
  generate_plot(data, measure, remove_title = TRUE) # Remove titles for top row
})[[1]]

# Generate plots for ug_per_kcal_measures_moist
Total_kcal_moist <- lapply(Total_AGE_Score_kcal_moist, function(measure) {
  generate_plot(data, measure, remove_title = TRUE) # Remove titles for top row
})[[1]]

# Define the plots
# Combine all three plots into a single row
Total_row1 <- wrap_plots(Total_ug_per_g_dry, Total_ug_per_g_moist, Total_kcal_moist, ncol = 3)

# Save the grid to a file
ggsave("total_log2_scale_2-13-24.png", Total_row1, width = 10, height = 4)

## obtain FDR corrected p values from fig 1 code, and then manually edit on illustrator.


## need to run this for both fig 1 and Fig 4 and total box plots to manually correct p values on illustrator.


# Define the vector of extracted p-values
#these are what I typed based on images of box-violin plots on fig1, LF fig and total figure, 
# if the data changes you will need to type these p values again
p_values <- c(
  3.88e-06, 0.000158, 3.96e-08, 0.765,
  0.258, 0.0227, 0.224, 2.84e-16,
  1.04e-07, 2.01e-05, 1.18e-09, 0.00144,
  3.12e-08, 9.41e-17, 0.000146, 4.06e-09, 3.07e-17, 0.0023
)

# Apply FDR correction (Benjamini-Hochberg method)
adjusted_p_values <- p.adjust(p_values, method = "fdr")

# Create a data frame with original and adjusted p-values
results <- data.frame(
  Original_P = p_values,
  Adjusted_P = adjusted_p_values
)

# Save as CSV file
write.csv(results, "FDR_corrected_p_values_boxplotsFeb1325.csv", row.names = FALSE)

# Print results to console
print(results)
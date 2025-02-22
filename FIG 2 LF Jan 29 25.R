## Figure 4
# Jan 29 2025

# continue after the pre processing code

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

# Define LF measure columns

LF_ug_per_g_measures <- c("LF_AGE_ug_per_g_food")

LF_ug_per_g_measures_moist <- c("LF_AGE_ug_per_g_food_moist")

LF_ug_per_kcal_measures_moist <- c("LF_ug_per_kcal_food_moist")

# Generate plots for ug_per_g_measures
LF_plot_dry <- lapply(LF_ug_per_g_measures, function(measure) {
  generate_plot(data_filtered_moist, measure, remove_title = TRUE)
})[[1]]  # Extract the first plot (since there's only one)

LF_plot_moist <- lapply(LF_ug_per_g_measures_moist, function(measure) {
  generate_plot(data_filtered_moist, measure, remove_title = TRUE)
})[[1]]

# Generate plot for ug_per_kcal_measures_moist
LF_plot_per_kcal <- lapply(LF_ug_per_kcal_measures_moist, function(measure) {
  generate_plot(data_filtered_moist, measure, remove_title = TRUE)
})[[1]]

# Combine all three plots into a single row
LF_row1 <- wrap_plots(LF_plot_dry, LF_plot_moist, LF_plot_per_kcal, ncol = 3)

# Save the grid to a file
ggsave("LF_log2_scale_12-18-24.png", LF_row1, width = 10, height = 4)

## obtain FDR corrected p values from fig 1 code, and then manually edit on illustrator.

#here add the scatter plots for the bottom
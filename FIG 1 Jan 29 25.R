# Figure 1

# continue after running the "PRE PROCESSING FILE"

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


#generate plot for dry weight
plots_ug_per_g_dry <- lapply(ug_per_g_measures_dry_weight, function(measure) {
  generate_plot(data_filtered_moist, measure, remove_title = TRUE) # Remove titles for top row
})

# Generate plots for ug_per_g_measures_moist
plots_ug_per_g <- lapply(ug_per_g_measures_moist, function(measure) {
  generate_plot(data_filtered_moist, measure, remove_title = TRUE) # Remove titles for top row
})

# Generate plots for ug_per_kcal_measures_moist
plots_ug_per_kcal <- lapply(ug_per_kcal_measures_moist, function(measure) {
  generate_plot(data_filtered_moist, measure, remove_title = TRUE)
})

# Define the plots
row1_dry <- wrap_plots(plots_ug_per_g_dry, ncol = 4)
row2_ug <- wrap_plots(plots_ug_per_g, ncol = 4)
row3_kcal <- wrap_plots(plots_ug_per_kcal, ncol = 4)

# Save the grid to a file
ggsave("plots_ug_per_g_dry.png", row1_dry, width = 16, height = 4)
ggsave("plots_ug_per_g.png", row2_ug , width = 16, height = 4)
ggsave("plots_ug_per_kcal.png", row3_kcal, width = 16, height = 4)


## need to run this for both fig 1 and Fig 4 box plots to manually correct p values on illustrator.


# Define the vector of extracted p-values
#these are what I typed based on above image, if the daya changes you will need to type these p values again
p_values <- c(
  3.88e-06, 0.000158, 3.96e-08, 0.765,
  0.258, 0.0227, 0.224, 2.84e-16,
  1.04e-07, 2.01e-05, 1.18e-09, 0.00144,
  3.12e-08, 2.15e-17, 0.000146
)

# Apply FDR correction (Benjamini-Hochberg method)
adjusted_p_values <- p.adjust(p_values, method = "fdr")

# Create a data frame with original and adjusted p-values
results <- data.frame(
  Original_P = p_values,
  Adjusted_P = adjusted_p_values
)

# Save as CSV file
write.csv(results, "FDR_corrected_p_values_boxplots.csv", row.names = FALSE)

# Print results to console
print(results)
###======================FIGURE 5 ======================================
#run preprocessing first.

#================ december 24 2024
data <- data_filtered_moist

# Ensure 'DogFoodLabel2' includes the type for labeling
data <- data %>%
  mutate(DogFoodLabel2 = paste(ID, "(", Type, ")", sep = ""))

data <- data %>%
  mutate(Total_AGE_Score_g = CML_ug_per_g_food_moist + MG_ug_per_g_food_moist + LF_AGE_ug_per_g_food_moist + SF_ug_per_g_food_moist)


# Define custom colors
age_colors <- c(
  "CML_ug_per_g_food_moist" = "red",
  "MG_ug_per_g_food_moist" = "blue",
  "LF_AGE_ug_per_g_food_moist" = "darkgreen",
  "SF_ug_per_g_food_moist" = "darkgray"
)

# Step 1: Original Stacked Plot
data_long <- data %>%
  arrange(Total_AGE_Score_g) %>%
  mutate(DogFoodLabel2 = factor(DogFoodLabel2, levels = DogFoodLabel2)) %>%
  select(DogFoodLabel2, CML_ug_per_g_food_moist, MG_ug_per_g_food_moist, LF_AGE_ug_per_g_food_moist, SF_ug_per_g_food_moist) %>%
  pivot_longer(
    cols = c(CML_ug_per_g_food_moist, MG_ug_per_g_food_moist, LF_AGE_ug_per_g_food_moist, SF_ug_per_g_food_moist),
    names_to = "AGE_Measure",
    values_to = "Value"
  )

stacked_plot <- ggplot(data_long, aes(x = DogFoodLabel2, y = Value, fill = AGE_Measure)) +
  geom_bar(stat = "identity") +
  labs(title = "Total AGE Score for Dog Foods", x = "Dog Food (Type)", y = "Total AGE Score", fill = "AGE Measure") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  ) +
  scale_fill_manual(values = age_colors)

# Step 2: Red CML Plot
data_cml <- data %>%
  arrange(CML_ug_per_g_food_moist) %>%
  mutate(DogFoodLabel2 = factor(DogFoodLabel2, levels = DogFoodLabel2))

cml_plot <- ggplot(data_cml, aes(x = DogFoodLabel2, y = CML_ug_per_g_food_moist)) +
  geom_bar(stat = "identity", fill = age_colors["CML_ug_per_g_food_moist"]) +
  labs(title = "CML AGE Measure", x = "Dog Food (Type)", y = "CML (ug/g)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14)
  )

# Step 3: Reorder Remaining AGE Measures
data_mg <- data_cml %>%
  select(DogFoodLabel2, MG_ug_per_g_food_moist)

data_sf <- data_cml %>%
  select(DogFoodLabel2, SF_ug_per_g_food_moist)

data_lf <- data_cml %>%
  select(DogFoodLabel2, LF_AGE_ug_per_g_food_moist)

mg_plot <- ggplot(data_mg, aes(x = DogFoodLabel2, y = MG_ug_per_g_food_moist)) +
  geom_bar(stat = "identity", fill = age_colors["MG_ug_per_g_food_moist"]) +
  labs(title = "MG AGE Measure", x = "Dog Food (Type)", y = "MG (ug/g)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14)
  )

sf_plot <- ggplot(data_sf, aes(x = DogFoodLabel2, y = SF_ug_per_g_food_moist)) +
  geom_bar(stat = "identity", fill = age_colors["SF_ug_per_g_food_moist"]) +
  labs(title = "SF AGE Measure", x = "Dog Food (Type)", y = "SF (ug/g)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14)
  )

lf_plot <- ggplot(data_lf, aes(x = DogFoodLabel2, y = LF_AGE_ug_per_g_food_moist)) +
  geom_bar(stat = "identity", fill = age_colors["LF_AGE_ug_per_g_food_moist"]) +
  labs(title = "LF AGE Measure", x = "Dog Food (Type)", y = "LF (ug/g)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14)
  )

# Step 4: Combine Plots
combined_plot <- stacked_plot / cml_plot / mg_plot / sf_plot / lf_plot + 
  plot_layout(heights = c(2, 1, 1, 1, 1))

# Save the Combined Plot
ggsave("figure_6_combined_AGE_plots_with_colors.png", combined_plot, width = 16, height = 20)

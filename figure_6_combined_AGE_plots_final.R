library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)  # For combining plots

# ====== Load Your Data (replace this with your file loading step) ======


# ====== Compute Total AGE Score per kcal ======
data <- data_filtered_13 %>%
  mutate(
    DogFoodLabel = ID,  # Use just the ID number for x-axis
    Total_AGE_Score_kcal = CML_ug_per_kcal_food_moist +
      MG_ug_per_kcal_food_moist +
      LF_ug_per_kcal_food_moist +
      SF_ug_per_kcal_food_moist
  )

# ====== Define Custom Colors ======
age_colors <- c(
  "CML_ug_per_kcal_food_moist" = "red",
  "MG_ug_per_kcal_food_moist" = "gray",
  "LF_ug_per_kcal_food_moist" = "darkgreen",
  "SF_ug_per_kcal_food_moist" = "blue"
)

# ====== Step 1: Stacked Bar Plot in Total AGE Score Order ======
data_long <- data %>%
  arrange(Total_AGE_Score_kcal) %>%
  mutate(DogFoodLabel = factor(DogFoodLabel, levels = DogFoodLabel)) %>%
  select(DogFoodLabel,
         CML_ug_per_kcal_food_moist,
         MG_ug_per_kcal_food_moist,
         LF_ug_per_kcal_food_moist,
         SF_ug_per_kcal_food_moist) %>%
  pivot_longer(cols = -DogFoodLabel, names_to = "AGE_Measure", values_to = "Value")


stacked_plot <- ggplot(data_long, aes(x = DogFoodLabel, y = Value, fill = AGE_Measure)) +
  geom_bar(stat = "identity") +
  labs(title = "Total AGE Score (ug/kcal)", x = "Dog Food ID", y = "ug/kcal") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = age_colors)

# ====== Step 2: Individual AGE Plots in Same Order ======
plot_single_age <- function(var, color, label) {
  ggplot(data %>%
           arrange(Total_AGE_Score_kcal) %>%
           mutate(DogFoodLabel = factor(DogFoodLabel, levels = DogFoodLabel)),
         aes(x = DogFoodLabel, y = .data[[var]])) +
    geom_bar(stat = "identity", fill = color) +
    labs(title = paste(label, "AGE Measure"), x = "Dog Food ID", y = "ug/kcal") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

cml_plot <- plot_single_age("CML_ug_per_kcal_food_moist", age_colors["CML_ug_per_kcal_food_moist"], "CML")
mg_plot  <- plot_single_age("MG_ug_per_kcal_food_moist", age_colors["MG_ug_per_kcal_food_moist"], "MG")
sf_plot  <- plot_single_age("SF_ug_per_kcal_food_moist", age_colors["SF_ug_per_kcal_food_moist"], "SF")
lf_plot  <- plot_single_age("LF_ug_per_kcal_food_moist", age_colors["LF_ug_per_kcal_food_moist"], "LF")

# ====== Step 3: Combine All Plots Vertically ======
combined_plot <- stacked_plot / cml_plot / mg_plot / sf_plot / lf_plot +
  plot_layout(heights = c(2, 1, 1, 1, 1))

# ====== Save the Plot ======
ggsave("figure_6_combined_AGE_plots_final.png", combined_plot, width = 16, height = 20)

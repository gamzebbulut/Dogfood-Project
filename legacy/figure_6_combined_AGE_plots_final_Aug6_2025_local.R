library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)  # For combining plots

# ====== Load Your Data (replace this with your file loading step) ======
#ran pre processing filejan 29 

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
ggsave("figure_6_combined_AGE_plots_finalAugust6save.png", combined_plot, width = 16, height = 20)


# ====== Step 1: Calculate CVs for Each AGE Measure ======
library(dplyr)

# Set up AGE variables for each scale
dry_columns <- c("CML_ug_per_g_food", "MG_ug_per_g_food", "LF_AGE_ug_per_g_food", "SF_ug_per_g_food")
asfed_columns <- c("CML_ug_per_g_food_moist", "MG_ug_per_g_food_moist", "LF_AGE_ug_per_g_food_moist", "SF_ug_per_g_food_moist")
kcal_columns <- c("CML_ug_per_kcal_food_moist", "MG_ug_per_kcal_food_moist", "LF_ug_per_kcal_food_moist", "SF_ug_per_kcal_food_moist")

# Function to calculate CV table
calculate_cv <- function(data, columns, scale_label) {
  tibble(
    AGE_Measure = columns
  ) %>%
    rowwise() %>%
    mutate(
      Mean = round(mean(data[[AGE_Measure]], na.rm = TRUE), 3),
      SD   = round(sd(data[[AGE_Measure]], na.rm = TRUE), 3),
      CV   = round((SD / Mean) * 100, 1),
      Scale = scale_label
    )
}

# Calculate CVs
cv_dry <- calculate_cv(data_filtered_13, dry_columns, "Dry Matter (ug/g)")
cv_asfed <- calculate_cv(data_filtered_13, asfed_columns, "As Fed (ug/g)")
cv_kcal <- calculate_cv(data_filtered_13, kcal_columns, "As Fed (ug/kcal)")

# Combine all together
cv_all <- bind_rows(cv_dry, cv_asfed, cv_kcal) %>%
  mutate(CV = round(CV, 1)) %>%
  select(Scale, AGE_Measure, CV)

# View result
print(cv_all)

# ====== Step 4: Calculate Combined Fluorescence CVs ======

# Total Fluorescence: Dry Matter
total_fluor_dry <- data_filtered_13$LF_AGE_ug_per_g_food + data_filtered_13$SF_ug_per_g_food
mean_total_dry <- mean(total_fluor_dry, na.rm = TRUE)
sd_total_dry <- sd(total_fluor_dry, na.rm = TRUE)
cv_total_dry <- (sd_total_dry / mean_total_dry) * 100

# Total Fluorescence: As Fed (ug/g)
total_fluor_asfed <- data_filtered_13$LF_AGE_ug_per_g_food_moist + data_filtered_13$SF_ug_per_g_food_moist
mean_total_asfed <- mean(total_fluor_asfed, na.rm = TRUE)
sd_total_asfed <- sd(total_fluor_asfed, na.rm = TRUE)
cv_total_asfed <- (sd_total_asfed / mean_total_asfed) * 100

# Total Fluorescence: As Fed (ug/kcal)
total_fluor_kcal <- data_filtered_13$LF_ug_per_kcal_food_moist + data_filtered_13$SF_ug_per_kcal_food_moist
mean_total_kcal <- mean(total_fluor_kcal, na.rm = TRUE)
sd_total_kcal <- sd(total_fluor_kcal, na.rm = TRUE)
cv_total_kcal <- (sd_total_kcal / mean_total_kcal) * 100

# ====== Step 5: Create Combined Fluorescence Table ======

cv_total_fluor <- tibble(
  Scale = c("Dry Matter (ug/g)", "As Fed (ug/g)", "As Fed (ug/kcal)"),
  AGE_Measure = "Total Fluorescent AGEs",
  CV = round(c(cv_total_dry, cv_total_asfed, cv_total_kcal), 1)
)

# ====== Step 6: Append Combined Fluorescence to the Main Table ======

cv_all_final <- bind_rows(cv_all, cv_total_fluor)

# View result
print(cv_all_final)


######### update August 6 2025 Daves req to split canned and kibble

# 🛠 Function to calculate full CV set for a given subset
calculate_all_cv <- function(data_subset, label) {
  cv_dry <- calculate_cv(data_subset, dry_columns, "Dry Matter (ug/g)")
  cv_asfed <- calculate_cv(data_subset, asfed_columns, "As Fed (ug/g)")
  cv_kcal <- calculate_cv(data_subset, kcal_columns, "As Fed (ug/kcal)")
  
  # Combine single AGEs
  cv_all <- bind_rows(cv_dry, cv_asfed, cv_kcal) %>%
    mutate(CV = round(CV, 1), Group = label)
  
  # Add Total Fluorescent AGEs
  total_fluor_dry   <- data_subset$LF_AGE_ug_per_g_food + data_subset$SF_ug_per_g_food
  total_fluor_asfed <- data_subset$LF_AGE_ug_per_g_food_moist + data_subset$SF_ug_per_g_food_moist
  total_fluor_kcal  <- data_subset$LF_ug_per_kcal_food_moist + data_subset$SF_ug_per_kcal_food_moist
  
  # Add Total Fluorescent AGEs (with rounded Mean + SD!)
  mean_total_dry   <- mean(total_fluor_dry, na.rm = TRUE)
  sd_total_dry     <- sd(total_fluor_dry, na.rm = TRUE)
  cv_total_dry     <- (sd_total_dry / mean_total_dry) * 100
  
  mean_total_asfed <- mean(total_fluor_asfed, na.rm = TRUE)
  sd_total_asfed   <- sd(total_fluor_asfed, na.rm = TRUE)
  cv_total_asfed   <- (sd_total_asfed / mean_total_asfed) * 100
  
  mean_total_kcal  <- mean(total_fluor_kcal, na.rm = TRUE)
  sd_total_kcal    <- sd(total_fluor_kcal, na.rm = TRUE)
  cv_total_kcal    <- (sd_total_kcal / mean_total_kcal) * 100
  
  cv_total_fluor <- tibble(
    AGE_Measure = "Total Fluorescent AGEs",
    Mean = round(c(mean_total_dry, mean_total_asfed, mean_total_kcal), 3),
    SD   = round(c(sd_total_dry, sd_total_asfed, sd_total_kcal), 3),
    CV   = round(c(cv_total_dry, cv_total_asfed, cv_total_kcal), 1),
    Scale = c("Dry Matter (ug/g)", "As Fed (ug/g)", "As Fed (ug/kcal)"),
    Group = label
  )
  
  bind_rows(cv_all, cv_total_fluor)
}

# 🥫 Calculate CVs for Canned
cv_canned <- calculate_all_cv(filter(data_filtered_13, Type == "Canned"), "Canned")

# 🥣 Calculate CVs for Kibble
cv_kibble <- calculate_all_cv(filter(data_filtered_13, Type == "Kibble"), "Kibble")

# 📊 Final Combined Table
cv_combined <- bind_rows(cv_canned, cv_kibble)

# 🖨️ Print or save
print(cv_combined)

# Optional: Save to CSV
write.csv(cv_combined, "CV_Table_Canned_vs_KibbleAug6.csv", row.names = FALSE)


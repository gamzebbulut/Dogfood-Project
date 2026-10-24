# Load Required Libraries
library(tidyverse)
library(knitr)
library(kableExtra)
library(webshot2)
library(dplyr)

# Preprocessing: Compute New Metrics
data_filtered_moist_ss <- data_filtered_moist %>%
  mutate(CML_AGE_per20kg_dog_per_day = CML_ug_per_kcal_food_moist * 629.82) %>%
  mutate(MG_AGE_per20kg_dog_per_day = MG_ug_per_kcal_food_moist * 629.82) %>%
  mutate(CMLplusMG_AGE_per20kg_dog_per_day = CML_plus_MG_kcal_moist * 629.82) %>%
  mutate(SF_AGE_per20kg_dog_per_day = SF_ug_per_kcal_food_moist * 629.82) %>%
  mutate(Combined_AGE_per20kg_dog_per_day = Combined_kcal_moist * 629.82) %>%
  mutate(LF_AGE_per20kg_dog_per_day = LF_ug_per_kcal_food_moist * 629.82)

# Define AGE Measures
age_columns_ss <- c(
  "CML_ug_per_g_food", "MG_ug_per_g_food", "CML_plus_MG_g", "SF_ug_per_g_food", "Combined_g", "LF_AGE_ug_per_g_food",
  "CML_ug_per_g_food_moist", "MG_ug_per_g_food_moist", "CML_plus_MG_g_moist", "SF_ug_per_g_food_moist", 
  "Combined_g_moist", "LF_AGE_ug_per_g_food_moist", "CML_ug_per_kcal_food_moist", "MG_ug_per_kcal_food_moist", 
  "CML_plus_MG_kcal_moist", "SF_ug_per_kcal_food_moist", "Combined_kcal_moist", "LF_ug_per_kcal_food_moist", 
  "CML_AGE_per20kg_dog_per_day", "MG_AGE_per20kg_dog_per_day", "CMLplusMG_AGE_per20kg_dog_per_day", 
  "SF_AGE_per20kg_dog_per_day", "Combined_AGE_per20kg_dog_per_day", "LF_AGE_per20kg_dog_per_day"
)

# Step 1: Compute Summary Statistics
summary_stats <- data_filtered_moist_ss %>%
  select(Type, all_of(age_columns_ss)) %>%
  group_by(Type) %>%
  summarise(across(
    everything(),
    list(
      min = ~min(., na.rm = TRUE),
      Q1 = ~quantile(., 0.25, na.rm = TRUE),
      median = ~median(., na.rm = TRUE),
      Q3 = ~quantile(., 0.75, na.rm = TRUE),
      max = ~max(., na.rm = TRUE)
    ),
    .names = "{col}_{fn}"
  )) 

# Step 2: Reshape to Long Format
reshaped_stats <- summary_stats %>%
  pivot_longer(
    cols = -Type,
    names_to = c("Measure", "Statistic"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  pivot_wider(
    names_from = Type,
    values_from = value
  )

# Step 3: Define Normalization Types
reshaped_stats <- reshaped_stats %>%
  mutate(
    Normalization = case_when(
      Measure %in% c("CML_ug_per_g_food", "MG_ug_per_g_food", "CML_plus_MG_g",
                     "SF_ug_per_g_food", "Combined_g", "LF_AGE_ug_per_g_food") ~ "Dry Weight (ug/g)",
      Measure %in% c("CML_ug_per_g_food_moist", "MG_ug_per_g_food_moist", "CML_plus_MG_g_moist",
                     "SF_ug_per_g_food_moist", "Combined_g_moist", "LF_AGE_ug_per_g_food_moist") ~ "As Fed (ug/g)",
      Measure %in% c("CML_ug_per_kcal_food_moist", "MG_ug_per_kcal_food_moist", "CML_plus_MG_kcal_moist",
                     "SF_ug_per_kcal_food_moist", "Combined_kcal_moist", "LF_ug_per_kcal_food_moist") ~ "As Fed kcal",
      Measure %in% c("CML_AGE_per20kg_dog_per_day", "MG_AGE_per20kg_dog_per_day", "CMLplusMG_AGE_per20kg_dog_per_day",
                     "SF_AGE_per20kg_dog_per_day", "Combined_AGE_per20kg_dog_per_day", "LF_AGE_per20kg_dog_per_day") ~ "AGEs consumed by 20 kg dog per day",
      TRUE ~ "Other"
    )
  )

# Step 4: Format and Round Numbers
formatted_stats <- reshaped_stats %>%
  pivot_wider(
    names_from = Statistic,
    values_from = c(Canned, Kibble),
    names_glue = "{.value}_{Statistic}"
  ) %>%
  mutate(
    `Canned Median (IQR)` = paste0(round(Canned_median, 2), 
                                   " (", round(Canned_Q1, 2), "–", round(Canned_Q3, 2), ")"),
    `Kibble Median (IQR)` = paste0(round(Kibble_median, 2), 
                                   " (", round(Kibble_Q1, 2), "–", round(Kibble_Q3, 2), ")"),
    `Min-Max Canned` = paste0(round(Canned_min, 2), "–", round(Canned_max, 2)),
    `Min-Max Kibble` = paste0(round(Kibble_min, 2), "–", round(Kibble_max, 2))
  ) %>%
  select(Normalization, Measure, `Canned Median (IQR)`, `Kibble Median (IQR)`, `Min-Max Canned`, `Min-Max Kibble`) %>%
  arrange(Measure)

# Step 5: Transpose Table: Normalization Types as Columns
transposed_stats <- formatted_stats %>%
  pivot_wider(
    names_from = Normalization,   
    values_from = c(`Canned Median (IQR)`, `Kibble Median (IQR)`, 
                    `Min-Max Canned`, `Min-Max Kibble`),
    names_glue = "{Normalization}_{.value}"
  ) %>%
  arrange(Measure)  

# Step 6: Generate Transposed Table
ft_transposed <- transposed_stats %>%
  kbl(
    caption = "Summary Statistics for AGE Measures by Type and Normalization",
    col.names = c(
      "Measure", 
      "AGEs consumed by 20 kg dog per day (Canned Median IQR)",
      "Dry Weight (ug/g) (Canned Median IQR)",
      "As Fed (ug/g) (Canned Median IQR)",
      "As Fed kcal (Canned Median IQR)",
      "AGEs consumed by 20 kg dog per day (Kibble Median IQR)",
      "Dry Weight (ug/g) (Kibble Median IQR)",
      "As Fed (ug/g) (Kibble Median IQR)",
      "As Fed kcal (Kibble Median IQR)",
      "AGEs consumed by 20 kg dog per day (Min-Max Canned)",
      "Dry Weight (ug/g) (Min-Max Canned)",
      "As Fed (ug/g) (Min-Max Canned)",
      "As Fed kcal (Min-Max Canned)",
      "AGEs consumed by 20 kg dog per day (Min-Max Kibble)",
      "Dry Weight (ug/g) (Min-Max Kibble)",
      "As Fed (ug/g) (Min-Max Kibble)",
      "As Fed kcal (Min-Max Kibble)"
    )  # Exact match with 17 columns
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE, font_size = 12)


# View the Final Transposed Table
ft_transposed

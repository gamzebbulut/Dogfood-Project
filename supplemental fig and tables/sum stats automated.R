library(tidyverse)
library(knitr)
library(kableExtra)
#summary statistics
# Required Libraries
library(tidyverse)
library(knitr)
library(kableExtra)
# install.packages("webshot2")
library(webshot2)
library(dplyr)

#Run pre processing first.

# create new metrics
data_filtered_moist_ss <- data_filtered_moist %>%
  mutate(CML_AGE_per20kg_dog_per_day = CML_ug_per_kcal_food_moist*629.82) %>%
  mutate(MG_AGE_per20kg_dog_per_day = MG_ug_per_kcal_food_moist*629.82) %>%
  mutate(CMLplusMG_AGE_per20kg_dog_per_day = CML_plus_MG_kcal_moist*629.82) %>%
  mutate(SF_AGE_per20kg_dog_per_day = SF_ug_per_kcal_food_moist*629.82) %>%
  mutate(Combined_AGE_per20kg_dog_per_day = Combined_kcal_moist*629.82) %>%
  mutate(LF_AGE_per20kg_dog_per_day = LF_ug_per_kcal_food_moist*629.82)

# Define all AGE measures
age_columns_ss <- c(
  "CML_ug_per_g_food", "MG_ug_per_g_food", "CML_plus_MG_g", "SF_ug_per_g_food", "Combined_g", "LF_AGE_ug_per_g_food",
  "CML_ug_per_g_food_moist", "MG_ug_per_g_food_moist", "CML_plus_MG_g_moist", "SF_ug_per_g_food_moist", 
  "Combined_g_moist", "LF_AGE_ug_per_g_food_moist", "CML_ug_per_kcal_food_moist", "MG_ug_per_kcal_food_moist", 
  "CML_plus_MG_kcal_moist", "SF_ug_per_kcal_food_moist", "Combined_kcal_moist", "LF_ug_per_kcal_food_moist", 
  "CML_AGE_per20kg_dog_per_day" , "MG_AGE_per20kg_dog_per_day" , "CMLplusMG_AGE_per20kg_dog_per_day" , 
  "SF_AGE_per20kg_dog_per_day" , "Combined_AGE_per20kg_dog_per_day" , "LF_AGE_per20kg_dog_per_day"
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

# Step 4: Format Median (IQR) and Min-Max values
formatted_stats <- reshaped_stats %>%
  pivot_wider(
    names_from = Statistic,
    values_from = c(Canned, Kibble)
  ) %>%
  mutate(
    `Canned` = paste0(median, " (", Q1, "–", Q3, ")"),
    `Kibble` = paste0(median, " (", Q1, "–", Q3, ")"),
    `Min-Max Canned` = paste0(min, "–", max),
    `Min-Max Kibble` = paste0(min, "–", max)
  ) %>%
  select(Normalization, Measure, `Canned`, `Kibble`, `Min-Max Canned`, `Min-Max Kibble`) %>%
  arrange(Normalization, Measure)

# Step 5: Display Table in R
ft <- formatted_stats %>%
  kbl(caption = "Summary Statistics for AGE Measures by Type and Normalization") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE, font_size = 12) %>%
  pack_rows("Dry Weight (ug/g)", 1, max(which(formatted_stats$Normalization == "Dry Weight (ug/g)"))) %>%
  pack_rows("As Fed (ug/g)", min(which(formatted_stats$Normalization == "As Fed (ug/g)")), max(which(formatted_stats$Normalization == "As Fed (ug/g)"))) %>%
  pack_rows("As Fed kcal", min(which(formatted_stats$Normalization == "As Fed kcal")), max(which(formatted_stats$Normalization == "As Fed kcal"))) %>%
  pack_rows("AGEs consumed by 20 kg dog per day", min(which(formatted_stats$Normalization == "AGEs consumed by 20 kg dog per day")), max(which(formatted_stats$Normalization == "AGEs consumed by 20 kg dog per day")))

# View Final Table
ft

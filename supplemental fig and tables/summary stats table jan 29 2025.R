### summary statistics jan 29 2025

#summary statistics
# Required Libraries
library(tidyverse)
library(knitr)
library(kableExtra)
# install.packages("webshot2")
library(webshot2)
library(dplyr)


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

# Calculate summary statistics, including IQR
summary_stats <- data_filtered_moist_ss %>%
  select(Type, all_of(age_columns_ss)) %>%
  group_by(Type) %>%
  summarise(across(
    everything(),
    list(
      min = ~min(., na.rm = TRUE),
      Q1 = ~quantile(., 0.25, na.rm = TRUE),  # 25th percentile
      median = ~median(., na.rm = TRUE),
      Q3 = ~quantile(., 0.75, na.rm = TRUE),  # 75th percentile
      max = ~max(., na.rm = TRUE),
      mean = ~mean(., na.rm = TRUE),
      sd = ~sd(., na.rm = TRUE)
    ),
    .names = "{col}_{fn}"
  ))

# Reshape the data for easier reporting
reshaped_stats <- summary_stats %>%
  pivot_longer(
    cols = -c(Type),  # Keep 'Type' fixed
    names_to = c("Measure", "Statistic"),  # Separate columns into Measure and Statistic
    names_pattern = "(.*)_(.*)"  # Split by the underscore separator
  ) %>%
  pivot_wider(
    names_from = Type,  # Spread by 'Type' (e.g., Canned, Kibble)
    values_from = value
  )

# Add Normalization Types with Explicit Mapping
reshaped_stats <- reshaped_stats %>%
  mutate(
    Normalization = case_when(
      Measure %in% c("CML_ug_per_g_food", "MG_ug_per_g_food", "CML_plus_MG_g", 
                     "SF_ug_per_g_food", "Combined_g", "LF_AGE_ug_per_g_food") ~ "Dry Weight (ug/g)",
      Measure %in% c("CML_ug_per_g_food_moist", "MG_ug_per_g_food_moist", "CML_plus_MG_g_moist", 
                     "SF_ug_per_g_food_moist", "Combined_g_moist", "LF_AGE_ug_per_g_food_moist") ~ "As Fed (ug/g)",
      Measure %in% c("CML_ug_per_kcal_food_moist", "MG_ug_per_kcal_food_moist", "CML_plus_MG_kcal_moist", 
                     "SF_ug_per_kcal_food_moist", "Combined_kcal_moist", "LF_ug_per_kcal_food_moist") ~ "As Fed kcal",
      Measure %in% c("CML_AGE_per20kg_dog_per_day" , "MG_AGE_per20kg_dog_per_day" , "CMLplusMG_AGE_per20kg_dog_per_day" , 
                     "SF_AGE_per20kg_dog_per_day" , "Combined_AGE_per20kg_dog_per_day" , "LF_AGE_per20kg_dog_per_day") ~ "AGEs consumed by 20 kg dog per day",
      TRUE ~ "Other"
    )
  )

# Apply rounding
reshaped_stats <- reshaped_stats %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

# Generate the Table with Proper Header Alignment
ft <- reshaped_stats %>%
  select(Normalization, Measure, Statistic, Canned, Kibble) %>%
  arrange(Normalization, Measure) %>%
  kbl(
    caption = "Summary Statistics for AGE Measures by Type and Normalization",
    col.names = c("Normalization", "Measure", "Statistic", "Canned", "Kibble")
  ) %>%
  add_header_above(c(
    " " = 3,  # Empty columns for Normalization, Measure, Statistic
    "Food Type" = 2  # Two columns for Canned and Kibble
  )) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    font_size = 12
  )

# Add Packed Rows for Each Normalization
ft <- ft %>%
  pack_rows(
    "Dry Weight (ug/g)", 
    1, 
    max(which(reshaped_stats$Normalization == "Dry Weight (ug/g)"))
  ) %>%
  pack_rows(
    "As Fed (ug/g)", 
    min(which(reshaped_stats$Normalization == "As Fed (ug/g)")), 
    max(which(reshaped_stats$Normalization == "As Fed (ug/g)"))
  ) %>%
  pack_rows(
    "As Fed kcal", 
    min(which(reshaped_stats$Normalization == "As Fed kcal")), 
    max(which(reshaped_stats$Normalization == "As Fed kcal"))
  )

# View the Table
ft

# Optionally Save to Word Document with flextable
ft_flex <- flextable(reshaped_stats) %>%
  set_header_labels(
    Normalization = "Normalization",
    Measure = "Measure",
    Statistic = "Statistic",
    Canned = "Canned",
    Kibble = "Kibble"
  ) %>%
  theme_vanilla() %>%
  autofit()

doc <- read_docx() %>%
  body_add_flextable(ft_flex)

# Save the document
print(doc, target = "summary_statistics_with_iqr.docx")

#this table still needs a lot of reformating by hand.
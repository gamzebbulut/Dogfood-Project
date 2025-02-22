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

# Save as CSV
write_csv(data_filtered_moist_ss, "data_filtered_moist_ss.csv")

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


###### ATTTENTION ##########################
#here open the ft table,click zoom then put cursor in, 
#then control A control C, open excel and control V, tricky.sometimes it pastes as a string, 
#sometimes a table. then "reformat by hand" until you get to something like final format first tab. 
# Then ask code pilot to move (iqr) next to median. 

#### BE CAREFUL, THE ORDER OF MEASURES IN AGES CONSUMED BY 20 KG DOG IS DIFFERENT FROM OTHERS, DO NOT ASSUME. 
## CML AND CML+MG GETS MIXED UP OTHER WISE.#######

# Load Required Libraries
library(tidyverse)
library(knitr)
library(kableExtra)
library(readr)
library(writexl)

# Step 1: Load Data

## Go to the correct working directory where the file is
# dogfood project > attem 12 > supp fig > making a summ stats table

input_file <- "summ stats raw output to reformat.csv"
df <- read_csv(input_file)

# Step 2: Ensure Proper Column Names
colnames(df) <- c("Normalization", "Measure", "Statistic", "Canned", "Kibble")
df

df_wide <- df %>%
  pivot_wider(
    names_from = "Statistic",
    values_from = c("Canned", "Kibble"),
    names_glue = "{.value}_{Statistic}"  # Correct order for names
  )
df_wide

#step 4

df_wide <- df_wide %>%
  mutate(
    `Canned Median (IQR)` = paste0(round(Canned_median, 2), 
                                   " (", round(Canned_Q1, 2), "–", round(Canned_Q3, 2), ")"),
    `Kibble Median (IQR)` = paste0(round(Kibble_median, 2), 
                                   " (", round(Kibble_Q1, 2), "–", round(Kibble_Q3, 2), ")"),
    `Min-Max Canned` = paste0(round(Canned_min, 2), "–", round(Canned_max, 2)),
    `Min-Max Kibble` = paste0(round(Kibble_min, 2), "–", round(Kibble_max, 2))
  ) %>%
  select(Normalization, Measure, `Canned Median (IQR)`, `Kibble Median (IQR)`, `Min-Max Canned`, `Min-Max Kibble`) %>%
  arrange(Normalization, Measure)


# Step 5: Save Processed Data as CSV & Excel
write_csv(df_wide, "formatted_summary_stats.csv")
write_xlsx(df_wide, "formatted_summary_stats.xlsx")

# Step 6: Generate Final Table (No Transpose)
ft <- df_wide %>%
  kbl(
    caption = "Summary Statistics for AGE Measures by Type and Normalization",
    col.names = c("Normalization", "Measure", "Canned Median (IQR)", "Kibble Median (IQR)", "Min-Max Canned", "Min-Max Kibble")
  ) %>%
  add_header_above(c(
    " " = 2,  # Empty columns for Normalization & Measure
    "Food Type" = 4  # Four columns for Canned & Kibble statistics
  )) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE, font_size = 12) %>%
  pack_rows("Dry Weight (ug/g)", min(which(df_wide$Normalization == "Dry Weight (ug/g)")),
            max(which(df_wide$Normalization == "Dry Weight (ug/g)"))) %>%
  pack_rows("As Fed (ug/g)", min(which(df_wide$Normalization == "As Fed (ug/g)")),
            max(which(df_wide$Normalization == "As Fed (ug/g)"))) %>%
  pack_rows("As Fed kcal", min(which(df_wide$Normalization == "As Fed kcal")),
            max(which(df_wide$Normalization == "As Fed kcal"))) %>%
  pack_rows("AGEs consumed by 20 kg dog per day", min(which(df_wide$Normalization == "AGEs consumed by 20 kg dog per day")),
            max(which(df_wide$Normalization == "AGEs consumed by 20 kg dog per day")))

# Step 7: Display Table
ft
# here you will need to copy paste and somehow save this table.
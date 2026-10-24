# SF versus LF Supplemental Figure Vick's question on 1-1-25

# Continue after running the "PRE PROCESSING CODE"
#the original plots were made in prism for some reason, need to redo that.

data <- data_filtered_moist

# Define SF and LF columns for each normalization
sf_lf_columns <- list(
  "Dry Weight (ug/g)" = c("SF_ug_per_g_food", "LF_AGE_ug_per_g_food"),
  "As Fed (ug/g)" = c("SF_ug_per_g_food_moist", "LF_AGE_ug_per_g_food_moist"),
  "As Fed (ug/kcal)" = c("SF_ug_per_kcal_food_moist", "LF_ug_per_kcal_food_moist")
)

# Prepare to store p-values
p_values <- data.frame(
  Normalization = character(),
  Type = character(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

# Calculate Mann-Whitney U test p-values for each type and normalization
for (normalization in names(sf_lf_columns)) {
  measures <- sf_lf_columns[[normalization]]
  
  for (food_type in c("Canned", "Kibble")) {
    # Subset the data for the current food type and normalization
    filtered_data <- data %>% filter(Type == food_type)
    
    # Extract SF and LF values
    sf_values <- filtered_data[[measures[1]]]
    lf_values <- filtered_data[[measures[2]]]
    
    # Perform the Mann-Whitney U test without continuity correction
    test_result <- wilcox.test(sf_values, lf_values, exact = FALSE, correct = FALSE)
    
    # Store the p-value
    p_values <- rbind(p_values, data.frame(
      Normalization = normalization,
      Type = food_type,
      P_Value = test_result$p.value
    ))
  }
}

# Print the p-values
print(p_values)
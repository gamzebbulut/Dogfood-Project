## Figure 2 March 27 2025

library(readxl)
library(dplyr)

# Load Tab 2 from Excel
data <- read_excel("Final excel sheet for manuscript_02-20-25.xlsx", sheet = 2)
View(data)

# Check column names and structure
glimpse(data)

# Coerce data types as needed
data_Attempt14GB <- data %>%
  mutate(
    label = as.character(ID),
    Type.f = as.factor(Type),
    CML_ug_per_g_food = as.numeric(CML_ug_per_g_food),
    Science.f = as.factor(Science)
  )

# Create new combined AGE metrics (dry weight only)
data_Attempt14GB <- data_Attempt14GB %>%
  mutate(
    CML_plus_MG_g = CML_ug_per_g_food + MG_ug_per_g_food,
    Fluo_total_g = SF_ug_per_g_food + LF_AGE_ug_per_g_food
  )

# Define AGE columns to correlate
age_columns <- c(
  "CML_ug_per_g_food", 
  "MG_ug_per_g_food", 
  "CML_plus_MG_g", 
  "SF_ug_per_g_food", 
  "LF_AGE_ug_per_g_food", 
  "Fluo_total_g"
)

# Define moisture-normalized macronutrient columns
ingredient_columns <- c("Dry_weight_protein", "Dry_weight_fat", "Dry_weight_fiber", "Dry_weight_carbohydrate")

# Filter for complete rows
data_Attempt14GB <- data_Attempt14GB %>% drop_na(any_of(c(age_columns, ingredient_columns)))

# Subset by type
data_filtered_13 <- data_Attempt14GB

# Filter for Kibble
data_kibble <- data_filtered_13 %>% filter(Type == "Kibble")

# Compute correlations and FDR correction
kibble_results <- compute_correlations_and_matrix(data_kibble, age_columns, ingredient_columns)
kibble_corr_matrix <- kibble_results$corr_matrix
kibble_fdr_p_matrix <- kibble_results$fdr_p_matrix
kibble_table <- kibble_results$results_table
write.csv(kibble_table, "kibble_ingredient_correlations.csv", row.names = FALSE)
plot_corr(kibble_corr_matrix, kibble_fdr_p_matrix, "Correlation Matrix for Kibble (Dry Weight)")

# Filter for Canned
data_canned <- data_filtered_13 %>% filter(Type == "Canned")
canned_results <- compute_correlations_and_matrix(data_canned, age_columns, ingredient_columns)
canned_corr_matrix <- canned_results$corr_matrix
canned_fdr_p_matrix <- canned_results$fdr_p_matrix
canned_table <- canned_results$results_table
write.csv(canned_table, "canned_ingredient_correlations.csv", row.names = FALSE)
plot_corr(canned_corr_matrix, canned_fdr_p_matrix, "Correlation Matrix for Canned (Dry Weight)")

View(canned_table)
sum(canned_table$FDR_Adjusted_P <= 0.05)
sum(kibble_table$FDR_Adjusted_P <= 0.05)




# WORKING ON INGREDIENT CORRELATIONS figure here
### Feb 21 2025: new goal is to use Dry weight AGE measures and dry weight corrected ingredient numbers

# this piece of code makes the corr matrix for fig 2 and also the table 

# Function to compute pairwise correlations and FDR correction
compute_correlations_and_matrix <- function(data, age_cols, ing_cols) {
  # Initialize matrices for correlation coefficients and p-values
  corr_matrix <- matrix(NA, nrow = length(ing_cols), ncol = length(age_cols),
                        dimnames = list(ing_cols, age_cols))
  p_matrix <- matrix(NA, nrow = length(ing_cols), ncol = length(age_cols),
                     dimnames = list(ing_cols, age_cols))
  
  # Initialize a data frame for the table
  results_table <- data.frame(Variable1 = character(), Variable2 = character(),
                              Correlation = numeric(), P_Value = numeric(),
                              stringsAsFactors = FALSE)
  
  # Compute pairwise correlations
  for (age in age_cols) {
    for (ing in ing_cols) {
      # Filter out NA values for the current pair
      valid_data <- data %>%
        select(all_of(c(age, ing))) %>%
        drop_na()
      
      if (nrow(valid_data) > 2) {
        # Perform Spearman correlation
        cor_test <- cor.test(valid_data[[age]], valid_data[[ing]], method = "spearman")
        correlation <- cor_test$estimate
        p_value <- cor_test$p.value
        
        # Store in the matrix
        corr_matrix[ing, age] <- correlation
        p_matrix[ing, age] <- p_value
        
        # Append to the table
        results_table <- rbind(results_table, data.frame(
          Variable1 = age, Variable2 = ing,
          Correlation = correlation, P_Value = p_value
        ))
      }
    }
  }
  
  # Apply FDR correction to p-values
  results_table$FDR_Adjusted_P <- p.adjust(results_table$P_Value, method = "BH")
  
  # Update the matrix with FDR-adjusted p-values
  fdr_p_matrix <- matrix(p.adjust(p_matrix, method = "BH"), 
                         nrow = nrow(p_matrix), dimnames = dimnames(p_matrix))
  
  list(corr_matrix = corr_matrix, p_matrix = p_matrix,
       fdr_p_matrix = fdr_p_matrix, results_table = results_table)
}

# Function to plot the correlation matrix
# Step 7: Plot Correlation Heatmaps with Spearman and FDR p-values
plot_corr <- function(corr_matrix, fdr_p_matrix, title) {
  # Mask non-significant cells for annotations
  significant <- fdr_p_matrix <= 0.05
  
  corrplot(
    corr_matrix,
    method = "color",
    col = colorRampPalette(c("blue", "white", "red"))(200), # Blue = negative, Red = positive
    p.mat = fdr_p_matrix,
    insig = "blank",       # Hide non-significant correlations
    sig.level = 0.05,      # Significance threshold
    tl.cex = 0.8,          # Text label size
    cl.cex = 0.8,          # Color legend size
    title = title,
    mar = c(0, 0, 2, 0),   # Adjust margins
    addgrid.col = "gray",  # Add grid for structure
    addCoef.col = "black", # Add coefficient text in black
    number.cex = 1.0       # size of labels
  )
}



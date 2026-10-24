## Figure 2 Feb 21 2025

#continue after running the "PRE PROCESSING CODE"

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


# Define dry weight AGE columns only
age_columns <- c("CML_ug_per_g_food", 
                 "MG_ug_per_g_food", 
                 "CML_plus_MG_g", 
                 "SF_ug_per_g_food", 
                 "Combined_g", 
                 "LF_AGE_ug_per_g_food")

ingredient_columns <- c("Dry_weight_protein", "Dry_weight_fat", "Dry_weight_fiber")

# Filter data for Kibble
data_kibble <- data_filtered_13 %>% filter(Type == "Kibble")

# Compute correlations and FDR correction
kibble_results <- compute_correlations_and_matrix(data_kibble, age_columns, ingredient_columns)

# Extract results
kibble_corr_matrix <- kibble_results$corr_matrix
kibble_fdr_p_matrix <- kibble_results$fdr_p_matrix
kibble_table <- kibble_results$results_table

# Save the table to a CSV
write.csv(kibble_table, "kibble_correlationsFeb212025.csv", row.names = FALSE)

# Plot the correlation matrix
plot_corr(kibble_corr_matrix, kibble_fdr_p_matrix, "Correlation Matrix for Kibble Food")

#For canned
# Filter data for CANNED
data_canned <- data_filtered_13 %>% filter(Type == "Canned")

# Compute correlations and FDR correction
canned_results <- compute_correlations_and_matrix(data_canned, age_columns, ingredient_columns)

# Extract results
canned_corr_matrix <- canned_results$corr_matrix
canned_fdr_p_matrix <- canned_results$fdr_p_matrix
canned_table <- canned_results$results_table

# Save the table to a CSV
write.csv(canned_table, "canned_correlationsFeb212025.csv", row.names = FALSE)

# Plot the correlation matrix
plot_corr(canned_corr_matrix, canned_fdr_p_matrix, "Correlation Matrix for Canned Food")

View(canned_table)

sum(canned_table$FDR_Adjusted_P <= 0.05)
sum(kibble_table$FDR_Adjusted_P <= 0.05)

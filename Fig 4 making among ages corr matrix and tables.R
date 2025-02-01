# Figure 4 
# STARTING TO WORK ON among AGE measures correlations.
# here i can compare CML, MG, LF, SF, 

# 4 will be per 1 of the 3 normalization styles (dry weight or as fed or as fed kcal)

plot_corr_remove_diag <- function(corr_matrix, fdr_p_matrix, title) {
  # Mask the diagonal by setting it to NA
  diag(corr_matrix) <- NA
  diag(fdr_p_matrix) <- NA
  
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
    number.cex = 1.5,      # Size of labels
    diag = FALSE           # Remove the diagonal
  )
}


# Function to compute pairwise correlations
compute_correlations_and_matrix_fig3 <- function(data, vars) {
  # Initialize matrices for correlation coefficients and p-values
  corr_matrix <- matrix(NA, nrow = length(vars), ncol = length(vars),
                        dimnames = list(vars, vars))
  p_matrix <- matrix(NA, nrow = length(vars), ncol = length(vars),
                     dimnames = list(vars, vars))
  
  # Initialize a data frame for the table
  results_table <- data.frame(Variable1 = character(), Variable2 = character(),
                              Correlation = numeric(), P_Value = numeric(),
                              stringsAsFactors = FALSE)
  
  # Compute pairwise correlations
  for (i in seq_along(vars)) {
    for (j in seq(i, length(vars))) {  # Only compute for upper triangle (i <= j)
      x <- vars[i]
      y <- vars[j]
      
      # Filter out NA values for the current pair
      valid_data <- data %>%
        select(all_of(c(x, y))) %>%
        drop_na()
      
      if (nrow(valid_data) > 2) {
        # Perform Spearman correlation
        cor_test <- cor.test(valid_data[[x]], valid_data[[y]], method = "spearman")
        correlation <- cor_test$estimate
        p_value <- cor_test$p.value
        
        # Store in the matrices
        corr_matrix[x, y] <- correlation
        corr_matrix[y, x] <- correlation
        p_matrix[x, y] <- p_value
        p_matrix[y, x] <- p_value
        
        # Append to the table
        results_table <- rbind(results_table, data.frame(
          Variable1 = x, Variable2 = y,
          Correlation = correlation, P_Value = p_value
        ))
      }
    }
  }
  
  list(corr_matrix = corr_matrix, p_matrix = p_matrix, results_table = results_table)
}

# Combine all p-values for global FDR correction
combine_and_apply_global_fdr <- function(...) {
  all_p_matrices <- list(...)
  all_p_values <- unlist(lapply(all_p_matrices, as.vector), use.names = FALSE)
  all_p_values <- all_p_values[!is.na(all_p_values)] # Remove NA values
  
  # Apply global FDR correction
  global_fdr_values <- p.adjust(all_p_values, method = "BH")
  
  # Split global FDR-adjusted p-values back into matrices
  adjusted_p_matrices <- list()
  index <- 1
  for (p_matrix in all_p_matrices) {
    adjusted_p_matrix <- p_matrix
    adjusted_p_matrix[!is.na(p_matrix)] <- global_fdr_values[index:(index + sum(!is.na(p_matrix)) - 1)]
    adjusted_p_matrices[[length(adjusted_p_matrices) + 1]] <- adjusted_p_matrix
    index <- index + sum(!is.na(p_matrix))
  }
  
  adjusted_p_matrices
}

# Define AGE measures
age_columns_fig3_1 <- c("CML_ug_per_g_food", "MG_ug_per_g_food", "SF_ug_per_g_food", "LF_AGE_ug_per_g_food")
age_columns_fig3_2 <- c("CML_ug_per_g_food_moist", "MG_ug_per_g_food_moist", "SF_ug_per_g_food_moist", "LF_AGE_ug_per_g_food_moist")
age_columns_fig3_3 <- c("CML_ug_per_kcal_food_moist", "MG_ug_per_kcal_food_moist", "SF_ug_per_kcal_food_moist", "LF_ug_per_kcal_food_moist")

# Compute correlations for Kibble

data_kibble_fig3 <- data_filtered_moist %>% filter(Type == "Kibble")
kibble_results_fig3_1 <- compute_correlations_and_matrix_fig3(data_kibble_fig3, age_columns_fig3_1)
kibble_results_fig3_2 <- compute_correlations_and_matrix_fig3(data_kibble_fig3, age_columns_fig3_2)
kibble_results_fig3_3 <- compute_correlations_and_matrix_fig3(data_kibble_fig3, age_columns_fig3_3)

# Compute correlations for Canned
data_canned_fig3 <- data_filtered_moist %>% filter(Type == "Canned")
canned_results_fig3_1 <- compute_correlations_and_matrix_fig3(data_canned_fig3, age_columns_fig3_1)
canned_results_fig3_2 <- compute_correlations_and_matrix_fig3(data_canned_fig3, age_columns_fig3_2)
canned_results_fig3_3 <- compute_correlations_and_matrix_fig3(data_canned_fig3, age_columns_fig3_3)

# Apply global FDR correction
adjusted_p_matrices <- combine_and_apply_global_fdr(
  kibble_results_fig3_1$p_matrix, kibble_results_fig3_2$p_matrix, kibble_results_fig3_3$p_matrix,
  canned_results_fig3_1$p_matrix, canned_results_fig3_2$p_matrix, canned_results_fig3_3$p_matrix
)

# Update matrices with globally adjusted p-values
kibble_fdr_p_matrix_fig3_1 <- adjusted_p_matrices[[1]]
kibble_fdr_p_matrix_fig3_2 <- adjusted_p_matrices[[2]]
kibble_fdr_p_matrix_fig3_3 <- adjusted_p_matrices[[3]]
canned_fdr_p_matrix_fig3_1 <- adjusted_p_matrices[[4]]
canned_fdr_p_matrix_fig3_2 <- adjusted_p_matrices[[5]]
canned_fdr_p_matrix_fig3_3 <- adjusted_p_matrices[[6]]

# Re-Plot with global FDR-adjusted p-values
plot_corr_remove_diag(kibble_results_fig3_1$corr_matrix, kibble_fdr_p_matrix_fig3_1, "Kibble (Dry Weight)")
plot_corr_remove_diag(kibble_results_fig3_2$corr_matrix, kibble_fdr_p_matrix_fig3_2, "Kibble (As Fed ug)")
plot_corr_remove_diag(kibble_results_fig3_3$corr_matrix, kibble_fdr_p_matrix_fig3_3, "Kibble (As Fed kcal)")

plot_corr_remove_diag(canned_results_fig3_1$corr_matrix, canned_fdr_p_matrix_fig3_1, "Canned (Dry Weight)")
plot_corr_remove_diag(canned_results_fig3_2$corr_matrix, canned_fdr_p_matrix_fig3_2, "Canned (As Fed ug)")
plot_corr_remove_diag(canned_results_fig3_3$corr_matrix, canned_fdr_p_matrix_fig3_3, "Canned (As Fed kcal)")

# need to manually save the plots from the plots pane.

#why does it not make a table?
library(readxl)
library(dplyr)
library(corrplot)
library(tidyr)

# Load data
data <- read_excel("Final excel sheet for manuscript_02-20-25.xlsx", sheet = 2)

# Initial data cleanup
data_Attempt14GB <- data %>%
  mutate(
    label = as.character(ID),
    Type.f = as.factor(Type),
    CML_ug_per_g_food = as.numeric(CML_ug_per_g_food),
    Science.f = as.factor(Science)
  )

# Add new AGE metrics including Total AGEs
data_Attempt14GB <- data_Attempt14GB %>%
  mutate(
    CML_plus_MG_g = CML_ug_per_g_food + MG_ug_per_g_food,
    Fluo_total_g = SF_ug_per_g_food + LF_AGE_ug_per_g_food,
    Total_AGEs_g = CML_ug_per_g_food + MG_ug_per_g_food + SF_ug_per_g_food + LF_AGE_ug_per_g_food
  )

# Updated AGE columns (including Total AGEs)
age_columns <- c(
  "CML_ug_per_g_food",
  "MG_ug_per_g_food",
  "CML_plus_MG_g",
  "SF_ug_per_g_food",
  "LF_AGE_ug_per_g_food",
  "Fluo_total_g",
  "Total_AGEs_g"
)

# Updated nutrient (ingredient) columns
ingredient_columns <- c(
  "Dry_weight_protein",
  "Dry_weight_fat",
  "Dry_weight_fiber",
  "Dry_weight_carbohydrate"
)

# Drop missing values for relevant columns
data_Attempt14GB <- data_Attempt14GB %>%
  drop_na(any_of(c(age_columns, ingredient_columns)))

# Function to compute pairwise Spearman correlations and FDR
compute_correlations_and_matrix <- function(data, age_cols, ing_cols) {
  corr_matrix <- matrix(NA, nrow = length(ing_cols), ncol = length(age_cols),
                        dimnames = list(ing_cols, age_cols))
  p_matrix <- matrix(NA, nrow = length(ing_cols), ncol = length(age_cols),
                     dimnames = list(ing_cols, age_cols))
  results_table <- data.frame(Variable1 = character(), Variable2 = character(),
                              Correlation = numeric(), P_Value = numeric(),
                              stringsAsFactors = FALSE)
  
  for (age in age_cols) {
    for (ing in ing_cols) {
      valid_data <- data %>%
        select(all_of(c(age, ing))) %>%
        drop_na()
      
      if (nrow(valid_data) > 2) {
        cor_test <- cor.test(valid_data[[age]], valid_data[[ing]], method = "spearman")
        correlation <- cor_test$estimate
        p_value <- cor_test$p.value
        
        corr_matrix[ing, age] <- correlation
        p_matrix[ing, age] <- p_value
        
        results_table <- rbind(results_table, data.frame(
          Variable1 = age, Variable2 = ing,
          Correlation = correlation, P_Value = p_value
        ))
      }
    }
  }
  
  results_table$FDR_Adjusted_P <- p.adjust(results_table$P_Value, method = "BH")
  fdr_p_matrix <- matrix(p.adjust(p_matrix, method = "BH"),
                         nrow = nrow(p_matrix), dimnames = dimnames(p_matrix))
  
  list(corr_matrix = corr_matrix, p_matrix = p_matrix,
       fdr_p_matrix = fdr_p_matrix, results_table = results_table)
}

# Rename variables for output table
rename_labels <- function(df) {
  df$Variable1 <- recode(df$Variable1,
                         "SF_ug_per_g_food" = "Hydrophilic AGEs",
                         "LF_AGE_ug_per_g_food" = "Hydrophobic AGEs",
                         "Fluo_total_g" = "Fluorescent AGEs",
                         "Total_AGEs_g" = "Total AGEs"
  )
  df$Variable2 <- recode(df$Variable2,
                         "Dry_weight_protein" = "Crude Protein",
                         "Dry_weight_fat" = "Crude Fat",
                         "Dry_weight_fiber" = "Crude Fiber",
                         "Dry_weight_carbohydrate" = "NFE"
  )
  df
}

# Plot function with pretty labels
plot_corr <- function(corr_matrix, fdr_p_matrix, title) {
  corrplot(
    corr_matrix,
    method = "color",
    col = colorRampPalette(c("blue", "white", "red"))(200),
    p.mat = fdr_p_matrix,
    insig = "blank",
    sig.level = 0.05,
    tl.cex = 1.25,
    tl.col = "black",    # Text color
    cl.cex = 1.2,
    title = title,
    mar = c(0, 0, 2, 0),
    addgrid.col = "gray",
    addCoef.col = "black",
    number.cex = 1.5 
  )
}

# Subset by type and run analysis
data_kibble <- data_Attempt14GB %>% filter(Type == "Kibble")
kibble_results <- compute_correlations_and_matrix(data_kibble, age_columns, ingredient_columns)
kibble_table <- rename_labels(kibble_results$results_table)
write.csv(kibble_table, "kibble_ingredient_correlations.csv", row.names = FALSE)

# Plot

# Define label vectors to use consistently
row_labels <- c("Crude Protein", "Crude Fat", "Crude Fiber", "NFE")
col_labels <- c("CML", "MG", "CML+MG", "Hydrophilic AGEs", "Hydrophobic AGEs", "Hydrophilic + Hydrophobic AGEs", "Total AGEs")

# --- KIBBLE SECTION ---
data_kibble <- data_Attempt14GB %>% filter(Type == "Kibble")
kibble_results <- compute_correlations_and_matrix(data_kibble, age_columns, ingredient_columns)
kibble_table <- rename_labels(kibble_results$results_table)
write.csv(kibble_table, "kibble_ingredient_correlations.csv", row.names = FALSE)

# Apply label fixes for matrix alignment
rownames(kibble_results$corr_matrix) <- row_labels
colnames(kibble_results$corr_matrix) <- col_labels
rownames(kibble_results$fdr_p_matrix) <- row_labels
colnames(kibble_results$fdr_p_matrix) <- col_labels

# Plot
plot_corr(kibble_results$corr_matrix, kibble_results$fdr_p_matrix, "Kibble: Ingredient vs AGE Correlations")

# --- CANNED SECTION ---
data_canned <- data_Attempt14GB %>% filter(Type == "Canned")
canned_results <- compute_correlations_and_matrix(data_canned, age_columns, ingredient_columns)
canned_table <- rename_labels(canned_results$results_table)
write.csv(canned_table, "canned_ingredient_correlations.csv", row.names = FALSE)

# Apply label fixes for matrix alignment
rownames(canned_results$corr_matrix) <- row_labels
colnames(canned_results$corr_matrix) <- col_labels
rownames(canned_results$fdr_p_matrix) <- row_labels
colnames(canned_results$fdr_p_matrix) <- col_labels

# Plot
plot_corr(canned_results$corr_matrix, canned_results$fdr_p_matrix, "Canned: Ingredient vs AGE Correlations")

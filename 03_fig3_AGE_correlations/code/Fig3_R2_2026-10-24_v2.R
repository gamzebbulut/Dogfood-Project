# =============================================================================
# Figure 3 — R2 revision (PLOS ONE PONE-D-26-36808), version 2026-10-24 v2 (v2 = TIFF device fix: no cairo)
# Based on R1 script: "Fig 4 making among ages corr matrix and tables.R"
#   (GitHub 3f2e802; file name says Fig 4 but it makes manuscript Fig 3)
#
# CHANGE LOG (every edit is tagged "# R2 CHANGE n" below; nothing else changed)
#  1. Standalone: loads libraries + data and makes the analysis set itself
#     (R1 needed "pre pocessing Jan 29 25.R" run first). Same 56 wet / 44 dry.
#  2. BH: diagonal (variable vs itself, p = 0) removed from the BH vector.
#     R1 included 12 diagonal zeros, which made BH slightly lenient.
#     Checked: no cell changes significance. Family unchanged (both matrices).
#  3. Every cell now shows r_s, with stars for BH-adjusted p (R1 blanked
#     non-significant cells)                                     (Reviewer 3, Fig 3)
#  4. Labels: MG -> MG-H1; titles "Dry (kibble) — dry matter (n = 44)" and
#     "Wet — dry matter (n = 56)"                                (Reviewer 3, Fig 3)
#  5. Panel order: A = Dry (kibble), B = Wet. Matches Results text
#     (lines 281-283: "kibble (Fig. 3A) ... canned foods (Fig. 3B)") and the
#     revised legend. R1 figure had them the other way round.
#  6. Plots saved to TIFF from code (R1 saved by hand from the Plots pane).
#  7. Correlation tables now include BH-adjusted p and stars.
# =============================================================================

# ---- TUNING BLOCK ----------------------------------------------------- R2 CHANGE 1
data_file <- "/Users/gbbulut/Desktop/making figures attempt 14/dogfood-AGE-figures/data/Final excel sheet for manuscript_02-20-25.xlsx"
out_dir   <- "/Users/gbbulut/Desktop/making figures attempt 14/dogfood-AGE-figures/03_fig3_AGE_correlations/output"
version   <- "R2_2026-10-24"
plot_w_in <- 9; plot_h_in <- 8     # TIFF size per panel (inches)
# ---------------------------------------------------------------------------

# R2 CHANGE 1: libraries + data (R1 relied on the preprocessing script)
library(readxl)
library(dplyr)
library(tidyr)
library(corrplot)
data_filtered_13 <- read_excel(data_file) %>%
  # same type casting as R1 "pre pocessing Jan 29 25.R" (CML column is read as text)
  mutate(label = as.character(ID),
         Type.f = as.factor(Type),
         CML_ug_per_g_food = as.numeric(CML_ug_per_g_food),
         `kcal/kg` = as.numeric(`kcal/kg`)) %>%
  rename(kcal_per_kg = `kcal/kg`) %>%
  mutate(Science.f = as.factor(Science)) %>%
  filter(Type %in% c("Canned", "Kibble")) %>%
  drop_na(CML_ug_per_g_food, MG_ug_per_g_food, SF_ug_per_g_food, LF_AGE_ug_per_g_food,
          CML_ug_per_kcal_food_moist, MG_ug_per_kcal_food_moist,
          SF_ug_per_kcal_food_moist, LF_ug_per_kcal_food_moist)
print(table(data_filtered_13$Type))   # expect Canned 56, Kibble 44

stars <- function(p) ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "")))

# Figure 3 
# STARTING TO WORK ON among AGE measures correlations.
# do pre processing first.
# here i can compare CML, MG, LF, SF, 

# 4 will be per 1 of the 3 normalization styles (dry weight or as fed or as fed kcal)

plot_corr_remove_diag <- function(corr_matrix, fdr_p_matrix, title) {
  # R2 CHANGE 3: draw every cell; write r_s + stars ourselves (no blanking)
  diag(corr_matrix) <- NA
  diag(fdr_p_matrix) <- NA
  cp <- corrplot(
    corr_matrix,
    method = "color",
    col = colorRampPalette(c("blue", "white", "red"))(200), # Blue = negative, Red = positive
    tl.cex = 1.3,          # Text label size
    cl.cex = 0.8,          # Color legend size
    tl.col = "black",    # Text color
    title = title,
    mar = c(0, 0, 2, 0),   # Adjust margins
    addgrid.col = "gray",  # Add grid for structure
    na.label = " ",        # diagonal left empty
    diag = FALSE           # Remove the diagonal
  )
  pos <- cp$corrPos                                  # x, y, xName, yName, corr per cell
  p_lab <- fdr_p_matrix[cbind(pos$yName, pos$xName)]
  keep <- !is.na(pos$corr)
  text(pos$x[keep], pos$y[keep],
       labels = paste0(sprintf("%.2f", pos$corr[keep]), stars(p_lab[keep])),
       cex = 1.2, col = "black")
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
  all_p_matrices <- lapply(list(...), function(m) { diag(m) <- NA; m })   # R2 CHANGE 2
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

library(dplyr)

data_filtered_13 <- data_filtered_13 %>%
  mutate(
    CML_MG_dry = CML_ug_per_g_food + MG_ug_per_g_food,
    Fluo_total_dry = SF_ug_per_g_food + LF_AGE_ug_per_g_food,
    Total_AGE_dry = CML_ug_per_g_food + MG_ug_per_g_food + SF_ug_per_g_food + LF_AGE_ug_per_g_food,
    
    CML_MG_af = CML_ug_per_g_food_moist + MG_ug_per_g_food_moist,
    Fluo_total_af = SF_ug_per_g_food_moist + LF_AGE_ug_per_g_food_moist,
    Total_AGE_af = CML_ug_per_g_food_moist + MG_ug_per_g_food_moist + SF_ug_per_g_food_moist + LF_AGE_ug_per_g_food_moist,
    
    CML_MG_kcal = CML_ug_per_kcal_food_moist + MG_ug_per_kcal_food_moist,
    Fluo_total_kcal = SF_ug_per_kcal_food_moist + LF_ug_per_kcal_food_moist,
    Total_AGE_kcal = CML_ug_per_kcal_food_moist + MG_ug_per_kcal_food_moist + SF_ug_per_kcal_food_moist + LF_ug_per_kcal_food_moist
    
  )

# Define expanded AGE column list for Dry Weight only 3-27-25

age_columns_fig3_1 <- c(
  "CML_ug_per_g_food",
  "MG_ug_per_g_food",
  "CML_MG_dry",                  # Move this here (3rd)
  "SF_ug_per_g_food",           # Hydrophilic AGEs
  "LF_AGE_ug_per_g_food",       # Hydrophobic AGEs
  "Fluo_total_dry"
)


# Compute correlations for Kibble

data_kibble_fig3 <- data_filtered_13 %>% filter(Type == "Kibble")
kibble_results_fig3_1 <- compute_correlations_and_matrix_fig3(data_kibble_fig3, age_columns_fig3_1)
#kibble_results_fig3_2 <- compute_correlations_and_matrix_fig3(data_kibble_fig3, age_columns_fig3_2)
#kibble_results_fig3_3 <- compute_correlations_and_matrix_fig3(data_kibble_fig3, age_columns_fig3_3)

# Compute correlations for Canned
data_canned_fig3 <- data_filtered_13 %>% filter(Type == "Canned")
canned_results_fig3_1 <- compute_correlations_and_matrix_fig3(data_canned_fig3, age_columns_fig3_1)
#canned_results_fig3_2 <- compute_correlations_and_matrix_fig3(data_canned_fig3, age_columns_fig3_2)
#canned_results_fig3_3 <- compute_correlations_and_matrix_fig3(data_canned_fig3, age_columns_fig3_3)

# Apply global FDR correction
adjusted_p_matrices <- combine_and_apply_global_fdr(
  kibble_results_fig3_1$p_matrix, 
  canned_results_fig3_1$p_matrix)

# Update matrices with globally adjusted p-values
kibble_fdr_p_matrix_fig3_1 <- adjusted_p_matrices[[1]]
#kibble_fdr_p_matrix_fig3_2 <- adjusted_p_matrices[[2]]
#kibble_fdr_p_matrix_fig3_3 <- adjusted_p_matrices[[3]]
canned_fdr_p_matrix_fig3_1 <- adjusted_p_matrices[[2]]
#canned_fdr_p_matrix_fig3_2 <- adjusted_p_matrices[[5]]
#canned_fdr_p_matrix_fig3_3 <- adjusted_p_matrices[[6]]

row_labels <- c(
  "CML",
  "MG-H1",            # R2 CHANGE 4
  "CML+MG-H1",
  "Hydrophilic\nAGEs",
  "Hydrophobic\nAGEs",
  "Hydrophilic +\nHydrophobic AGEs"  # Manual wrap
)
col_labels <- row_labels


rownames(kibble_results_fig3_1$corr_matrix) <- row_labels
colnames(kibble_results_fig3_1$corr_matrix) <- col_labels
rownames(kibble_fdr_p_matrix_fig3_1) <- row_labels
colnames(kibble_fdr_p_matrix_fig3_1) <- col_labels

# Rename for Canned matrix
rownames(canned_results_fig3_1$corr_matrix) <- row_labels
colnames(canned_results_fig3_1$corr_matrix) <- col_labels
rownames(canned_fdr_p_matrix_fig3_1) <- row_labels
colnames(canned_fdr_p_matrix_fig3_1) <- col_labels


# Re-Plot with global FDR-adjusted p-values
# R2 CHANGE 4, 5, 6: A = Dry (kibble), B = Wet; saved to TIFF
tiff(file.path(out_dir, paste0("Figure3A_Dry_kibble_", version, ".tiff")),
     width = plot_w_in, height = plot_h_in, units = "in", res = 600, compression = "lzw", bg = "white")
plot_corr_remove_diag(kibble_results_fig3_1$corr_matrix, kibble_fdr_p_matrix_fig3_1,
                      "Dry (kibble) \u2014 dry matter (n = 44)")
dev.off()

tiff(file.path(out_dir, paste0("Figure3B_Wet_", version, ".tiff")),
     width = plot_w_in, height = plot_h_in, units = "in", res = 600, compression = "lzw", bg = "white")
plot_corr_remove_diag(canned_results_fig3_1$corr_matrix, canned_fdr_p_matrix_fig3_1,
                      "Wet \u2014 dry matter (n = 56)")
dev.off()

# need to manually save the plots from the plots pane.

#why does it not make a table?

# View correlation table
kibble_results_fig3_1$results_table
canned_results_fig3_1$results_table

# Export to CSV (optional)
# R2 CHANGE 7: tables with BH-adjusted p and stars (diagonal dropped)
add_adj <- function(res, adj_mat) {
  res$results_table %>%
    filter(Variable1 != Variable2) %>%
    mutate(P_adj_BH = adj_mat[cbind(Variable1, Variable2)],
           Stars = stars(P_adj_BH))
}
kib_tab <- add_adj(kibble_results_fig3_1, adjusted_p_matrices[[1]])
can_tab <- add_adj(canned_results_fig3_1, adjusted_p_matrices[[2]])
print(kib_tab); print(can_tab)
write.csv(kib_tab, file.path(out_dir, paste0("Figure3A_Dry_corr_table_", version, ".csv")), row.names = FALSE)
write.csv(can_tab, file.path(out_dir, paste0("Figure3B_Wet_corr_table_", version, ".csv")), row.names = FALSE)

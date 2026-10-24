# =============================================================================
# Figure 4 — R2 revision (PLOS ONE PONE-D-26-36808), version 2026-10-24 v3 (v2 = TIFF device fix: no cairo; v3 = stars on a second line, wrapped column title)
# Based on R1 script: "Fig 4 April 5 2025 ingredient corr.R" (GitHub 3f2e802)
#
# CHANGE LOG (every edit is tagged "# R2 CHANGE n" below; nothing else changed)
#  1. Paths: hardcoded data file + output folder (TUNING BLOCK). Same sheet 2
#     ("macro normalization"); same 56 wet / 44 dry foods as Figs 1-3 (checked).
#  2. Every cell shows r_s, with stars for BH-adjusted p (R1 blanked
#     non-significant cells)                                    (Reviewer 3, Fig 4)
#  3. Labels: MG -> MG-H1, CML+MG -> CML+MG-H1,
#     Total AGEs -> Composite AGE Index (CAI)                    (Reviewer 3 / corresponding author, Fig 4)
#  4. Titles: "Wet (canned) — dry matter (n = 56)"; dry matrix kept as a
#     check plot "Dry (kibble) — dry matter (n = 44)" (not in the paper:
#     no significant correlations)                               (Reviewer 3, Fig 4)
#  5. Plots saved to TIFF from code (R1 saved by hand).
#  6. Tables written to output/ with stars and the new labels.
#  BH family unchanged from R1: 28 tests (7 AGE measures x 4 nutrients) per food type.
# =============================================================================

# ---- TUNING BLOCK ----------------------------------------------------- R2 CHANGE 1
data_file <- "/Users/gbbulut/Desktop/making figures attempt 14/dogfood-AGE-figures/data/Final excel sheet for manuscript_02-20-25.xlsx"
out_dir   <- "/Users/gbbulut/Desktop/making figures attempt 14/dogfood-AGE-figures/04_fig4_nutrient_correlations/output"
version   <- "R2_2026-10-24"
plot_w_in <- 12; plot_h_in <- 7    # TIFF size (inches)
# ---------------------------------------------------------------------------

library(readxl)
library(dplyr)
library(corrplot)
library(tidyr)

# Load data
data <- read_excel(data_file, sheet = 2)   # R2 CHANGE 1

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
                         "Total_AGEs_g" = "Composite AGE Index (CAI)",   # R2 CHANGE 3
                         "MG_ug_per_g_food" = "MG-H1",
                         "CML_plus_MG_g" = "CML+MG-H1"
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
# R2 CHANGE 2: draw every cell; write r_s + stars ourselves (no blanking)
stars <- function(p) ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "")))
plot_corr <- function(corr_matrix, fdr_p_matrix, title) {
  cp <- corrplot(
    corr_matrix,
    method = "color",
    col = colorRampPalette(c("blue", "white", "red"))(200),
    tl.cex = 1.25,
    tl.col = "black",    # Text color
    cl.cex = 1.2,
    title = title,
    mar = c(0, 0, 2, 0),
    addgrid.col = "gray"
  )
  pos <- cp$corrPos
  p_lab <- fdr_p_matrix[cbind(pos$yName, pos$xName)]
  text(pos$x, pos$y, labels = paste0(sprintf("%.2f", pos$corr), "\n", stars(p_lab)),   # v3: stars below r
       cex = 1.2, col = "black")
}

# Subset by type and run analysis
data_kibble <- data_Attempt14GB %>% filter(Type == "Kibble")
kibble_results <- compute_correlations_and_matrix(data_kibble, age_columns, ingredient_columns)
kibble_table <- rename_labels(kibble_results$results_table)
kibble_table$Stars <- stars(kibble_table$FDR_Adjusted_P)   # R2 CHANGE 6
write.csv(kibble_table, file.path(out_dir, paste0("Figure4_Dry_check_correlations_", version, ".csv")), row.names = FALSE)

# Plot

# Define label vectors to use consistently
row_labels <- c("Crude Protein", "Crude Fat", "Crude Fiber", "NFE")
col_labels <- c("CML", "MG-H1", "CML+MG-H1", "Hydrophilic AGEs", "Hydrophobic AGEs", "Hydrophilic +\nHydrophobic AGEs", "Composite AGE\nIndex (CAI)")   # v3: wrapped   # R2 CHANGE 3

# --- KIBBLE SECTION ---
data_kibble <- data_Attempt14GB %>% filter(Type == "Kibble")
kibble_results <- compute_correlations_and_matrix(data_kibble, age_columns, ingredient_columns)
kibble_table <- rename_labels(kibble_results$results_table)
kibble_table$Stars <- stars(kibble_table$FDR_Adjusted_P)   # R2 CHANGE 6
write.csv(kibble_table, file.path(out_dir, paste0("Figure4_Dry_check_correlations_", version, ".csv")), row.names = FALSE)

# Apply label fixes for matrix alignment
rownames(kibble_results$corr_matrix) <- row_labels
colnames(kibble_results$corr_matrix) <- col_labels
rownames(kibble_results$fdr_p_matrix) <- row_labels
colnames(kibble_results$fdr_p_matrix) <- col_labels

# Plot
# R2 CHANGE 4, 5: check plot for dry foods (not in the paper), saved to TIFF
tiff(file.path(out_dir, paste0("Figure4_Dry_check_", version, ".tiff")),
     width = plot_w_in, height = plot_h_in, units = "in", res = 600, compression = "lzw", bg = "white")   # R2: quartz device on Mac (cairo not installed)
plot_corr(kibble_results$corr_matrix, kibble_results$fdr_p_matrix, "Dry (kibble) \u2014 dry matter (n = 44)")
dev.off()

# --- CANNED SECTION ---
data_canned <- data_Attempt14GB %>% filter(Type == "Canned")
canned_results <- compute_correlations_and_matrix(data_canned, age_columns, ingredient_columns)
canned_table <- rename_labels(canned_results$results_table)
canned_table$Stars <- stars(canned_table$FDR_Adjusted_P)   # R2 CHANGE 6
print(canned_table)
write.csv(canned_table, file.path(out_dir, paste0("Figure4_Wet_correlations_", version, ".csv")), row.names = FALSE)

# Apply label fixes for matrix alignment
rownames(canned_results$corr_matrix) <- row_labels
colnames(canned_results$corr_matrix) <- col_labels
rownames(canned_results$fdr_p_matrix) <- row_labels
colnames(canned_results$fdr_p_matrix) <- col_labels

# Plot
# R2 CHANGE 4, 5: Figure 4 = wet foods, saved to TIFF
tiff(file.path(out_dir, paste0("Figure4_Wet_", version, ".tiff")),
     width = plot_w_in, height = plot_h_in, units = "in", res = 600, compression = "lzw", bg = "white")   # R2: quartz device on Mac (cairo not installed)
plot_corr(canned_results$corr_matrix, canned_results$fdr_p_matrix, "Wet (canned) \u2014 dry matter (n = 56)")
dev.off()

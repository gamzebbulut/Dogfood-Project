# =============================================================================
# Table 3 (coefficient of variation) — R2 revision, version 2026-10-24 v2 (v2: CAI row removed; same 5 rows as R1)
# Based on R1: CV block at the end of
#   07_fig7_CAI/code/figure_6_combined_AGE_plots_final_Aug6_2025_local.R
#   (calculate_cv() / calculate_all_cv(), request to split canned and kibble, Aug 2025)
#
# CHANGE LOG
#  1. Standalone: loads data with the same casting/NA filter as R1 preprocessing
#     (n = 56 wet / 44 dry). CV = SD / mean x 100 (sample SD), as in R1.
#  2. Same 5 rows as R1 Table 3.
#  3. Wide output with the 6 correct columns (R1 table image repeated the two
#     ug/kcal columns and had a header typo) and R2 labels:
#     Wet / Dry (kibble); Dry matter (µg/g); As-fed (µg/g); Caloric basis (µg/kcal ME)
#  4. Writes CSVs to output/ (the Word table is built from the wide CSV).
#  R1 values reproduce exactly (checked 2026-10-24).
# =============================================================================

# ---- TUNING BLOCK ----
data_file <- "/Users/gbbulut/Desktop/making figures attempt 14/dogfood-AGE-figures/data/Final excel sheet for manuscript_02-20-25.xlsx"
out_dir   <- "/Users/gbbulut/Desktop/making figures attempt 14/dogfood-AGE-figures/table3_CV/output"
version   <- "R2_2026-10-24"
# ----------------------

library(readxl)
library(dplyr)
library(tidyr)

dir.create(out_dir, showWarnings = FALSE)

# CHANGE 1: data exactly as R1 preprocessing
data_filtered_13 <- read_excel(data_file) %>%
  mutate(CML_ug_per_g_food = as.numeric(CML_ug_per_g_food)) %>%
  mutate(CML_plus_MG_kcal_moist = CML_ug_per_kcal_food_moist + MG_ug_per_kcal_food_moist) %>%
  drop_na(any_of(c("CML_ug_per_kcal_food_moist", "MG_ug_per_kcal_food_moist",
                   "CML_plus_MG_kcal_moist", "SF_ug_per_kcal_food_moist"))) %>%
  filter(Type %in% c("Canned", "Kibble"))
print(table(data_filtered_13$Type))   # expect Canned 56, Kibble 44

# Columns per basis, in the order CML, MG-H1, hydrophilic (SF), hydrophobic (LF)
bases <- list(
  "Dry matter (µg/g)"          = c("CML_ug_per_g_food", "MG_ug_per_g_food", "SF_ug_per_g_food", "LF_AGE_ug_per_g_food"),
  "As-fed (µg/g)"              = c("CML_ug_per_g_food_moist", "MG_ug_per_g_food_moist", "SF_ug_per_g_food_moist", "LF_AGE_ug_per_g_food_moist"),
  "Caloric basis (µg/kcal ME)" = c("CML_ug_per_kcal_food_moist", "MG_ug_per_kcal_food_moist", "SF_ug_per_kcal_food_moist", "LF_ug_per_kcal_food_moist")
)
metric_names <- c("CML", "MG-H1",
                  "Hydrophilic AGE-associated fluorescence",
                  "Hydrophobic AGE-associated fluorescence",
                  "Hydrophilic + hydrophobic AGE-associated fluorescence")

cv <- function(x) sd(x) / mean(x) * 100   # same formula as R1 calculate_cv()

long <- bind_rows(lapply(c("Canned", "Kibble"), function(tp) {
  d <- filter(data_filtered_13, Type == tp)
  bind_rows(lapply(names(bases), function(b) {
    cols <- bases[[b]]
    vals <- list(d[[cols[1]]], d[[cols[2]]], d[[cols[3]]], d[[cols[4]]],
                 d[[cols[3]]] + d[[cols[4]]])
    data.frame(Group = ifelse(tp == "Canned", "Wet", "Dry (kibble)"), n = nrow(d),
               Basis = b, Metric = metric_names,
               Mean = sapply(vals, mean), SD = sapply(vals, sd),
               CV = round(sapply(vals, cv), 1))
  }))
}))
write.csv(long, file.path(out_dir, paste0("Table3_CV_long_", version, ".csv")), row.names = FALSE)

# CHANGE 3: wide table, 6 value columns
wide <- long %>%
  mutate(Column = paste(Group, "-", Basis)) %>%
  select(Metric, Column, CV) %>%
  pivot_wider(names_from = Column, values_from = CV)
col_order <- c("Metric", unlist(lapply(names(bases), function(b) paste(c("Wet", "Dry (kibble)"), "-", b))))
wide <- wide[, col_order]
print(as.data.frame(wide))
write.csv(wide, file.path(out_dir, paste0("Table3_CV_wide_", version, ".csv")), row.names = FALSE,
          fileEncoding = "UTF-8")

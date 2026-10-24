# =============================================================================
# Figure 1 — R2 revision (PLOS ONE PONE-D-26-36808), version 2026-10-24
# Based on R1 script: Figure1_Plots_And_FDR_Final.R (GitHub 3f2e802, Apr 2025)
#
# CHANGE LOG (every edit is tagged "# R2 CHANGE n" below; nothing else changed)
#  1. Paths: hardcoded data file + output folder (TUNING BLOCK).
#  2. Food type labels: Canned -> Wet, Kibble -> Dry            (Reviewer 3, Fig 1 item D)
#  3. Compound label: MG -> MG-H1                                (consistent naming)
#  4. Y-axis titles: Dry matter (µg/g), As-fed (µg/g),
#     Caloric basis (µg/kcal ME)                                 (Reviewer 3, Fig 1 items A-C)
#  5. Y-axis breaks extended to 0.125 and 0.5 (MG-H1 wet values go below 2;
#     the lower tail had no tick labels)
#  6. Jitter seed fixed so points land in the same place each run;
#     boxplot outliers hidden (they were drawn twice, once by boxplot, once by jitter)
#  7. TIFF saved with LZW compression and white background (PLOS requirements)
#  8. Stats: BH correction now over the 9 wet-vs-dry comparisons shown in the
#     figure (R1 corrected over all 45 pairwise tests, including cross-analyte
#     pairs). Stars unchanged; adjusted p-values shift slightly.
#     ("define significance stars ... Wilcoxon with BH", Reviewer 3, Fig 1)
#     The R1 45-test table is still written, for comparison.
#  9. Library calls trimmed to the 5 packages actually used (ggstatsplot needs R >= 4.5)
# =============================================================================

# ---- TUNING BLOCK ----------------------------------------------------- R2 CHANGE 1
data_file <- "/Users/gbbulut/Desktop/making figures attempt 14/dogfood-AGE-figures/data/Final excel sheet for manuscript_02-20-25.xlsx"
out_dir   <- "/Users/gbbulut/Desktop/making figures attempt 14/dogfood-AGE-figures/01_fig1_CML_MGH1/output"
version   <- "R2_2026-10-24"
jitter_seed <- 2024
# ---------------------------------------------------------------------------

# Figure 1 (Final April 7: Clean layout, white background, n annotations, fixed grid lines)

# R2 CHANGE 9: load only the packages this script uses (ggstatsplot needs R >= 4.5)
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)
library(rstatix)   # wilcox_test
# library(tidyverse)   # not needed
# library(ggstatsplot) # not used; fails to install on R 4.4
# library(patchwork)
# library(grid)
# library(writexl)
# library(plotly)
# library(htmlwidgets)
# library(GGally)
# library(corrplot)
# library(reshape2)
# library(ggpubr)

# Load data
data_Attempt13GB <- read_excel(data_file)   # R2 CHANGE 1

# Pre-processing
data_Attempt13GB <- data_Attempt13GB %>%
  mutate(label = as.character(ID),
         Type.f = as.factor(Type),
         CML_ug_per_g_food = as.numeric(CML_ug_per_g_food),
         `kcal/kg` = as.numeric(`kcal/kg`)) %>%
  rename(kcal_per_kg = `kcal/kg`) %>%
  mutate(Science.f = as.factor(Science),
         CML_plus_MG_g = CML_ug_per_g_food + MG_ug_per_g_food,
         CML_plus_MG_g_moist = CML_ug_per_g_food_moist + MG_ug_per_g_food_moist,
         CML_plus_MG_kcal_moist = CML_ug_per_kcal_food_moist + MG_ug_per_kcal_food_moist)

# Define measure columns
ug_per_g_measures_dry_weight_Fig1 <- c("CML_ug_per_g_food", "MG_ug_per_g_food", "CML_plus_MG_g")
ug_per_g_measures_moist_Fig1 <- c("CML_ug_per_g_food_moist", "MG_ug_per_g_food_moist", "CML_plus_MG_g_moist")
ug_per_kcal_measures_moist_Fig1 <- c("CML_ug_per_kcal_food_moist", "MG_ug_per_kcal_food_moist", "CML_plus_MG_kcal_moist")

# NA removal
data_Attempt13GB <- data_Attempt13GB %>% drop_na(any_of(ug_per_g_measures_dry_weight_Fig1))
data_Attempt13GB <- data_Attempt13GB %>% drop_na(any_of(ug_per_g_measures_moist_Fig1))
data_Attempt13GB <- data_Attempt13GB %>% drop_na(any_of(ug_per_kcal_measures_moist_Fig1))

# R2 CHANGE 2: Canned -> Wet, Kibble -> Dry (keeps only these two food types)
data_Attempt13GB <- data_Attempt13GB %>%
  filter(Type %in% c("Canned", "Kibble")) %>%
  mutate(Type = recode(Type, "Canned" = "Wet", "Kibble" = "Dry"))
print(table(data_Attempt13GB$Type))   # expect Dry 44, Wet 56


# Custom theme
custom_theme <- theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray80", linewidth = c(0.4, 0.8, 0.4, 0.8)),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "top"
  )

# Function to create plot
create_age_plot <- function(df, value_label, file_name) {
  group_counts <- df %>%
    group_by(Group) %>%
    summarise(n = n(), .groups = "drop")
  
  df <- df %>% left_join(group_counts, by = "Group")
  
  labels <- group_counts %>%
    mutate(label = paste0(Group, "\n(n = ", n, ")")) %>%
    arrange(match(Group, levels(df$Group))) %>%
    pull(label)
  
  plot <- ggplot(df, aes(x = Group, y = Value, fill = Type)) +
    geom_violin(alpha = 0.6, linewidth = 1.2, trim = FALSE) +
    geom_boxplot(width = 0.2, position = position_dodge(width = 0.75), linewidth = 1.2,
                 outlier.shape = NA) +   # R2 CHANGE 6
    geom_jitter(position = position_jitter(width = 0.1, seed = jitter_seed), alpha = 0.5, size = 2) +   # R2 CHANGE 6
    scale_y_continuous(trans = "log2",
                       breaks = c(0.125, 0.5, 2, 8, 32, 128, 512, 2048, 4096, 8192),
                       labels = function(x) as.character(x)) +   # R2 CHANGE 5 (labels: no trailing .000)
    scale_x_discrete(labels = labels) +
    scale_fill_manual(values = c("Wet" = "#F8766D", "Dry" = "#00BFC4"),   # R2 CHANGE 2
                      name = "Food Type") +
    labs(x = NULL, y = value_label) +
    custom_theme
  
  ggsave(file.path(out_dir, file_name), plot, width = 12, height = 6, dpi = 600,
         compression = "lzw", bg = "white")   # R2 CHANGE 1, 7
}

# DRY MATTER
dry_data <- data_Attempt13GB %>%
  select(Type, CML_ug_per_g_food, MG_ug_per_g_food, CML_plus_MG_g) %>%
  pivot_longer(
    cols = c(CML_ug_per_g_food, MG_ug_per_g_food, CML_plus_MG_g),
    names_to = "Compound",
    values_to = "Value"
  ) %>%
  mutate(
    Compound = recode(Compound,
                      "CML_ug_per_g_food" = "CML",
                      "MG_ug_per_g_food" = "MG-H1",   # R2 CHANGE 3
                      "CML_plus_MG_g" = "CML+MG-H1"),
    Group = factor(paste(Compound, "-", Type),
                   levels = c("CML - Wet", "CML - Dry",             # R2 CHANGE 2, 3
                              "MG-H1 - Wet", "MG-H1 - Dry",
                              "CML+MG-H1 - Wet", "CML+MG-H1 - Dry"))
  )


create_age_plot(dry_data, "Dry matter (µg/g)", paste0("Figure1_RowA_DryMatter_", version, ".tiff"))   # R2 CHANGE 4

# MOIST (ug/g)
moist_data <- data_Attempt13GB %>%
  select(Type, CML_ug_per_g_food_moist, MG_ug_per_g_food_moist, CML_plus_MG_g_moist) %>%
  pivot_longer(
    cols = c(CML_ug_per_g_food_moist, MG_ug_per_g_food_moist, CML_plus_MG_g_moist),
    names_to = "Compound",
    values_to = "Value"
  ) %>%
  mutate(
    Compound = recode(Compound,
                      "CML_ug_per_g_food_moist" = "CML",
                      "MG_ug_per_g_food_moist" = "MG-H1",   # R2 CHANGE 3
                      "CML_plus_MG_g_moist" = "CML+MG-H1"),
    Group = factor(paste(Compound, "-", Type),
                   levels = c("CML - Wet", "CML - Dry",             # R2 CHANGE 2, 3
                              "MG-H1 - Wet", "MG-H1 - Dry",
                              "CML+MG-H1 - Wet", "CML+MG-H1 - Dry"))
  )


create_age_plot(moist_data, "As-fed (µg/g)", paste0("Figure1_RowB_AsFed_", version, ".tiff"))   # R2 CHANGE 4

# MOIST (ug/kcal)
kcal_data <- data_Attempt13GB %>%
  select(Type, CML_ug_per_kcal_food_moist, MG_ug_per_kcal_food_moist, CML_plus_MG_kcal_moist) %>%
  pivot_longer(
    cols = c(CML_ug_per_kcal_food_moist, MG_ug_per_kcal_food_moist, CML_plus_MG_kcal_moist),
    names_to = "Compound",
    values_to = "Value"
  ) %>%
  mutate(
    Compound = recode(Compound,
                      "CML_ug_per_kcal_food_moist" = "CML",
                      "MG_ug_per_kcal_food_moist" = "MG-H1",   # R2 CHANGE 3
                      "CML_plus_MG_kcal_moist" = "CML+MG-H1"),
    Group = factor(paste(Compound, "-", Type),
                   levels = c("CML - Wet", "CML - Dry",             # R2 CHANGE 2, 3
                              "MG-H1 - Wet", "MG-H1 - Dry",
                              "CML+MG-H1 - Wet", "CML+MG-H1 - Dry"))
  )


create_age_plot(kcal_data, "Caloric basis (µg/kcal ME)", paste0("Figure1_RowC_CaloricBasis_", version, ".tiff"))   # R2 CHANGE 4

############## STATS

# Group order
group_levels <- c("CML - Wet", "CML - Dry",                 # R2 CHANGE 2, 3
                  "MG-H1 - Wet", "MG-H1 - Dry",
                  "CML+MG-H1 - Wet", "CML+MG-H1 - Dry")

# Helper function to reshape long for p-value analysis
prep_long <- function(df, cml, mg, cmlmg, label) {
  df %>%
    select(Type, !!sym(cml), !!sym(mg), !!sym(cmlmg)) %>%
    pivot_longer(
      cols = c(!!sym(cml), !!sym(mg), !!sym(cmlmg)),
      names_to = "Compound", values_to = "Value"
    ) %>%
    mutate(
      Compound = recode(Compound,
                        !!cml := "CML",
                        !!mg := "MG-H1",   # R2 CHANGE 3
                        !!cmlmg := "CML+MG-H1"),
      Group = factor(paste(Compound, "-", Type), levels = group_levels),
      Measurement = label
    )
}

# Prepare all data frames
dry_data_long <- prep_long(data_Attempt13GB, "CML_ug_per_g_food", "MG_ug_per_g_food", "CML_plus_MG_g", "Dry matter (µg/g)")
moist_data_long <- prep_long(data_Attempt13GB, "CML_ug_per_g_food_moist", "MG_ug_per_g_food_moist", "CML_plus_MG_g_moist", "As-fed (µg/g)")
kcal_data_long <- prep_long(data_Attempt13GB, "CML_ug_per_kcal_food_moist", "MG_ug_per_kcal_food_moist", "CML_plus_MG_kcal_moist", "Caloric basis (µg/kcal ME)")   # R2 CHANGE 4

# Combine all into one
all_data_long <- bind_rows(dry_data_long, moist_data_long, kcal_data_long)

# Pairwise tests and FDR correction
all_pvals <- all_data_long %>%
  group_by(Measurement) %>%
  wilcox_test(Value ~ Group, p.adjust.method = "none") %>%
  ungroup() %>%
  mutate(
    p.adj = p.adjust(p, method = "fdr"),
    Significant = ifelse(p.adj < 0.05, "Yes", "No")
  )

# Save results
write.csv(all_pvals, file.path(out_dir, paste0("Figure1_R1method_all45tests_", version, ".csv")),
          row.names = FALSE)   # R2 CHANGE 1: R1 table kept for comparison

# ---- R2 CHANGE 8: BH over the 9 wet-vs-dry comparisons shown in the figure ----
wet_dry_pvals <- all_pvals %>%
  filter(sub(" - .*", "", group1) == sub(" - .*", "", group2)) %>%   # same compound, Wet vs Dry
  mutate(
    p.adj = p.adjust(p, method = "BH"),
    p.adj.signif = case_when(p.adj < 0.0001 ~ "****",
                             p.adj < 0.001  ~ "***",
                             p.adj < 0.01   ~ "**",
                             p.adj < 0.05   ~ "*",
                             TRUE           ~ "ns"),
    Significant = ifelse(p.adj < 0.05, "Yes", "No")
  )
stopifnot(nrow(wet_dry_pvals) == 9)
print(wet_dry_pvals %>% select(Measurement, group1, group2, n1, n2, p, p.adj, p.adj.signif))
write.csv(wet_dry_pvals, file.path(out_dir, paste0("Figure1_WetVsDry_BH9_", version, ".csv")),
          row.names = FALSE)

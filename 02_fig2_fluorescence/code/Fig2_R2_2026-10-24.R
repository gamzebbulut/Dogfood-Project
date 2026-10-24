# =============================================================================
# Figure 2 — R2 revision (PLOS ONE PONE-D-26-36808), version 2026-10-24
# Based on R1 script: Figure2_April7_Combined_SF_LF.R (GitHub 3f2e802 =
#   local Figure2_April23_Combined_SF_LF.R)
#
# CHANGE LOG (every edit is tagged "# R2 CHANGE n" below; nothing else changed)
#  1. Paths: hardcoded data file + output folder (TUNING BLOCK).
#  2. BUG FIX: R1 dropped NAs only on the fluorescence columns, so 3 foods with
#     no CML value (IDs 98, 117, 120) stayed in -> n = 58 wet / 45 dry.
#     Now the same 56 wet / 44 dry foods as Fig 1 and Table 1.
#  3. Food type labels: Canned -> Wet, Kibble -> Dry            (Reviewer 3, Figs 1-2)
#  4. Y-axis titles: Dry matter (µg/g), As-fed (µg/g),
#     Caloric basis (µg/kcal ME)                                 (Reviewer 3, Fig 2)
#  5. Jitter seed fixed; boxplot outliers hidden (drawn twice in R1)
#  6. TIFF saved with LZW compression and white background (PLOS requirements)
#  7. Stats: BH over the 6 wet-vs-dry comparisons shown (R1 corrected over 18
#     tests incl. cross pairs such as hydrophilic-wet vs hydrophobic-dry).
#     Matches the revised legend ("Differences between wet and dry foods ...
#     Wilcoxon ... Benjamini-Hochberg"). R1-method table still written.
#  8. Within-dry hydrophilic vs hydrophobic (the long red bar in R1) written
#     separately as a PAIRED Wilcoxon signed-rank test (same 44 foods), for use
#     only if that bar is kept.
#  9. Library calls trimmed to the packages actually used.
# =============================================================================

# ---- TUNING BLOCK ----------------------------------------------------- R2 CHANGE 1
data_file <- "/Users/gbbulut/Desktop/making figures attempt 14/dogfood-AGE-figures/data/Final excel sheet for manuscript_02-20-25.xlsx"
out_dir   <- "/Users/gbbulut/Desktop/making figures attempt 14/dogfood-AGE-figures/02_fig2_fluorescence/output"
version   <- "R2_2026-10-24"
jitter_seed <- 2024
# ---------------------------------------------------------------------------


# Figure 2 (April 7 Final: Combined SF and LF plots with shared x-axis layout like Figure 1)

# R2 CHANGE 9: only packages used
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)
library(rstatix)   # wilcox_test
# library(tidyverse)  # not needed
# library(ggpubr)     # not used

# Load data
data <- read_excel(data_file)   # R2 CHANGE 1

# Drop NAs for relevant columns
sf_measures <- c(
  "SF_ug_per_g_food",
  "SF_ug_per_g_food_moist",
  "SF_ug_per_kcal_food_moist"
)

lf_measures <- c(
  "LF_AGE_ug_per_g_food",
  "LF_AGE_ug_per_g_food_moist",
  "LF_ug_per_kcal_food_moist"
)

data <- data %>% drop_na(any_of(c(sf_measures, lf_measures)))

# R2 CHANGE 2: keep only foods with all four AGE measures (same set as Fig 1 / Table 1)
data <- data %>%
  drop_na(CML_ug_per_g_food, MG_ug_per_g_food,
          CML_ug_per_kcal_food_moist, MG_ug_per_kcal_food_moist) %>%
  filter(Type %in% c("Canned", "Kibble")) %>%
  mutate(Type = recode(Type, "Canned" = "Wet", "Kibble" = "Dry"))   # R2 CHANGE 3
print(table(data$Type))   # expect Dry 44, Wet 56

# Add group totals
custom_theme <- theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray80", linewidth = 0.5),
    panel.grid.minor = element_line(color = "gray90", linewidth = 0.25),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "top"
  )

# Function to build combined plots
create_combined_plot <- function(df, label, filename) {
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
                 outlier.shape = NA) +   # R2 CHANGE 5
    geom_jitter(position = position_jitter(width = 0.1, seed = jitter_seed), alpha = 0.5, size = 2) +   # R2 CHANGE 5
    scale_y_continuous(trans = "log2", breaks = c(2, 8, 32, 128, 512, 1024, 2048 , 4096, 8192, 16384, 32768 )) +
    scale_x_discrete(labels = labels) +
    scale_fill_manual(values = c("Wet" = "#F8766D", "Dry" = "#00BFC4"), name = "Food Type") +   # R2 CHANGE 3
    labs(x = NULL, y = label) +
    custom_theme

  ggsave(file.path(out_dir, filename), plot, width = 8, height = 6, dpi = 600,
         compression = "lzw", bg = "white")   # R2 CHANGE 1, 6
}

# Row A: Dry Matter (ug/g)
rowA <- data %>%
  select(Type, SF_ug_per_g_food, LF_AGE_ug_per_g_food) %>%
  pivot_longer(cols = c(SF_ug_per_g_food, LF_AGE_ug_per_g_food),
               names_to = "Compound", values_to = "Value") %>%
  mutate(
    Compound = recode(Compound,
                      "SF_ug_per_g_food" = "Hydrophilic AGEs",
                      "LF_AGE_ug_per_g_food" = "Hydrophobic AGEs"),
    Group = factor(paste(Compound, "-", Type),
                   levels = c("Hydrophilic AGEs - Wet", "Hydrophilic AGEs - Dry",       # R2 CHANGE 3
                              "Hydrophobic AGEs - Wet", "Hydrophobic AGEs - Dry"))
  )

create_combined_plot(rowA, "Dry matter (µg/g)", paste0("Figure2_RowA_DryMatter_", version, ".tiff"))   # R2 CHANGE 4

# Row B: Moist (ug/g)
rowB <- data %>%
  select(Type, SF_ug_per_g_food_moist, LF_AGE_ug_per_g_food_moist) %>%
  pivot_longer(cols = c(SF_ug_per_g_food_moist, LF_AGE_ug_per_g_food_moist),
               names_to = "Compound", values_to = "Value") %>%
  mutate(
    Compound = recode(Compound,
                      "SF_ug_per_g_food_moist" = "Hydrophilic AGEs",
                      "LF_AGE_ug_per_g_food_moist" = "Hydrophobic AGEs"),
    Group = factor(paste(Compound, "-", Type),
                   levels = c("Hydrophilic AGEs - Wet", "Hydrophilic AGEs - Dry",       # R2 CHANGE 3
                              "Hydrophobic AGEs - Wet", "Hydrophobic AGEs - Dry"))
  )

create_combined_plot(rowB, "As-fed (µg/g)", paste0("Figure2_RowB_AsFed_", version, ".tiff"))   # R2 CHANGE 4

# Row C: Moist (ug/kcal)
rowC <- data %>%
  select(Type, SF_ug_per_kcal_food_moist, LF_ug_per_kcal_food_moist) %>%
  pivot_longer(cols = c(SF_ug_per_kcal_food_moist, LF_ug_per_kcal_food_moist),
               names_to = "Compound", values_to = "Value") %>%
  mutate(
    Compound = recode(Compound,
                      "SF_ug_per_kcal_food_moist" = "Hydrophilic AGEs",
                      "LF_ug_per_kcal_food_moist" = "Hydrophobic AGEs"),
    Group = factor(paste(Compound, "-", Type),
                   levels = c("Hydrophilic AGEs - Wet", "Hydrophilic AGEs - Dry",       # R2 CHANGE 3
                              "Hydrophobic AGEs - Wet", "Hydrophobic AGEs - Dry"))
  )

create_combined_plot(rowC, "Caloric basis (µg/kcal ME)", paste0("Figure2_RowC_CaloricBasis_", version, ".tiff"))   # R2 CHANGE 4

# === Stats ===
rowA$Measurement <- "Dry matter (µg/g)"   # R2 CHANGE 4
rowB$Measurement <- "As-fed (µg/g)"
rowC$Measurement <- "Caloric basis (µg/kcal ME)"
all_combined_data <- bind_rows(rowA, rowB, rowC)

# Perform pairwise Wilcoxon tests and apply FDR
combined_stats <- all_combined_data %>%
  group_by(Measurement) %>%
  wilcox_test(Value ~ Group, p.adjust.method = "none") %>%
  ungroup() %>%
  mutate(
    p.adj = p.adjust(p, method = "fdr"),
    Significant = ifelse(p.adj < 0.05, "Yes", "No")
  )

write.csv(combined_stats, file.path(out_dir, paste0("Figure2_R1method_all18tests_", version, ".csv")),
          row.names = FALSE)   # R2 CHANGE 1: R1 method kept for comparison

stars <- function(p) case_when(p < 0.0001 ~ "****", p < 0.001 ~ "***",
                               p < 0.01 ~ "**", p < 0.05 ~ "*", TRUE ~ "ns")

# ---- R2 CHANGE 7: BH over the 6 wet-vs-dry comparisons ----
wet_dry_stats <- combined_stats %>%
  filter(sub(" - .*", "", group1) == sub(" - .*", "", group2)) %>%   # same fraction, Wet vs Dry
  mutate(p.adj = p.adjust(p, method = "BH"),
         p.adj.signif = stars(p.adj),
         Significant = ifelse(p.adj < 0.05, "Yes", "No"))
stopifnot(nrow(wet_dry_stats) == 6)
print(wet_dry_stats %>% select(Measurement, group1, group2, n1, n2, p, p.adj, p.adj.signif))
write.csv(wet_dry_stats, file.path(out_dir, paste0("Figure2_WetVsDry_BH6_", version, ".csv")),
          row.names = FALSE)

# ---- R2 CHANGE 8: within-dry hydrophilic vs hydrophobic, paired signed-rank ----
dry_only <- data %>% filter(Type == "Dry")
pairs <- list("Dry matter (µg/g)"          = c("SF_ug_per_g_food", "LF_AGE_ug_per_g_food"),
              "As-fed (µg/g)"              = c("SF_ug_per_g_food_moist", "LF_AGE_ug_per_g_food_moist"),
              "Caloric basis (µg/kcal ME)" = c("SF_ug_per_kcal_food_moist", "LF_ug_per_kcal_food_moist"))
within_dry <- bind_rows(lapply(names(pairs), function(m) {
  x <- dry_only[[pairs[[m]][1]]]; y <- dry_only[[pairs[[m]][2]]]
  data.frame(Measurement = m, comparison = "Dry: hydrophilic vs hydrophobic (paired)",
             n = length(x), p = wilcox.test(x, y, paired = TRUE)$p.value)
})) %>%
  mutate(p.adj = p.adjust(p, method = "BH"), p.adj.signif = stars(p.adj))
print(within_dry)
write.csv(within_dry, file.path(out_dir, paste0("Figure2_WithinDry_paired_", version, ".csv")),
          row.names = FALSE)

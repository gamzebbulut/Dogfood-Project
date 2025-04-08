
# Figure 2 (April 7 Final: Combined SF and LF plots with shared x-axis layout like Figure 1)

library(ggplot2)
library(dplyr)
library(readxl)
library(tidyverse)
library(ggpubr)
library(rstatix)

# Load data
data <- read_excel("Final excel sheet for manuscript_02-20-25.xlsx")

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
    geom_boxplot(width = 0.2, position = position_dodge(width = 0.75), linewidth = 1.2) +
    geom_jitter(width = 0.1, alpha = 0.5, size = 2) +
    scale_y_continuous(trans = "log2", breaks = c(2, 8, 32, 128, 512, 2048)) +
    scale_x_discrete(labels = labels) +
    scale_fill_manual(values = c("Canned" = "#F8766D", "Kibble" = "#00BFC4"), name = "Food Type") +
    labs(x = NULL, y = label) +
    custom_theme

  ggsave(filename, plot, width = 8, height = 6, dpi = 600)
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
                   levels = c("Hydrophilic AGEs - Canned", "Hydrophilic AGEs - Kibble",
                              "Hydrophobic AGEs - Canned", "Hydrophobic AGEs - Kibble"))
  )

create_combined_plot(rowA, "Dry Matter (µg/g)", "Figure2_RowA_Combined_Dry.tiff")

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
                   levels = c("Hydrophilic AGEs - Canned", "Hydrophilic AGEs - Kibble",
                              "Hydrophobic AGEs - Canned", "Hydrophobic AGEs - Kibble"))
  )

create_combined_plot(rowB, "As Fed (µg/g)", "Figure2_RowB_Combined_Moist.tiff")

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
                   levels = c("Hydrophilic AGEs - Canned", "Hydrophilic AGEs - Kibble",
                              "Hydrophobic AGEs - Canned", "Hydrophobic AGEs - Kibble"))
  )

create_combined_plot(rowC, "As Fed (µg/kcal)", "Figure2_RowC_Combined_Kcal.tiff")

# === Stats ===
rowA$Measurement <- "Dry Matter (µg/g)"
rowB$Measurement <- "As Fed (µg/g)"
rowC$Measurement <- "As Fed (µg/kcal)"
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

write.csv(combined_stats, "Figure2_Combined_FDR_Pvalues.csv", row.names = FALSE)


# Figure 2 (April 7 Update — Match to New Figure 1 Layout)

library(ggplot2)
library(dplyr)
library(readxl)
library(tidyverse)
library(ggpubr)
library(rstatix)

# Load and preprocess
data <- read_excel("Final excel sheet for manuscript_02-20-25.xlsx")

data <- data %>%
  mutate(
    SF_LF_g = as.numeric(SF_ug_per_g_food),
    SF_LF_g_moist = as.numeric(SF_ug_per_g_food_moist),
    SF_LF_kcal_moist = as.numeric(SF_ug_per_kcal_food_moist)
  ) %>%
  mutate(Type = factor(Type))

# Custom theme
custom_theme <- theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray80", size = 0.5),
    panel.grid.minor = element_line(color = "gray90", size = 0.25),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "top"
  )

# Function to plot LF vs SF
create_sf_plot <- function(df, y_label, file_name) {
  df <- df %>%
    mutate(Group = paste("SF", "-", Type)) %>%
    rename(Value = SF_LF)

  group_counts <- df %>%
    group_by(Group) %>%
    summarise(n = n(), .groups = "drop")

  df <- df %>% left_join(group_counts, by = "Group")

  labels <- group_counts %>%
    mutate(label = paste0(Group, "
(n = ", n, ")")) %>%
    arrange(match(Group, unique(df$Group))) %>%
    pull(label)

  plot <- ggplot(df, aes(x = Group, y = Value, fill = Type)) +
    geom_violin(alpha = 0.6, linewidth = 1.2, trim = FALSE) +
    geom_boxplot(width = 0.2, position = position_dodge(width = 0.75), linewidth = 1.2) +
    geom_jitter(width = 0.1, alpha = 0.5, size = 2) +
    scale_y_continuous(trans = "log2",
                       breaks = c(2, 8, 32, 128, 512, 2048)) +
    scale_x_discrete(labels = labels) +
    scale_fill_manual(values = c("Canned" = "#F8766D", "Kibble" = "#00BFC4"),
                      name = "Food Type") +
    labs(x = NULL, y = y_label) +
    custom_theme

  ggsave(file_name, plot, width = 12, height = 6, dpi = 600)
}

# Prepare and plot Row A: Dry Matter
sf_rowA <- data %>%
  select(Type, SF_ug_per_g_food) %>%
  rename(SF_LF = SF_ug_per_g_food)

create_sf_plot(sf_rowA, "Dry Matter (µg/g)", "Figure2_RowA_SF_vs_LF_Dry.tiff")

# Prepare and plot Row B: As Fed (ug/g)
sf_rowB <- data %>%
  select(Type, SF_ug_per_g_food_moist) %>%
  rename(SF_LF = SF_ug_per_g_food_moist)

create_sf_plot(sf_rowB, "As Fed (µg/g)", "Figure2_RowB_SF_vs_LF_Moist.tiff")

# Prepare and plot Row C: As Fed (µg/kcal)
sf_rowC <- data %>%
  select(Type, SF_ug_per_kcal_food_moist) %>%
  rename(SF_LF = SF_ug_per_kcal_food_moist)

create_sf_plot(sf_rowC, "As Fed (µg/kcal)", "Figure2_RowC_SF_vs_LF_Kcal.tiff")

# Combine all for stats
sf_rowA$Measurement <- "Dry Matter (µg/g)"
sf_rowB$Measurement <- "As Fed (µg/g)"
sf_rowC$Measurement <- "As Fed (µg/kcal)"
all_sf_data <- bind_rows(sf_rowA, sf_rowB, sf_rowC)

# FDR-corrected Wilcoxon tests
sf_stats <- all_sf_data %>%
  group_by(Measurement) %>%
  wilcox_test(SF_LF ~ Type, p.adjust.method = "none") %>%
  ungroup() %>%
  mutate(
    p.adj = p.adjust(p, method = "fdr"),
    Significant = ifelse(p.adj < 0.05, "Yes", "No")
  )

write.csv(sf_stats, "Figure2_SFvsLF_FDR_Pvalues.csv", row.names = FALSE)

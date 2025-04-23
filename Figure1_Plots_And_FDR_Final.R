
# Figure 1 (Final April 7: Clean layout, white background, n annotations, fixed grid lines)

library(ggplot2)
library(dplyr)
library(readxl)
library(tidyverse)
library(ggstatsplot)
library(patchwork)
library(grid)
library(writexl)
library(tidyr)
library(plotly)
library(htmlwidgets)
library(GGally)
library(corrplot)
library(reshape2)
library(ggpubr)
library(rstatix)

# Load data
data_Attempt13GB <- read_excel("Final excel sheet for manuscript_02-20-25.xlsx")

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
    geom_boxplot(width = 0.2, position = position_dodge(width = 0.75), linewidth = 1.2) +
    geom_jitter(width = 0.1, alpha = 0.5, size = 2) +
    scale_y_continuous(trans = "log2",
                       breaks = c(2, 8, 32, 128, 512, 2048, 4096, 8192)) +
    scale_x_discrete(labels = labels) +
    scale_fill_manual(values = c("Canned" = "#F8766D", "Kibble" = "#00BFC4"),
                      name = "Food Type") +
    labs(x = NULL, y = value_label) +
    custom_theme
  
  ggsave(file_name, plot, width = 12, height = 6, dpi = 600)
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
                      "MG_ug_per_g_food" = "MG",
                      "CML_plus_MG_g" = "CML+MG"),
    Group = factor(paste(Compound, "-", Type),
                   levels = c("CML - Canned", "CML - Kibble",
                              "MG - Canned", "MG - Kibble",
                              "CML+MG - Canned", "CML+MG - Kibble"))
  )


create_age_plot(dry_data, "Dry Matter (µg/g)", "Figure1_RowA_DryMatter.tiff")

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
                      "MG_ug_per_g_food_moist" = "MG",
                      "CML_plus_MG_g_moist" = "CML+MG"),
    Group = factor(paste(Compound, "-", Type),
                   levels = c("CML - Canned", "CML - Kibble",
                              "MG - Canned", "MG - Kibble",
                              "CML+MG - Canned", "CML+MG - Kibble"))
  )


create_age_plot(moist_data, "As Fed (µg/g)", "Figure1_RowB_AsFed_ug_per_g.tiff")

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
                      "MG_ug_per_kcal_food_moist" = "MG",
                      "CML_plus_MG_kcal_moist" = "CML+MG"),
    Group = factor(paste(Compound, "-", Type),
                   levels = c("CML - Canned", "CML - Kibble",
                              "MG - Canned", "MG - Kibble",
                              "CML+MG - Canned", "CML+MG - Kibble"))
  )


create_age_plot(kcal_data, "As Fed (µg/kcal)", "Figure1_RowC_AsFed_ug_per_kcal.tiff")

############## STATS

# Group order
group_levels <- c("CML - Canned", "CML - Kibble",
                  "MG - Canned", "MG - Kibble",
                  "CML+MG - Canned", "CML+MG - Kibble")

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
                        !!mg := "MG",
                        !!cmlmg := "CML+MG"),
      Group = factor(paste(Compound, "-", Type), levels = group_levels),
      Measurement = label
    )
}

# Prepare all data frames
dry_data_long <- prep_long(data_Attempt13GB, "CML_ug_per_g_food", "MG_ug_per_g_food", "CML_plus_MG_g", "Dry Matter (µg/g)")
moist_data_long <- prep_long(data_Attempt13GB, "CML_ug_per_g_food_moist", "MG_ug_per_g_food_moist", "CML_plus_MG_g_moist", "As Fed (µg/g)")
kcal_data_long <- prep_long(data_Attempt13GB, "CML_ug_per_kcal_food_moist", "MG_ug_per_kcal_food_moist", "CML_plus_MG_kcal_moist", "As Fed (µg/kcal)")

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
write.csv(all_pvals, "Figure1_AllRows_FDR_Pvalues.csv", row.names = FALSE)

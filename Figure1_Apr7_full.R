
# Figure 1 (Updated Flat Layout: CML, MG, CML+MG with Canned/Kibble side-by-side)

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

# Load data
data_Attempt13GB <- read_excel("Final excel sheet for manuscript_02-20-25.xlsx")

# Pre-processing
data_Attempt13GB <- data_Attempt13GB %>%
  mutate(label = as.character(ID)) %>%
  mutate(Type.f = as.factor(Type)) %>%
  mutate(CML_ug_per_g_food = as.numeric(CML_ug_per_g_food),
         `kcal/kg` = as.numeric(`kcal/kg`)) %>%
  rename(kcal_per_kg = `kcal/kg`) %>%
  mutate(Science.f = as.factor(Science)) %>%
  mutate(CML_plus_MG_g = CML_ug_per_g_food + MG_ug_per_g_food,
         CML_plus_MG_g_moist = CML_ug_per_g_food_moist + MG_ug_per_g_food_moist,
         CML_plus_MG_kcal_moist = CML_ug_per_kcal_food_moist + MG_ug_per_kcal_food_moist)

# Na removal?
# Define measure columns April 7th

ug_per_g_measures_dry_weight_Fig1 <- c("CML_ug_per_g_food", "MG_ug_per_g_food", 
                                       "CML_plus_MG_g")

ug_per_g_measures_moist_Fig1 <- c("CML_ug_per_g_food_moist", "MG_ug_per_g_food_moist", 
                                  "CML_plus_MG_g_moist")

ug_per_kcal_measures_moist_Fig1 <- c("CML_ug_per_kcal_food_moist", "MG_ug_per_kcal_food_moist", 
                                     "CML_plus_MG_kcal_moist")

# Filter data to remove missing values
data_Attempt13GB <- data_Attempt13GB %>% drop_na(any_of(ug_per_g_measures_dry_weight_Fig1))
data_Attempt13GB <- data_Attempt13GB %>% drop_na(any_of(ug_per_g_measures_moist_Fig1))
data_Attempt13GB <- data_Attempt13GB %>% drop_na(any_of(ug_per_kcal_measures_moist_Fig1))

# === DRY MATTER PLOT ===
dry_data <- data_Attempt13GB %>%
  select(Type, CML_ug_per_g_food, MG_ug_per_g_food, CML_plus_MG_g) %>%
  pivot_longer(cols = c(CML_ug_per_g_food, MG_ug_per_g_food, CML_plus_MG_g),
               names_to = "Compound", values_to = "Value") %>%
  mutate(Compound = recode(Compound,
                           "CML_ug_per_g_food" = "CML",
                           "MG_ug_per_g_food" = "MG",
                           "CML_plus_MG_g" = "CML+MG"),
         Group = factor(paste(Compound, "-", Type),
                        levels = c("CML - Canned", "CML - Kibble",
                                   "MG - Canned", "MG - Kibble",
                                   "CML+MG - Canned", "CML+MG - Kibble")))

plot_dry <- ggplot(dry_data, aes(x = Group, y = Value, fill = Type)) +
  geom_violin(alpha = 0.6, linewidth = 1.2, trim = FALSE) +
  geom_boxplot(width = 0.2, position = position_dodge(width = 0.75), linewidth = 1.2) +
  geom_jitter(width = 0.1, alpha = 0.5, size = 2) +
  scale_y_continuous(trans = "log2") +
  labs(x = NULL, y = "Dry Matter (µg/g)") +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "none"
  )


ggsave("Figure1_RowA_DryMatter.tiff", plot_dry, width = 12, height = 6, dpi = 600)

# === AS FED (µg/g) PLOT ===
moist_data <- data_Attempt13GB %>%
  select(Type, CML_ug_per_g_food_moist, MG_ug_per_g_food_moist, CML_plus_MG_g_moist) %>%
  pivot_longer(cols = c(CML_ug_per_g_food_moist, MG_ug_per_g_food_moist, CML_plus_MG_g_moist),
               names_to = "Compound", values_to = "Value") %>%
  mutate(Compound = recode(Compound,
                           "CML_ug_per_g_food_moist" = "CML",
                           "MG_ug_per_g_food_moist" = "MG",
                           "CML_plus_MG_g_moist" = "CML+MG"),
         Group = factor(paste(Compound, "-", Type),
                        levels = c("CML - Canned", "CML - Kibble",
                                   "MG - Canned", "MG - Kibble",
                                   "CML+MG - Canned", "CML+MG - Kibble")))

plot_moist <- ggplot(moist_data, aes(x = Group, y = Value, fill = Type)) +
  geom_violin(alpha = 0.6, linewidth = 1.2, trim = FALSE) +
  geom_boxplot(width = 0.2, position = position_dodge(width = 0.75), linewidth = 1.2) +
  geom_jitter(width = 0.1, alpha = 0.5, size = 2) +
  scale_y_continuous(trans = "log2") +
  labs(x = NULL, y = "As Fed (µg/g)") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position = "none")

ggsave("Figure1_RowB_AsFed_ug_per_g.png", plot_moist, width = 12, height = 4, dpi = 600)


# === AS FED (µg/kcal) PLOT ===
kcal_data <- data_Attempt13GB %>%
  select(Type, CML_ug_per_kcal_food_moist, MG_ug_per_kcal_food_moist, CML_plus_MG_kcal_moist) %>%
  pivot_longer(cols = c(CML_ug_per_kcal_food_moist, MG_ug_per_kcal_food_moist, CML_plus_MG_kcal_moist),
               names_to = "Compound", values_to = "Value") %>%
  mutate(Compound = recode(Compound,
                           "CML_ug_per_kcal_food_moist" = "CML",
                           "MG_ug_per_kcal_food_moist" = "MG",
                           "CML_plus_MG_kcal_moist" = "CML+MG"),
         Group = factor(paste(Compound, "-", Type),
                        levels = c("CML - Canned", "CML - Kibble",
                                   "MG - Canned", "MG - Kibble",
                                   "CML+MG - Canned", "CML+MG - Kibble")))

plot_kcal <- ggplot(kcal_data, aes(x = Group, y = Value, fill = Type)) +
  geom_violin(alpha = 0.6, linewidth = 1.2, trim = FALSE) +
  geom_boxplot(width = 0.2, position = position_dodge(width = 0.75), linewidth = 1.2) +
  geom_jitter(width = 0.1, alpha = 0.5, size = 2) +
  scale_y_continuous(trans = "log2") +
  labs(x = NULL, y = "As Fed (µg/kcal)") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position = "none")

ggsave("Figure1_RowC_AsFed_ug_per_kcal.png", plot_kcal, width = 12, height = 4, dpi = 600)

# (version on Jan 29 2025)

# This part only has the "PRE-PROCESSING STEPS"

# we normalized all AGE measures to per kcal of food === only for box plots in fig1.
# 5 kcal values that were missing were added by hand, Dr Turner found them.
# 3 super high CML value containing rows were removed by hand , 
# in addition to these now we also have normalization to moisture/dryness 10-31-24

library(ggplot2)
library(dplyr)
library(readxl)
library(tidyverse)
library(ggstatsplot)
library(patchwork)
library(grid)
library(readxl)
library(writexl)
library(tidyr)
library(plotly)
library(htmlwidgets) 
library(GGally)
library(corrplot)
library(reshape2)
library(ggpubr)
library(corrplot)


# import data

Final_excel_sheet_for_manuscript_01_22_25 <- read_excel("Final excel sheet for manuscript_01-22-25.xlsx")
View(Final_excel_sheet_for_manuscript_01_22_25)

data_moisture <- Final_excel_sheet_for_manuscript_01_22_25

str(data_moisture)

# need some modification here: coerce the data type to be either numeric or factor or as character as needed.
data_moisture<- data_moisture %>%
  mutate(label = as.character(ID)) %>%
  mutate(Type.f = as.factor(Type)) %>%
  mutate(CML_ug_per_g_food = as.numeric(CML_ug_per_g_food)) %>%
  mutate(CML_ug_per_g_food_moist = as.numeric(CML_ug_per_g_food_moist)) %>%
  mutate(CML_ug_per_g_food = as.numeric(CML_ug_per_g_food)) %>%
  mutate(`kcal/kg` = as.numeric(`kcal/kg`)) %>%
  rename(kcal_per_kg = `kcal/kg`) %>%
  mutate(Science.f = as.factor(Science)) 

# create new metrics
data_moisture <- data_moisture %>%
  mutate(CML_plus_MG_g = CML_ug_per_g_food + MG_ug_per_g_food) %>%
  mutate(CML_plus_MG_g_moist = CML_ug_per_g_food_moist + MG_ug_per_g_food_moist) %>%
  mutate(CML_plus_MG_kcal_moist = CML_ug_per_kcal_food_moist + MG_ug_per_kcal_food_moist) %>%
  mutate(Combined_g= CML_ug_per_g_food + MG_ug_per_g_food + SF_ug_per_g_food) %>%
  mutate(Combined_g_moist= CML_ug_per_g_food_moist + MG_ug_per_g_food_moist + SF_ug_per_g_food_moist) %>%
  mutate(Combined_kcal_moist= CML_ug_per_kcal_food_moist + MG_ug_per_kcal_food_moist + SF_ug_per_kcal_food_moist)

View(data_moisture)
str(data_moisture)

#======================remove na

# Define measure columns

ug_per_g_measures_dry_weight <- c("CML_ug_per_g_food", "MG_ug_per_g_food", 
                                  "CML_plus_MG_g", "SF_ug_per_g_food")

ug_per_g_measures_moist <- c("CML_ug_per_g_food_moist", "MG_ug_per_g_food_moist", 
                             "CML_plus_MG_g_moist", "SF_ug_per_g_food_moist")

ug_per_kcal_measures_moist <- c("CML_ug_per_kcal_food_moist", "MG_ug_per_kcal_food_moist", 
                                "CML_plus_MG_kcal_moist", "SF_ug_per_kcal_food_moist")

# Filter data to remove missing values
data_filtered_moist <- data_moisture %>% drop_na(any_of(ug_per_g_measures_dry_weight))
data_filtered_moist <- data_moisture %>% drop_na(any_of(ug_per_g_measures_moist))
data_filtered_moist <- data_moisture %>% drop_na(any_of(ug_per_kcal_measures_moist))

View(data_filtered_moist)

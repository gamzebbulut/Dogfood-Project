# (version on Feb 21 2025)

# This part only has the "PRE-PROCESSING STEPS"

# we normalized all AGE measures to per kcal of food === only for box plots in fig1.
# 5 kcal values that were missing were added by hand, Dr Turner found them.
# 3 super high CML value containing rows were removed by hand , 
# in addition to these now we also have normalization to moisture/dryness 10-31-24
# in addition now we have dryness corrected numbers for ingredients in guaranteed analysis. 2-21-25

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

# Set working directory to Dog food data Attempt 13

# import data
data_Attempt13GB <- read_excel("Final excel sheet for manuscript_02-20-25.xlsx")
View(data_Attempt13GB)

str(data_Attempt13GB)

# need some modification here: coerce the data type to be either numeric or factor or as character as needed.
data_Attempt13GB<- data_Attempt13GB %>%
  mutate(label = as.character(ID)) %>%
  mutate(Type.f = as.factor(Type)) %>%
  mutate(CML_ug_per_g_food = as.numeric(CML_ug_per_g_food)) %>%
  mutate(`kcal/kg` = as.numeric(`kcal/kg`)) %>%
  rename(kcal_per_kg = `kcal/kg`) %>%
  mutate(Science.f = as.factor(Science)) 

# create new metrics
data_Attempt13GB <- data_Attempt13GB %>%
  mutate(CML_plus_MG_g = CML_ug_per_g_food + MG_ug_per_g_food) %>%
  mutate(CML_plus_MG_g_moist = CML_ug_per_g_food_moist + MG_ug_per_g_food_moist) %>%
  mutate(CML_plus_MG_kcal_moist = CML_ug_per_kcal_food_moist + MG_ug_per_kcal_food_moist) %>%
  mutate(Combined_g= CML_ug_per_g_food + MG_ug_per_g_food + SF_ug_per_g_food) %>%
  mutate(Combined_g_moist= CML_ug_per_g_food_moist + MG_ug_per_g_food_moist + SF_ug_per_g_food_moist) %>%
  mutate(Combined_kcal_moist= CML_ug_per_kcal_food_moist + MG_ug_per_kcal_food_moist + SF_ug_per_kcal_food_moist)

View(data_Attempt13GB)
str(data_Attempt13GB)

#======================remove na

# Define measure columns

ug_per_g_measures_dry_weight <- c("CML_ug_per_g_food", "MG_ug_per_g_food", 
                                  "CML_plus_MG_g", "SF_ug_per_g_food")

ug_per_g_measures_moist <- c("CML_ug_per_g_food_moist", "MG_ug_per_g_food_moist", 
                             "CML_plus_MG_g_moist", "SF_ug_per_g_food_moist")

ug_per_kcal_measures_moist <- c("CML_ug_per_kcal_food_moist", "MG_ug_per_kcal_food_moist", 
                                "CML_plus_MG_kcal_moist", "SF_ug_per_kcal_food_moist")

# Filter data to remove missing values
data_filtered_13 <- data_Attempt13GB %>% drop_na(any_of(ug_per_g_measures_dry_weight))
data_filtered_13 <- data_Attempt13GB %>% drop_na(any_of(ug_per_g_measures_moist))
data_filtered_13 <- data_Attempt13GB %>% drop_na(any_of(ug_per_kcal_measures_moist))

View(data_filtered_13)

# (version on October 31 2024)
# we normalized all AGE measures to per kcal of food === only for box plots in fig1.
# 5 kcal values that were missing were added by hand, Dr Turner found them.
# 3 super high CML value containing rows were removed by hand , 
#we will exclude 3 rows for figure 1 boxplots and pairwise.
# we will only NA the 3 high CML guys and keep the rest for the other figures.
# in addition to these now we also have normalization to moisture/dryness 10-31-24

library(ggplot2)
library(dplyr)
library(readxl)

# or click on the file to import

ELISA_and_fluorescence_restruc_GB1_norm_kcal_3CML_NAed_VJF_edits_moisture_normalization <- read_excel("attempt 9/ELISA and fluorescence_restruc_GB1_norm_kcal_3CML_NAed_VJF edits_moisture normalization.xlsx")
View(ELISA_and_fluorescence_restruc_GB1_norm_kcal_3CML_NAed_VJF_edits_moisture_normalization)


data_moisture <- ELISA_and_fluorescence_restruc_GB1_norm_kcal_3CML_NAed_VJF_edits_moisture_normalization

str(data_moisture)

data_moisture<- data_moisture %>%
  mutate(label = as.character(ID))

# need some modification here: coerce the data type to be numeric.
data_moisture <- data_moisture %>%
  mutate(CML_ug_per_g_food_moist = as.numeric(CML_ug_per_g_food_moist))

# need some modification here: coerce the data type to be numeric.
data_moisture <- data_moisture %>%
  mutate(`kcal/kg` = as.numeric(`kcal/kg`)) %>%
  rename(kcal_per_kg = `kcal/kg`)

data_moisture <- data_moisture %>%
  mutate(CML_plus_MG_g_moist = CML_ug_per_g_food_moist + MG_ug_per_g_food_moist) %>%
  mutate(CML_plus_MG_kcal_moist = CML_ug_per_kcal_food_moist + MG_ug_per_kcal_food_moist) %>%
  mutate(Combined_g_moist= CML_ug_per_g_food_moist + MG_ug_per_g_food_moist + SF_ug_per_g_food_moist) 

# Remove rows with any NA values in the relevant columns
data_moisture <- data_moisture %>% drop_na(all_of(columns_to_convert))


View(data_moisture)

# 1a extract data for prism CML canned
canned <- data_moisture %>%
  filter(Type == "Canned") %>%
  dplyr::select(CML_ug_per_kcal_food_moist)
View (canned)

# 2a extract data for prism CML kibble
kibble <- data_moisture %>%
  filter(Type== "Kibble") %>%
  dplyr::select(CML_ug_per_kcal_food_moist)
View (kibble)

# 3a extract data for prism MG canned
canned_mg <- data_moisture %>%
  filter(Type== "Canned") %>%
  dplyr::select(MG_ug_per_kcal_food_moist)
View (canned_mg)

# 4a extract data for prism MG kibble
kibble_mg <- data_moisture %>%
  filter(Type== "Kibble") %>%
  dplyr::select(MG_ug_per_kcal_food_moist)
View (kibble_mg)

# 5a extract data for prism CML plus MG canned (this is the kcal version)
canned_cml_plus_mg <- data_moisture %>%
  filter(Type== "Canned") %>%
  dplyr::select(CML_plus_MG_kcal_moist)
View (canned_cml_plus_mg)

# 6a extract data for prism CML plus MG kibble 
kibble_cml_plus_mg <- data_moisture %>%
  filter(Type== "Kibble") %>%
  dplyr::select(CML_plus_MG_kcal_moist)
View (kibble_cml_plus_mg)

# 7a extract data for prism SF canned
canned_sf <- data_moisture %>%
  filter(Type== "Canned") %>%
  dplyr::select(SF_ug_per_kcal_food_moist)
View (canned_sf)

# 8a extract data for prism SF kibble
kibble_sf <- data_moisture %>%
  filter(Type== "Kibble") %>%
  dplyr::select(SF_ug_per_kcal_food_moist)
View (kibble_sf)

# 9a extract data for prism LF canned
canned_lf <- data_moisture %>%
  filter(Type== "Canned") %>%
  dplyr::select(LF_ug_per_kcal_food_moist)
View (canned_lf)

# 10a extract data for prism LF kibble
kibble_lf <- data_moisture %>%
  filter(Type== "Kibble") %>%
  dplyr::select(LF_ug_per_kcal_food_moist)
View (kibble_lf)

#Now start making same plots for ug per gram food
#==============================
#add a new column that calculates CML+ MG using ug per gram food

# 1 extract data for prism CML canned in ug per gram
canned <- data_moisture %>%
  filter(Type == "Canned") %>%
  dplyr::select(CML_ug_per_g_food_moist)
View (canned)

# 2 extract data for prism CML kibble
kibble <- data_moisture %>%
  filter(Type== "Kibble") %>%
  dplyr::select(CML_ug_per_g_food_moist)
View (kibble)

# 3 extract data for prism MG canned
canned_mg <- data_moisture %>%
  filter(Type== "Canned") %>%
  dplyr::select(MG_ug_per_g_food_moist)
View (canned_mg)

# 4 extract data for prism MG kibble
kibble_mg <- data_moisture %>%
  filter(Type== "Kibble") %>%
  dplyr::select(MG_ug_per_g_food_moist)
View (kibble_mg)

# 5 extract data for prism CML plus MG canned (this is the ug per g version)
canned_cml_plus_mg <- data_moisture %>%
  filter(Type== "Canned") %>%
  dplyr::select(CML_plus_MG_g_moist)
View (canned_cml_plus_mg)

# 6 extract data for prism CML plus MG kibble 
kibble_cml_plus_mg <- data_moisture %>%
  filter(Type== "Kibble") %>%
  dplyr::select(CML_plus_MG_g_moist)
View (kibble_cml_plus_mg)

# 7 extract data for prism SF canned
canned_sf <- data_moisture %>%
  filter(Type== "Canned") %>%
  dplyr::select(SF_ug_per_g_food_moist)
View (canned_sf)

# 8 extract data for prism SF kibble
kibble_sf <- data_moisture %>%
  filter(Type== "Kibble") %>%
  dplyr::select(SF_ug_per_g_food_moist)
View (kibble_sf)

# 9 extract data for prism LF canned
canned_lf <- data_moisture %>%
  filter(Type== "Canned") %>%
  dplyr::select(LF_AGE_ug_per_g_food_moist)
View (canned_lf)

# 10 extract data for prism LF kibble
kibble_lf <- data_moisture %>%
  filter(Type== "Kibble") %>%
  dplyr::select(LF_AGE_ug_per_g_food_moist)
View (kibble_lf)

#now re make all the plots for Fig 3, 

#make a new scatter plot to see AGE measure vs ingredients to find which ones are p value signif

#continue here on Nov 5th 2024========================================================================

# here we will use moisture data set
# CML
scatter_plot_by_type_label_left(data_moisture,"Percent_moisture" ,"CML_ug_per_g_food_moist", "Canned") #0.717 moist
scatter_plot_by_type_label_left(data_moisture,"Percent_max_Crude_protein" ,"CML_ug_per_g_food_moist", "Canned") # 0.253 moist
scatter_plot_by_type_label_left(data_moisture,"Percent_max_Crude_fat", "CML_ug_per_g_food_moist","Canned") #0.198 moist
scatter_plot_by_type_label_left(data_moisture,"Percent_Crude_fiber" ,"CML_ug_per_g_food_moist", "Canned") #0.654 moist

scatter_plot_by_type_label_left(data_moisture,"Percent_moisture" ,"CML_ug_per_g_food_moist", "Kibble") #signif #0.00694 moist
scatter_plot_by_type_label_left(data_moisture,"Percent_max_Crude_protein" ,"CML_ug_per_g_food_moist", "Kibble")  #0.437 moist
scatter_plot_by_type_label_left(data_moisture,"Percent_max_Crude_fat", "CML_ug_per_g_food_moist","Kibble")  #0.493 moist
scatter_plot_by_type_label_left(data_moisture,"Percent_Crude_fiber" ,"CML_ug_per_g_food_moist", "Kibble")  #0.91 moist

#MG
scatter_plot_by_type_label_left(data_moisture,"Percent_moisture" ,"MG_ug_per_g_food_moist", "Canned") # 0.0451 moist ****
scatter_plot_by_type_label_left(data_moisture,"Percent_max_Crude_protein" ,"MG_ug_per_g_food_moist", "Canned") # signif 0.00018 moist******
scatter_plot_by_type_label_left(data_moisture,"Percent_max_Crude_fat", "MG_ug_per_g_food_moist","Canned") #0.422 moist
scatter_plot_by_type_label_left(data_moisture,"Percent_Crude_fiber" ,"MG_ug_per_g_food_moist", "Canned") # 0.0014 moist****

scatter_plot_by_type_label_left(data_moisture,"Percent_moisture" ,"MG_ug_per_g_food_moist", "Kibble")  #Not signif 0.942 moist
scatter_plot_by_type_label_left(data_moisture,"Percent_max_Crude_protein" ,"MG_ug_per_g_food_moist", "Kibble")  #Not signif 0.517 moist
scatter_plot_by_type_label_left(data_moisture,"Percent_max_Crude_fat", "MG_ug_per_g_food_moist","Kibble")  #Not signif 0.717 moist
scatter_plot_by_type_label_left(data_moisture,"Percent_Crude_fiber" ,"MG_ug_per_g_food_moist", "Kibble")  #Not signif 0.839 moist

#SF
scatter_plot_by_type_label_left(data_moisture,"Percent_moisture" ,"SF_ug_per_g_food_moist", "Canned") #signif 0.0196 moist
scatter_plot_by_type_label_left(data_moisture,"Percent_max_Crude_protein" ,"SF_ug_per_g_food_moist", "Canned") #Not signif 0.991 moist
scatter_plot_by_type_label_left(data_moisture,"Percent_max_Crude_fat", "SF_ug_per_g_food_moist","Canned")  ## 0.571 not sig moist
scatter_plot_by_type_label_left(data_moisture,"Percent_Crude_fiber" ,"SF_ug_per_g_food_moist", "Canned") #Not signif 0.438 moist


scatter_plot_by_type_label_left(data_moisture,"Percent_moisture" ,"SF_ug_per_g_food_moist", "Kibble") #Not signif 0.789 m
scatter_plot_by_type_label_left(data_moisture,"Percent_max_Crude_protein" ,"SF_ug_per_g_food_moist", "Kibble")  #Not signif 0.154 m
scatter_plot_by_type_label_left(data_moisture,"Percent_max_Crude_fat", "SF_ug_per_g_food_moist","Kibble") #Not signif 0.743 m
scatter_plot_by_type_label_left(data_moisture,"Percent_Crude_fiber" ,"SF_ug_per_g_food_moist", "Kibble") #Not signif 0.72 m

#LF plots
scatter_plot_by_type_label_left(data_moisture,"Percent_moisture" ,"LF_AGE_ug_per_g_food_moist", "Canned")  # ** 0.00485 m
scatter_plot_by_type_label_left(data_moisture,"Percent_max_Crude_protein" ,"LF_AGE_ug_per_g_food_moist", "Canned")  # ** 0.000166 m
scatter_plot_by_type_label_left(data_moisture,"Percent_max_Crude_fat", "LF_AGE_ug_per_g_food_moist","Canned")   # 0.33 not signif m
scatter_plot_by_type_label_left(data_moisture,"Percent_Crude_fiber" ,"LF_AGE_ug_per_g_food_moist", "Canned") # ** 0.0197 m

scatter_plot_by_type_label_left(data_moisture,"Percent_moisture" ,"LF_AGE_ug_per_g_food_moist", "Kibble") # not signif 0.0833 m
scatter_plot_by_type_label_left(data_moisture,"Percent_max_Crude_protein" ,"LF_AGE_ug_per_g_food_moist", "Kibble") # yes signif 0.0206 m
scatter_plot_by_type_label_left(data_moisture,"Percent_max_Crude_fat", "LF_AGE_ug_per_g_food_moist","Kibble") # not signif 0.605 m
scatter_plot_by_type_label_left(data_moisture,"Percent_Crude_fiber" ,"LF_AGE_ug_per_g_food_moist", "Kibble") #not signif 0.784 m

#CML plus MG=== 
scatter_plot_by_type_label_left(data_moisture,"Percent_moisture" ,"CML_plus_MG_g_moist", "Canned") ## 0.318 not signif moist
scatter_plot_by_type_label_left(data_moisture,"Percent_max_Crude_protein" ,"CML_plus_MG_g_moist", "Canned") # not signif 0.578 moist
scatter_plot_by_type_label_left(data_moisture,"Percent_max_Crude_fat", "CML_plus_MG_g_moist","Canned")  # not signif 0.291 moist
scatter_plot_by_type_label_left(data_moisture,"Percent_Crude_fiber" ,"CML_plus_MG_g_moist", "Canned")  ## not signif 0.0597 moist

scatter_plot_by_type_label_left(data_moisture,"Percent_moisture" ,"CML_plus_MG_g_moist", "Kibble") # not signif 0.122 moist
scatter_plot_by_type_label_left(data_moisture,"Percent_max_Crude_protein" ,"CML_plus_MG_g_moist", "Kibble")  #Not signif 0.332 moist
scatter_plot_by_type_label_left(data_moisture,"Percent_max_Crude_fat", "CML_plus_MG_g_moist","Kibble")  #Not signif 0.441 moist
scatter_plot_by_type_label_left(data_moisture,"Percent_Crude_fiber" ,"CML_plus_MG_g_moist", "Kibble") #Not signif 0.512 moist

#new "combined" based on Dr Turner's paper draft 11-12-24

scatter_plot_by_type_label_left(data_moisture,"Percent_moisture" ,"Combined_g_moist", "Canned") ## 0.169 non signif moist
scatter_plot_by_type_label_left(data_moisture,"Percent_max_Crude_protein" ,"Combined_g_moist", "Canned") # 0.77 non signif moist
scatter_plot_by_type_label_left(data_moisture,"Percent_max_Crude_fat", "Combined_g_moist","Canned")  # 0.995 non signif moist
scatter_plot_by_type_label_left(data_moisture,"Percent_Crude_fiber" ,"Combined_g_moist", "Canned")  ## 0.758 non signif moist

scatter_plot_by_type_label_left(data_moisture,"Percent_moisture" ,"Combined_g_moist", "Kibble") # 0.923 non signif moist
scatter_plot_by_type_label_left(data_moisture,"Percent_max_Crude_protein" ,"Combined_g_moist", "Kibble")  # 0.184 non signif moist
scatter_plot_by_type_label_left(data_moisture,"Percent_max_Crude_fat", "Combined_g_moist","Kibble")  #0.513 non signif moist
scatter_plot_by_type_label_left(data_moisture,"Percent_Crude_fiber" ,"Combined_g_moist", "Kibble") #0.509 non signif moist

# FDR correction for ingredient correlations
# Load necessary libraries
library(readxl)
library(writexl)
install.packages("writexl")

# Load the data from the uploaded file
file_path <- "correlations_results11-12-24_compiled_FDR.xlsx"
data_fdr <- read_excel(file_path)

# Display column names to confirm p-value column (e.g., "P_Value")
print(names(data_fdr))

# Assuming the p-value column is named "P_Value" (adjust if necessary)
# Apply Benjamini-Hochberg (BH) correction for FDR
data_fdr$FDR_Adjusted_P <- p.adjust(data_fdr$P_Value, method = "BH")

# Save the updated data with FDR-adjusted p-values to a new file
output_path <- "correlations_results_FDR_corrected.xlsx"
write_xlsx(data_fdr, output_path)

#Figure 2 related plots among AGE measures correlations.

scatter_plot_by_type_label_right(data_moisture,"CML_ug_per_g_food_moist", "SF_ug_per_g_food_moist","Canned") # sign 0.00329 m
scatter_plot_by_type_label_right(data_moisture,"CML_ug_per_g_food_moist", "SF_ug_per_g_food_moist","Kibble") #not signif 0.653 m

scatter_plot_by_type_label_left(data_moisture,"MG_ug_per_g_food_moist", "SF_ug_per_g_food_moist","Canned") # 0.126 not signif m
scatter_plot_by_type_label_right(data_moisture,"MG_ug_per_g_food_moist", "SF_ug_per_g_food_moist","Kibble") # 0.00018 sign m

scatter_plot_by_type_label_left(data_moisture,"CML_plus_MG_g_moist", "SF_ug_per_g_food_moist","Canned") # 0.00436 m
scatter_plot_by_type_label_right(data_moisture,"CML_plus_MG_g_moist", "SF_ug_per_g_food_moist","Kibble") # 0.004 sign m

scatter_plot_by_type_label_left(data_moisture,"CML_ug_per_g_food_moist", "MG_ug_per_g_food_moist","Canned") #0.353
scatter_plot_by_type_label_right(data_moisture,"CML_ug_per_g_food_moist", "MG_ug_per_g_food_moist","Kibble") # signif 0.0233 m



##================================================================================================
#summary statistics
library(tidyverse)
# Calculate summary statistics for each measure by type
summary_stats <- data_moisture %>%
  select(Type, CML_ug_per_g_food_moist, MG_ug_per_g_food_moist, LF_AGE_ug_per_g_food_moist, SF_ug_per_g_food_moist, CML_ug_per_kcal_food_moist, 
         MG_ug_per_kcal_food_moist, LF_ug_per_kcal_food_moist, SF_ug_per_kcal_food_moist, CML_plus_MG_g_moist, CML_plus_MG_kcal_moist) %>%
  group_by(Type) %>%
  summarise(across(everything(), list(min = ~min(., na.rm = TRUE),
                                      max = ~max(., na.rm = TRUE),
                                      mean = ~mean(., na.rm = TRUE), 
                                      median = ~median(., na.rm = TRUE), 
                                      sd = ~sd(., na.rm = TRUE))))

# Save the summary statistics as a CSV file
write.csv(summary_stats, 'summary_statistics_by_type11-07-24.csv', row.names = FALSE)
View(summary_stats)

###======================FIGURE 5 ===================================================================
# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

#load data
data <-  ELISA_and_fluorescence_restruc_GB1_norm_kcal_3_super_high_cml_removed

# Exclude the dog foods with the type "fresh"
data <- data %>%
  filter(Type != "Fresh")

# Concatenate Make, Description, and ID for better labeling
#data <- data %>%
 # mutate(DogFoodLabel = paste(Make, Description, ID, sep = " - "))
#the above is the older version but now we do not want to show the actual dogfood names, only numbers

#add a new column that calls IDs the label.
data <- data %>%
  mutate(DogFoodLabel2 = ID)

View(data)

#big mutate block to add necessary calculations as new columns
data <- data %>%
  mutate(Total_AGE_Score_g = CML_ug_per_g_food_moist + MG_ug_per_g_food_moist + LF_AGE_ug_per_g_food_moist + SF_ug_per_g_food_moist) %>%
  mutate(Total_AGE_Score_kcal = CML_ug_per_kcal_food + MG_ug_per_kcal_food + LF_ug_per_kcal_food + SF_ug_per_kcal_food) %>%
  mutate(CML_plus_MG_g_moist = CML_ug_per_g_food_moist + MG_ug_per_g_food_moist) %>%
  mutate(CML_plus_MG_kcal = CML_ug_per_kcal_food + MG_ug_per_kcal_food) %>%
  mutate(Combined_fluo_g= LF_AGE_ug_per_g_food_moist + SF_ug_per_g_food_moist) %>%
  mutate(Combined_fluo_kcal= LF_ug_per_kcal_food + SF_ug_per_kcal_food)

# ======= Figure 5C Order the dog foods by increasing total AGE score in ug per gram
data <- data %>%
  arrange(Total_AGE_Score_g)

# Ensure the factor levels of DogFoodLabel are in the correct order
#data$DogFoodLabel <- factor(data$DogFoodLabel, levels = data$DogFoodLabel) #old version with food names.
data$DogFoodLabel2 <- factor(data$DogFoodLabel2, levels = data$DogFoodLabel2)

# Create a long format for the AGE measures (has dogfood names)
# data_long <- data %>%
 # select(DogFoodLabel, CML_ug_per_g_food_moist, MG_ug_per_g_food_moist, LF_AGE_ug_per_g_food_moist, SF_ug_per_g_food_moist) %>%
 # pivot_longer(cols = c(CML_ug_per_g_food_moist, MG_ug_per_g_food_moist, LF_AGE_ug_per_g_food_moist, SF_ug_per_g_food_moist),
      #         names_to = "AGE_Measure",
        #       values_to = "Value")

# Create a long format for the AGE measures (no dogfood names)
data_long <- data %>%
  select(DogFoodLabel2, CML_ug_per_g_food_moist, MG_ug_per_g_food_moist, LF_AGE_ug_per_g_food_moist, SF_ug_per_g_food_moist) %>%
  pivot_longer(cols = c(CML_ug_per_g_food_moist, MG_ug_per_g_food_moist, LF_AGE_ug_per_g_food_moist, SF_ug_per_g_food_moist),
               names_to = "AGE_Measure",
               values_to = "Value")

# Create the stacked bar plot Fig 5C
p <- ggplot(data_long, aes(x = DogFoodLabel2, y = Value, fill = AGE_Measure)) +
  geom_bar(stat = "identity") +
  labs(title = "Total AGE Score for Dog Foods",
       x = "Dog Food",
       y = "Total AGE Score",
       fill = "AGE Measure") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 13),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(hjust = 0.5, size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20)) +
  scale_fill_manual(values = c("CML_ug_per_g_food_moist" = "red",
                               "MG_ug_per_g_food_moist" = "blue",
                               "LF_AGE_ug_per_g_food_moist" = "darkgreen",
                               "SF_ug_per_g_food_moist" = "darkgray"))
p


#============================= new version september 25 2024 to remove legal dogfood names:

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Load data
data <- ELISA_and_fluorescence_restruc_GB1_norm_kcal_3_super_high_cml_removed

# Exclude the dog foods with the type "Fresh"
data <- data %>%
  filter(Type != "Fresh")

# Create a new column that combines ID and Type for the x-axis label
data <- data %>%
  mutate(DogFoodLabel = paste(ID, Type, sep = " - "))

# Perform necessary calculations
data <- data %>%
  mutate(Total_AGE_Score_g = CML_ug_per_g_food_moist + MG_ug_per_g_food_moist + LF_AGE_ug_per_g_food_moist + SF_ug_per_g_food_moist) %>%
  mutate(Total_AGE_Score_kcal = CML_ug_per_kcal_food + MG_ug_per_kcal_food + LF_ug_per_kcal_food + SF_ug_per_kcal_food) %>%
  mutate(CML_plus_MG_g_moist = CML_ug_per_g_food_moist + MG_ug_per_g_food_moist) %>%
  mutate(CML_plus_MG_kcal = CML_ug_per_kcal_food + MG_ug_per_kcal_food) %>%
  mutate(Combined_fluo_g= LF_AGE_ug_per_g_food_moist + SF_ug_per_g_food_moist) %>%
  mutate(Combined_fluo_kcal= LF_ug_per_kcal_food + SF_ug_per_kcal_food)

# Order the dog foods by increasing total AGE score in ug per gram
data <- data %>%
  arrange(Total_AGE_Score_g)

# Ensure the factor levels of DogFoodLabel are in the correct order
data$DogFoodLabel <- factor(data$DogFoodLabel, levels = data$DogFoodLabel)

# Create a long format for the AGE measures
data_long <- data %>%
  select(DogFoodLabel, CML_ug_per_g_food_moist, MG_ug_per_g_food_moist, LF_AGE_ug_per_g_food_moist, SF_ug_per_g_food_moist) %>%
  pivot_longer(cols = c(CML_ug_per_g_food_moist, MG_ug_per_g_food_moist, LF_AGE_ug_per_g_food_moist, SF_ug_per_g_food_moist),
               names_to = "AGE_Measure",
               values_to = "Value")

# Create the stacked bar plot
p <- ggplot(data_long, aes(x = DogFoodLabel, y = Value, fill = AGE_Measure)) +
  geom_bar(stat = "identity") +
  labs(title = "Total AGE Score for Dog Foods",
       x = "Dog Food (ID - Type)",
       y = "Total AGE Score",
       fill = "AGE Measure") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 13),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(hjust = 0.5, size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20)) +
  scale_fill_manual(values = c("CML_ug_per_g_food_moist" = "red",
                               "MG_ug_per_g_food_moist" = "blue",
                               "LF_AGE_ug_per_g_food_moist" = "darkgreen",
                               "SF_ug_per_g_food_moist" = "darkgray"))
# Display the plot
p

#======= Figure 5D Order the dog foods by increasing total AGE score in kcal
#rearrange
data <- data %>%
  arrange(Total_AGE_Score_kcal)

data_long_kcal <- data %>%
  select(DogFoodLabel2, CML_ug_per_kcal_food, MG_ug_per_kcal_food, LF_ug_per_kcal_food, SF_ug_per_kcal_food) %>%
  pivot_longer(cols = c(CML_ug_per_kcal_food, MG_ug_per_kcal_food, LF_ug_per_kcal_food, SF_ug_per_kcal_food),
               names_to = "AGE_Measure_per_kcal",
               values_to = "Value")

# make the same total AGE score stacked bar plot using kcal normalization

p <- ggplot(data_long_kcal, aes(x = DogFoodLabel2, y = Value, fill = AGE_Measure_per_kcal)) +
  geom_bar(stat = "identity") +
  labs(title = "Total AGE Score for Dog Foods (ug per kcal)",
       x = "Dog Food",
       y = "Total AGE Score",
       fill = "AGE Measure") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 13),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(hjust = 0.5, size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20)) +
  scale_fill_manual(values = c("CML_ug_per_kcal_food" = "red",
                               "MG_ug_per_kcal_food" = "blue",
                               "LF_ug_per_kcal_food" = "darkgreen",
                               "SF_ug_per_kcal_food" = "darkgray"))

p

# Save the plot as a PNG file Figure 5
ggsave(filename = "total_AGE_score_by_dog_food.png", plot = p, width = 12, height = 8, units = "in", dpi = 300)

# Print the plot
print(p)

##=========================================== 11-06-24 version moist================
## this is still the stacked plot, but making it interactive high resolution and color coded.
# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)

# Load data
data2 <- ELISA_and_fluorescence_restruc_GB1_norm_kcal_3CML_NAed_VJF_edits_moisture_normalization

# Exclude the dog foods with the type "fresh"
data2 <- data2 %>%
  filter(Type != "Fresh") %>%
  drop_na(CML_ug_per_g_food_moist) 

# Concatenate Make, Description, Type, and ID for better labeling
data2 <- data2 %>%
  mutate(DogFoodLabelFull = paste(Make, Description,ID, sep = " - "))

# Big mutate block to add necessary calculations as new columns
data2 <- data2 %>%
  mutate(Total_AGE_Score_g_moist = CML_ug_per_g_food_moist + MG_ug_per_g_food_moist + LF_AGE_ug_per_g_food_moist + SF_ug_per_g_food_moist) %>%
  mutate(Total_AGE_Score_kcal_moist = CML_ug_per_kcal_food_moist + MG_ug_per_kcal_food_moist + LF_ug_per_kcal_food_moist + SF_ug_per_kcal_food_moist) %>%
  mutate(CML_plus_MG_g_moist = CML_ug_per_g_food_moist + MG_ug_per_g_food_moist) %>%
  mutate(CML_plus_MG_kcal_moist = CML_ug_per_kcal_food_moist + MG_ug_per_kcal_food_moist) %>%
  mutate(Combined_fluo_g_moist= LF_AGE_ug_per_g_food_moist + SF_ug_per_g_food_moist) %>%
  mutate(Combined_fluo_kcal_moist= LF_ug_per_kcal_food_moist + SF_ug_per_kcal_food_moist)

# Order the dog foods by increasing total AGE score in ug per gram
data2 <- data2 %>%
  arrange(Total_AGE_Score_g_moist)

# Ensure the factor levels of DogFoodLabelFull are in the correct order
data2$DogFoodLabelFull <- factor(data2$DogFoodLabelFull, levels = data2$DogFoodLabelFull)

# Create a long format for the AGE measures
data_long <- data2 %>%
  select(DogFoodLabelFull, Type, CML_ug_per_g_food_moist, MG_ug_per_g_food_moist, LF_AGE_ug_per_g_food_moist, SF_ug_per_g_food_moist) %>%
  pivot_longer(cols = c(CML_ug_per_g_food_moist, MG_ug_per_g_food_moist, LF_AGE_ug_per_g_food_moist, SF_ug_per_g_food_moist),
               names_to = "AGE_Measure",
               values_to = "Value") %>%
  filter(!is.na(Value) & Value > 0)  # Remove NA or zero values

# Create the stacked bar plot with ggplot2
p <- ggplot(data_long, aes(x = DogFoodLabelFull, y = Value, fill = AGE_Measure,
                           text = paste("Dog Food:", DogFoodLabelFull, "<br>Type: ", Type))) +
  geom_bar(stat = "identity") +
  labs(title = "Total AGE Score for Dog Foods", x= NULL,
       y = "Total AGE Score",
       fill = "AGE Measure") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 13),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(hjust = 0.5, size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20)) +
  scale_fill_manual(values = c("CML_ug_per_g_food_moist" = "red",
                               "MG_ug_per_g_food_moist" = "blue",
                               "LF_AGE_ug_per_g_food_moist" = "darkgreen",
                               "SF_ug_per_g_food_moist" = "darkgray"))

# Convert ggplot to plotly object for interactivity
p_interactive <- ggplotly(p, tooltip = "text")

# Show the interactive plot
p_interactive

#this one above is good.

#=============================now lets make canned and kibble separately
# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(htmlwidgets)  # For saving as HTML

# Exclude the dog foods with the type "fresh" and any rows with NA in CML_ug_per_g_food_moist
data <- data_moisture %>%
  filter(Type != "Fresh") %>%
  drop_na(CML_ug_per_g_food_moist)  # Drop rows with NA in CML measure

# Concatenate Make, Description, Type, and ID for better labeling
data <- data %>%
  mutate(DogFoodLabelFull = paste(Make, Description, ID, sep = " - "))

# Big mutate block to add necessary calculations as new columns
data <- data %>%
  mutate(Total_AGE_Score_g_moist = CML_ug_per_g_food_moist + MG_ug_per_g_food_moist + LF_AGE_ug_per_g_food_moist + SF_ug_per_g_food_moist) %>%
  mutate(Total_AGE_Score_kcal_moist = CML_ug_per_kcal_food_moist + MG_ug_per_kcal_food_moist + LF_ug_per_kcal_food_moist + SF_ug_per_kcal_food_moist) %>%
  mutate(CML_plus_MG_g_moist = CML_ug_per_g_food_moist + MG_ug_per_g_food_moist) %>%
  mutate(CML_plus_MG_kcal_moist = CML_ug_per_kcal_food_moist + MG_ug_per_kcal_food_moist) %>%
  mutate(Combined_fluo_g_moist = LF_AGE_ug_per_g_food_moist + SF_ug_per_g_food_moist) %>%
  mutate(Combined_fluo_kcal_moist = LF_ug_per_kcal_food_moist + SF_ug_per_kcal_food_moist)

# Create function to generate a horizontal stacked bar plot and save it
generate_plot <- function(filtered_data, file_name) {
  
  # Order the filtered data by Total_AGE_Score_g_moist for correct plotting
  filtered_data <- filtered_data %>%
    arrange(Total_AGE_Score_g_moist) %>%
    mutate(DogFoodLabelFull = factor(DogFoodLabelFull, levels = unique(DogFoodLabelFull)))  # Set factor levels in correct order
  
  # Create a long format for the AGE measures
  data_long <- filtered_data %>%
    select(DogFoodLabelFull, Type, CML_ug_per_g_food_moist, MG_ug_per_g_food_moist, LF_AGE_ug_per_g_food_moist, SF_ug_per_g_food_moist) %>%
    pivot_longer(cols = c(CML_ug_per_g_food_moist, MG_ug_per_g_food_moist, LF_AGE_ug_per_g_food_moist, SF_ug_per_g_food_moist),
                 names_to = "AGE_Measure",
                 values_to = "Value") %>%
    filter(!is.na(Value) & Value > 0)  # Remove NA or zero values
  
  # Create the stacked bar plot with ggplot2
  p <- ggplot(data_long, aes(x = DogFoodLabelFull, y = Value, fill = AGE_Measure,
                             text = paste("Dog Food:", DogFoodLabelFull, "<br>Type: ", Type))) +
    geom_bar(stat = "identity") +
    labs(title = paste("Total AGE Score for", unique(filtered_data$Type), "Dog Foods"),
         x = NULL,
         y = "Total AGE Score",
         fill = "AGE Measure") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 13),
          axis.text.x = element_text(angle = 90, hjust = 1, size = 13),
          plot.title = element_text(hjust = 0.5, size = 20),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 20)) +
    scale_fill_manual(values = c("CML_ug_per_g_food_moist" = "red",
                                 "MG_ug_per_g_food_moist" = "blue",
                                 "LF_AGE_ug_per_g_food_moist" = "darkgreen",
                                 "SF_ug_per_g_food_moist" = "darkgray")) 
  
  # Convert ggplot to plotly object for interactivity
  p_interactive <- ggplotly(p, tooltip = "text")
  
  # Save the interactive plot as an HTML file
  saveWidget(p_interactive, file_name)
}

# Filter data for Canned and Kibble and generate plots
canned_data <- data %>% filter(Type == "Canned")
kibble_data <- data %>% filter(Type == "Kibble")

# Generate and save the plots as separate HTML files
generate_plot(canned_data, "canned_dog_foods.html")
generate_plot(kibble_data, "kibble_dog_foods.html")
generate_plot(data, "All_dog_foods.html") #this did not work



#====================== 8-26-24 Vick's question can we plot only CML as ranked bars: for Fig 5A (cml in ug per g) and B (cml kcal)
#Version 1 for 8-26-24 is ug per gram food

# Concatenate Make, Description, and ID for better labeling
data <-  ELISA_and_fluorescence_restruc_GB1_norm_kcal_3_super_high_cml_removed

# Exclude the dog foods with the type "fresh"
data <- data %>%
  filter(Type != "Fresh")

data <- data %>%
  mutate(Total_AGE_Score_g = CML_ug_per_g_food_moist + MG_ug_per_g_food_moist + LF_AGE_ug_per_g_food_moist + SF_ug_per_g_food_moist) %>%
  mutate(Total_AGE_Score_kcal = CML_ug_per_kcal_food + MG_ug_per_kcal_food + LF_ug_per_kcal_food + SF_ug_per_kcal_food) %>%
  mutate(CML_plus_MG_g_moist = CML_ug_per_g_food_moist + MG_ug_per_g_food_moist) %>%
  mutate(CML_plus_MG_kcal = CML_ug_per_kcal_food + MG_ug_per_kcal_food) %>%
  mutate(Combined_fluo_g= LF_AGE_ug_per_g_food_moist + SF_ug_per_g_food_moist) %>%
  mutate(Combined_fluo_kcal= LF_ug_per_kcal_food + SF_ug_per_kcal_food)

data <- data %>%
  mutate(DogFoodLabel2 = ID)

# Order the dog foods by increasing CML
data <- data %>%
  arrange(CML_ug_per_g_food_moist)

# Ensure the factor levels of DogFoodLabel are in the correct order
data$DogFoodLabel2 <- factor(data$DogFoodLabel2, levels = data$DogFoodLabel2)

# Create a long format for the AGE measures
data_long <- data %>%
  select(DogFoodLabel2, CML_ug_per_g_food_moist) %>%
  pivot_longer(cols = c(CML_ug_per_g_food_moist),
               names_to = "CML_AGE_Measure",
               values_to = "Value")

# Create the stacked bar plot
p <- ggplot(data_long, aes(x = DogFoodLabel2, y = Value, fill = CML_AGE_Measure)) +
  geom_bar(stat = "identity") +
  labs(title = "CML amount in Dog Foods",
       x = "Dog Food",
       y = "CML amount (ug per gram food)",
       fill = "CML_AGE Measure") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 13),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(hjust = 0.5, size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20)) +
  scale_fill_manual(values = c("CML_ug_per_g_food_moist" = "red"))
p

#================================================
#VErsion 2 for 8-26-24 is CML bar plot ug per kcal food

# Order the dog foods by increasing CML
data <- data %>%
  arrange(CML_ug_per_kcal_food)

# Ensure the factor levels of DogFoodLabel are in the correct order
data$DogFoodLabel2 <- factor(data$DogFoodLabel2, levels = data$DogFoodLabel2)

# Create a long format for the AGE measures
data_long <- data %>%
  select(DogFoodLabel2, CML_ug_per_kcal_food) %>%
  pivot_longer(cols = c(CML_ug_per_kcal_food),
               names_to = "CML_AGE_Measure_per_kcal",
               values_to = "Value")

# Create the stacked bar plot
p <- ggplot(data_long, aes(x = DogFoodLabel2, y = Value, fill = CML_AGE_Measure_per_kcal)) +
  geom_bar(stat = "identity") +
  labs(title = "CML amount in Dog Foods",
       x = "Dog Food",
       y = "CML amount (ug per kcal food)",
       fill = "CML_AGE_Measure_per_kcal") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 13),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(hjust = 0.5, size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20)) +
  scale_fill_manual(values = c("CML_ug_per_kcal_food" = "red"))
p

#######=========================================================================

#try chat gpt made version to make correlations and pca s

# Load necessary libraries

install.packages ("GGally")
library(readxl)
library(ggplot2)
library(GGally)
library(corrplot)
library(reshape2)

# Load the data: data_3or

# Select relevant columns for AGE measurements
age_columns <- c('CML_ug_per_g_food_moist', 'MG_ug_per_g_food_moist', 'LF_AGE_ug_per_g_food_moist', 'SF_ug_per_g_food_moist','Type')

ages_data <- data_3or[, age_columns, drop = FALSE]


#=========================PCA ===================
# 7-22-24 make the 3d pca plot with code pilot

# Load required libraries
library(readxl)
library(ggplot2)
library(ggfortify)
library(plotly)
library(cluster) # For silhouette analysis
library(factoextra) # For visualization

# Remove Fresh foods
data_3or_use <- data_3or[data_3or$Type != 'Fresh', ]

# Add a column for shape based on 'Canned' or 'Kibble'
shape_column <- ifelse(grepl('Canned', y), 'circle', 'square')

# Select relevant columns for PCA
features <- c('CML_ug_per_g_food_moist', 'MG_ug_per_g_food_moist', 'LF_AGE_ug_per_g_food_moist', 'SF_ug_per_g_food_moist', 'CML_ug_per_kcal_food', 'MG_ug_per_kcal_food', 'LF_ug_per_kcal_food', 'SF_ug_per_kcal_food')

x <- data_3or_use[features]
y <- data_3or_use$Type

# Standardize the data
x_scaled <- scale(x)

# Perform PCA to reduce to 3 components
pca <- prcomp(x_scaled, center = TRUE, scale. = TRUE)
summary(pca)

# Extract the PCA results
pca_data <- as.data.frame(pca$x[, 1:5])
colnames(pca_data) <- c("PC1", "PC2", "PC3", "PC4", "PC5")
pca_data$Type <- y
pca_data$Shape <- shape_column

# Elbow Method for optimal clusters
set.seed(123) # For reproducibility
wss <- sapply(1:10, function(k){
  kmeans(pca_data[, 1:5], k, nstart = 10)$tot.withinss
})

elbow_plot <- qplot(1:10, wss, geom = 'line') + 
  ggtitle('Elbow Method for Optimal Clusters') +
  xlab('Number of clusters') +
  ylab('Within-cluster sum of squares')

elbow_plot

# Silhouette Method for optimal clusters
silhouette_scores <- sapply(2:10, function(k){
  km <- kmeans(pca_data[, 1:5], centers = k, nstart = 10)
  ss <- silhouette(km$cluster, dist(pca_data[, 1:5]))
  mean(ss[, 3])
})

silhouette_plot <- qplot(2:10, silhouette_scores, geom = 'line') +
  ggtitle('Silhouette Method for Optimal Clusters') +
  xlab('Number of clusters') +
  ylab('Average silhouette width')

silhouette_plot

# Perform K-means clustering
set.seed(123) # For reproducibility
kmeans_result <- kmeans(pca_data[, 1:3], centers = 2)
pca_data$Cluster <- as.factor(kmeans_result$cluster)

# Plotting the 3D PCA with clusters and shapes
plot <- plot_ly(pca_data, x = ~PC1, y = ~PC2, z = ~PC3, color = ~Cluster, symbol = ~Shape, text = ~Type, symbols = c('circle', 'square'), colors = 'Set1') %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Principal Component 1'),
                      yaxis = list(title = 'Principal Component 2'),
                      zaxis = list(title = 'Principal Component 3')),
         title = '3D PCA of Dog Food Types with Clusters and Shapes')

plot


#========================== remake 3D PCA plot to show ovals around clusters DATE 8-29-24
# remake the 3d pca plot with code pilot

# Load required libraries
library(readxl)
library(ggplot2)
library(ggfortify)
library(plotly)
library(MASS)  # For generating ellipsoids
library(cluster) # For silhouette analysis
library(factoextra) # For visualization

# Remove Fresh foods
data_3or_use <- ELISA_and_fluorescence_restruc_GB1_norm_kcal_3_super_high_cml_removed
data_3or_use <- data_3or_use[data_3or_use$Type != 'Fresh', ]

# Select relevant columns for PCA
features <- c('CML_ug_per_g_food_moist', 'MG_ug_per_g_food_moist', 'LF_AGE_ug_per_g_food_moist', 'SF_ug_per_g_food_moist')
features2 <- c('CML_ug_per_kcal_food', 'MG_ug_per_kcal_food', 'LF_ug_per_kcal_food', 'SF_ug_per_kcal_food')
x <- data_3or_use[features]
x2 <- data_3or_use[features2]
y <- data_3or_use$Type

# Add a column for shape based on 'Canned' or 'Kibble'
shape_column <- ifelse(grepl('Canned', y), 'circle', 'square')

# Standardize the data
x_scaled <- scale(x)
x2_scaled <- scale(x2)

# Perform PCA to reduce to 3 components
pca <- prcomp(x_scaled, center = TRUE, scale. = TRUE)
pca2 <- prcomp(x2_scaled, center = TRUE, scale. = TRUE)

# Extract the PCA results
pca_data <- as.data.frame(pca$x[, 1:3])
colnames(pca_data) <- c("PC1", "PC2", "PC3")
pca_data$Type <- y
pca_data$Shape <- shape_column

pca_data2 <- as.data.frame(pca2$x[, 1:3])
colnames(pca_data2) <- c("PC1", "PC2", "PC3")
pca_data2$Type <- y
pca_data2$Shape <- shape_column

# Perform K-means clustering with 2 clusters
set.seed(123) # For reproducibility
kmeans_result <- kmeans(pca_data[, 1:3], centers = 2)
pca_data$Cluster <- as.factor(kmeans_result$cluster)

kmeans_result2 <- kmeans(pca_data2[, 1:3], centers = 2)
pca_data2$Cluster <- as.factor(kmeans_result2$cluster)

# Function to generate ellipsoid data
generate_ellipsoid <- function(center, cov_matrix, n_points = 100) {
  sphere <- MASS::mvrnorm(n = n_points, mu = c(0, 0, 0), Sigma = diag(3))
  ellipsoid <- t(center + t(sphere %*% chol(cov_matrix)))
  return(ellipsoid)
}

# Plotting the 3D PCA with clusters, shapes, and ellipsoids
plot <- plot_ly()

# Add the data points
plot <- plot %>%
  add_markers(data = pca_data, x = ~PC1, y = ~PC2, z = ~PC3, color = ~Cluster, symbol = ~Shape, text = ~Type, symbols = c('circle', 'square'), colors = 'Set1')

# Add ellipsoids for each cluster
for (cluster_num in levels(pca_data$Cluster)) {
  cluster_data <- pca_data[pca_data$Cluster == cluster_num, 1:3]
  cov_matrix <- cov(cluster_data)
  center <- colMeans(cluster_data)
  
  ellipsoid_data <- generate_ellipsoid(center, cov_matrix)
  
  plot <- plot %>%
    add_trace(x = ellipsoid_data[, 1], y = ellipsoid_data[, 2], z = ellipsoid_data[, 3],
              type = 'mesh3d', opacity = 0.2, color = ~factor(cluster_num), showlegend = FALSE)
}

plot <- plot %>%
  layout(scene = list(xaxis = list(title = 'Principal Component 1'),
                      yaxis = list(title = 'Principal Component 2'),
                      zaxis = list(title = 'Principal Component 3')),
         title = '3D PCA of Dog Food Types with 2 Clusters, Shapes, and Ellipsoids')

# Plot the result
plot

#===================== try again, see if we can make an ellipse oval

# Load required libraries
library(readxl)
library(ggplot2)
library(ggfortify)
library(plotly)
library(MASS)  # For generating ellipsoids
install.packages("rgl")
library(rgl)  # For 3D visualization and ellipsoids

# Remove Fresh foods
data_3or_use <- ELISA_and_fluorescence_restruc_GB1_norm_kcal_3_super_high_cml_removed
data_3or_use <- data_3or_use[data_3or_use$Type != 'Fresh', ]

# Select relevant columns for PCA
features <- c('CML_ug_per_g_food_moist', 'MG_ug_per_g_food_moist', 'LF_AGE_ug_per_g_food_moist', 'SF_ug_per_g_food_moist')
features2 <- c('CML_ug_per_kcal_food', 'MG_ug_per_kcal_food', 'LF_ug_per_kcal_food', 'SF_ug_per_kcal_food')
x <- data_3or_use[features]
x2 <- data_3or_use[features2]
y <- data_3or_use$Type

# Add a column for shape based on 'Canned' or 'Kibble'
shape_column <- ifelse(grepl('Canned', y), 'circle', 'square')

# Standardize the data
x_scaled <- scale(x)
x2_scaled <- scale(x2)

# Perform PCA to reduce to 3 components
pca <- prcomp(x_scaled, center = TRUE, scale. = TRUE)
pca2 <- prcomp(x2_scaled, center = TRUE, scale. = TRUE)

# Extract the PCA results
pca_data <- as.data.frame(pca$x[, 1:3])
colnames(pca_data) <- c("PC1", "PC2", "PC3")
pca_data$Type <- y
pca_data$Shape <- shape_column

pca_data2 <- as.data.frame(pca2$x[, 1:3])
colnames(pca_data2) <- c("PC1", "PC2", "PC3")
pca_data2$Type <- y
pca_data2$Shape <- shape_column

# Perform K-means clustering with 2 clusters
set.seed(123) # For reproducibility
kmeans_result <- kmeans(pca_data[, 1:3], centers = 2)
pca_data$Cluster <- as.factor(kmeans_result$cluster)

kmeans_result2 <- kmeans(pca_data2[, 1:3], centers = 2)
pca_data2$Cluster <- as.factor(kmeans_result2$cluster)

# Function to generate ellipsoid data
generate_ellipsoid <- function(center, cov_matrix, n_points = 100) {
  ellipsoid <- ellipse3d(cov_matrix, centre = center, level = 0.95)
  return(ellipsoid)
}

# Plotting the 3D PCA with clusters, shapes, and ellipsoids
plot <- plot_ly()

# Add the data points
plot <- plot %>%
  add_markers(data = pca_data, x = ~PC1, y = ~PC2, z = ~PC3, color = ~Cluster, symbol = ~Shape, text = ~Type, symbols = c('circle', 'square'), colors = 'Set1')

# Add ellipsoids for each cluster
for (cluster_num in levels(pca_data$Cluster)) {
  cluster_data <- pca_data[pca_data$Cluster == cluster_num, 1:3]
  cov_matrix <- cov(cluster_data)
  center <- colMeans(cluster_data)
  
  ellipsoid_data <- generate_ellipsoid(center, cov_matrix)
  
  # Add ellipsoid to the plot
  plot <- plot %>%
    add_mesh(x = ellipsoid_data$vb[1,], y = ellipsoid_data$vb[2,], z = ellipsoid_data$vb[3,],
             opacity = 0.2, color = I(cluster_num))
}


plot <- plot %>%
  layout(scene = list(xaxis = list(title = 'Principal Component 1'),
                      yaxis = list(title = 'Principal Component 2'),
                      zaxis = list(title = 'Principal Component 3')),
         title = '3D PCA of Dog Food Types with 2 Clusters, Shapes, and Ellipsoids')

# Plot the result
plot


#============================== new idea to swap shape and color in the 3D PCA=========== 9-25-2024
#this is the updated version I have made into a figure on 9-25-24

# Load required libraries
library(readxl)
library(ggplot2)
library(ggfortify)
library(plotly)
library(cluster) # For silhouette analysis
library(factoextra) # For visualization

#Load data
data_3or <- ELISA_and_fluorescence_restruc_GB1_norm_kcal_3_super_high_cml_removed

# Remove Fresh foods
data_3or_use <- data_3or[data_3or$Type != 'Fresh', ]

# Add a column for shape based on clusters (this will be updated after K-means clustering)
# Select relevant columns for PCA
features <- c('CML_ug_per_g_food_moist', 'MG_ug_per_g_food_moist', 'LF_AGE_ug_per_g_food_moist', 'SF_ug_per_g_food_moist',
              'CML_ug_per_kcal_food', 'MG_ug_per_kcal_food', 'LF_ug_per_kcal_food', 'SF_ug_per_kcal_food')

x <- data_3or_use[features]
y <- data_3or_use$Type  # Dog food type
dogfood_id <- data_3or_use$ID  # Assuming you have a column for dog food IDs

# Standardize the data
x_scaled <- scale(x)

# Perform PCA to reduce to 3 components
pca <- prcomp(x_scaled, center = TRUE, scale. = TRUE)
summary(pca)

# Extract the PCA results
pca_data <- as.data.frame(pca$x[, 1:5])
colnames(pca_data) <- c("PC1", "PC2", "PC3", "PC4", "PC5")
pca_data$Type <- y
pca_data$DogfoodID <- dogfood_id  # Add dog food ID for hover info
# Add Make and Description columns from the original data
pca_data$Make <- data_3or_use$Make  # Replace with the correct column name for 'Make'
pca_data$Description <- data_3or_use$Description  # Replace with the correct column name for 'Description'

# Elbow Method for optimal clusters
set.seed(123) # For reproducibility
wss <- sapply(1:10, function(k){ 
  kmeans(pca_data[, 1:5], k, nstart = 10)$tot.withinss 
})

elbow_plot <- qplot(1:10, wss, geom = 'line') +
  ggtitle('Elbow Method for Optimal Clusters') +
  xlab('Number of clusters') +
  ylab('Within-cluster sum of squares')
elbow_plot

# Silhouette Method for optimal clusters
silhouette_scores <- sapply(2:10, function(k){
  km <- kmeans(pca_data[, 1:5], centers = k, nstart = 10)
  ss <- silhouette(km$cluster, dist(pca_data[, 1:5]))
  mean(ss[, 3])
})

silhouette_plot <- qplot(2:10, silhouette_scores, geom = 'line') +
  ggtitle('Silhouette Method for Optimal Clusters') +
  xlab('Number of clusters') +
  ylab('Average silhouette width')
silhouette_plot

# Perform K-means clustering
set.seed(123) # For reproducibility
kmeans_result <- kmeans(pca_data[, 1:3], centers = 2)  # You can adjust the number of clusters here
pca_data$Cluster <- as.factor(kmeans_result$cluster)

# Assign shapes based on clusters
pca_data$Shape <- ifelse(pca_data$Cluster == 1, 'circle', 'square')

# Plotting the 3D PCA with clusters as shapes and food type as colors
plot <- plot_ly(pca_data, 
                x = ~PC1, y = ~PC2, z = ~PC3, 
                color = ~Type,  # Use Type for color coding (can still be kept)
                symbol = ~Shape,  # Use Shape (which reflects clusters) for shape coding
                text = ~paste("Make:", Make, "<br>Description:", Description),  # Hover text includes only Make and Description
                symbols = c('circle', 'square'), 
                colors = 'Set1') %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Principal Component 1'),
                      yaxis = list(title = 'Principal Component 2'),
                      zaxis = list(title = 'Principal Component 3')),
         title = '3D PCA of Dog Food Types with Clusters (by shape)')

# Display the plot
plot

# make a list of canned food that clustered with kibbles in blue squares. red squares essentially:

# Filter the data for Cluster 2 and Type 'Canned'
cluster_2_canned <- pca_data[pca_data$Cluster == 2 & pca_data$Type == 'Canned', 
                             c('DogfoodID', 'Make', 'Description')]

# Save the list as a CSV file
write.csv(cluster_2_canned, "Cluster_2_Canned_Foods.csv", row.names = FALSE)

# Message to confirm the file is saved
print("CSV file 'Cluster_2_Canned_Foods.csv' saved successfully!")

# Display the list of canned foods in Cluster 2
print(cluster_2_canned)


#==========================================assigning quartiles================== 10-28-2024
# Load necessary libraries
library(dplyr)
library(tidyr)

# Assuming your data is in a data frame called `data`
# Filter out rows with NA values in the relevant measure columns
measure_columns <- c("CML_ug_per_g_food_moist", "MG_ug_per_g_food_moist", "LF_AGE_ug_per_g_food_moist",
                     "SF_ug_per_g_food_moist", "CML_ug_per_kcal_food", "MG_ug_per_kcal_food",
                     "LF_ug_per_kcal_food", "SF_ug_per_kcal_food")

# Remove rows with missing values in measure columns

data_filtered <- data_3or %>% drop_na(any_of(measure_columns))

# Function to assign quartiles for each measure
assign_quartiles <- function(df, measure) {
  df <- df %>%
    group_by(Type) %>%
    mutate(
      !!paste0(measure, "_quartile") := cut(
        !!sym(measure),
        breaks = quantile(!!sym(measure), probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
        labels = c("Q1", "Q2", "Q3", "Q4"),
        include.lowest = TRUE
      )
    ) %>%
    ungroup()
  return(df)
}

# Apply the function for each measure column and add quartile columns to data frame
for (measure in measure_columns) {
  data_filtered <- assign_quartiles(data_filtered, measure)
}

# View the resulting data frame with quartiles assigned for each measure
write.csv(data_filtered, "quartile_data.csv")

#test a plot Beeswarm to see id's of extremes: 
#======================================
install.packages("ggbeeswarm")
# Load necessary libraries
library(ggplot2)
library(ggbeeswarm)
library(plotly)
library(dplyr)
library(tidyr)

# Define measure columns, split by units
ug_per_g_measures <- c("CML_ug_per_g_food_moist", "MG_ug_per_g_food_moist", "LF_AGE_ug_per_g_food_moist", "SF_ug_per_g_food_moist")
ug_per_kcal_measures <- c("CML_ug_per_kcal_food", "MG_ug_per_kcal_food", "LF_ug_per_kcal_food", "SF_ug_per_kcal_food")

# Helper function to create plot
create_plot <- function(measures, filename) {
  # Prepare data for the selected measures
  data_long <- data_filtered %>%
    pivot_longer(cols = all_of(measures), names_to = "Measure", values_to = "Value") %>%
    group_by(Type, Measure) %>%
    mutate(Quartile = ntile(Value, 5)) %>%
    ungroup() %>%
    mutate(Quartile_Label = paste0("Q", Quartile)) # Create quartile label
  
  # Create ggplot with jitter, transparency, and custom hover details
  p <- ggplot(data_long, aes(x = Type, y = Value, 
                             text = paste("ID:", ID, "<br>",
                                          "Make:", Make, "<br>",
                                          "Description:", Description, "<br>",
                                          "Quartile:", Quartile_Label))) +
    geom_beeswarm(aes(color = Type), size = 2, alpha = 0.7) + # Adds transparency and reduces overlap
    geom_boxplot(width = 0.1, alpha = 0.5) +
    scale_y_log10() +
    facet_wrap(~ Measure, scales = "free_y") +
    labs(
      title = paste("Log-Scaled Quartile Distribution for", filename),
      x = "Dog Food Type",
      y = "Value (Log Scale)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Convert to interactive plot with Plotly, showing only custom hover text
  interactive_plot <- ggplotly(p, tooltip = "text")
  
  # Save the interactive plot as an HTML file
  htmlwidgets::saveWidget(interactive_plot, filename)
}

# Create and save plots for each measure type
create_plot(ug_per_g_measures, "dog_food_quartile_ug_per_g.html")
create_plot(ug_per_kcal_measures, "dog_food_quartile_ug_per_kcal.html")

#=====================quick view for moisture 10-31-24

# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(plotly)
library(htmlwidgets)
library(ggbeeswarm)
library(ggrepel)

# Define measure columns, split by units
ug_per_g_measures_moist <- c("CML_ug_per_g_food_moist_moist", "MG_ug_per_g_food_moist_moist", "LF_AGE_ug_per_g_food_moist_moist", "SF_ug_per_g_food_moist_moist")
ug_per_kcal_measures_moist <- c("CML_ug_per_kcal_food_moist", "MG_ug_per_kcal_food_moist", "LF_ug_per_kcal_food_moist", "SF_ug_per_kcal_food_moist")

# Filter data to remove missing values
data_filtered_moist <- data_moisture %>% drop_na(any_of(ug_per_g_measures_moist))

# Helper function to create plot with tertiles and mean points
create_plot <- function(measures, filename) {
  # Prepare data for the selected measures
  data_long <- data_filtered_moist %>%
    pivot_longer(cols = all_of(measures), names_to = "Measure", values_to = "Value") %>%
    group_by(Type, Measure) %>%
    mutate(Tertile = ntile(Value, 3)) %>%  # Use tertiles instead of quartiles
    ungroup() %>%
    mutate(Tertile_Label = paste0("T", Tertile)) # Create tertile label
  
  # Calculate the mean for each measure and type for plotting
  mean_values <- data_long %>%
    group_by(Type, Measure) %>%
    summarize(Mean_Value = mean(Value, na.rm = TRUE), .groups = 'drop')
  
  # Create ggplot with boxplots, tertile labels, mean point, and jitter
  p <- ggplot(data_long, aes(x = Type, y = Value)) +
    geom_boxplot(width = 0.1, alpha = 0.5) +
    geom_beeswarm(aes(color = Type), size = 2, alpha = 0.7) +
    facet_wrap(~ Measure, scales = "free_y") +
    scale_y_log10() +
    labs(
      title = paste("Log-Scaled Tertile Distribution for", filename),
      x = "Dog Food Type",
      y = "Value (Log Scale)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    # Add mean point and nudge label right and up for each measure and type
    geom_point(data = mean_values, aes(y = Mean_Value), size = 4, color = "red") +
    geom_text(data = mean_values, aes(x = Type, y = Mean_Value, label = round(Mean_Value, 2)),
              color = "red", size = 4, nudge_y = 0.01, nudge_x = 0.12)  # Adjust both y and x positions
  
  # Convert to interactive plot with Plotly, showing only custom hover text
  interactive_plot <- ggplotly(p, tooltip = "text") 
  
  # Save the interactive plot as an HTML file
  saveWidget(interactive_plot, filename)
}

# Create and save plots for each measure type
create_plot(ug_per_g_measures_moist, "dog_food_tertile_ug_per_g_moist.html")
create_plot(ug_per_kcal_measures_moist, "dog_food_tertile_ug_per_kcal_moist.html")

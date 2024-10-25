# (version on September 19 2024)
# we normalized all AGE measures to per kcal of food === only for box plots in fig1.
# 5 kcal values that were missing were added by hand, Dr Turner found them.
# 3 super high CML value containing rows were removed by hand , 
#we will exclude 3 rows for figure 1 boxplots and pairwise.
# we will only NA the 3 high CML guys and keep the rest for the other figures.

library(ggplot2)
library(dplyr)
library(readxl)

# click on the file to import

#file name interpret: missing 5 kcal/kg values added, 3 (eye balled) outliers removed.
#not reasonable to eyeball outliers, need scientific justification.


# I have to work with 2 files in this code
# data_3NA_forCML is the one where 3 high CML values are NA ed
# data_3or is the one where the entire 3 rows are deleted.

data_3or <- ELISA_and_fluorescence_restruc_GB1_norm_kcal_3_super_high_cml_removed
data_3NA_forCML <- ELISA_and_fluorescence_restruc_GB1_norm_kcal_3CML_NAed

str(data_3or)
View(data_3or)

data_3or <- data_3or %>%
  mutate(label = as.character(ID))

data_3NA_forCML<- data_3NA_forCML %>%
  mutate(label = as.character(ID))

# need some modification here: coerce the data type to be numeric.
data_3or <- data_3or %>%
  mutate(CML_ug_per_g_food = as.numeric(CML_ug_per_g_food))

# need some modification here: coerce the data type to be numeric.
data_3NA_forCML <- data_3NA_forCML %>%
  mutate(CML_ug_per_g_food = as.numeric(CML_ug_per_g_food))

# need some modification here: coerce the data type to be numeric.
data_3NA_forCML <- data_3NA_forCML %>%
  mutate(`kcal/kg` = as.numeric(`kcal/kg`)) %>%
  rename(kcal_per_kg = `kcal/kg`)

# need some modification here: coerce the data type to be numeric.
data_3or <- data_3or %>%
  mutate(`kcal/kg` = as.numeric(`kcal/kg`)) %>%
  rename(kcal_per_kg = `kcal/kg`)

#bring in my whole chunk of mutate bundle

data_3NA_forCML <- data_3NA_forCML %>%
  mutate(Total_AGE_Score_g = CML_ug_per_g_food + MG_ug_per_g_food + LF_AGE_ug_per_g_food + SF_ug_per_g_food) %>%
  mutate(Total_AGE_Score_kcal = CML_ug_per_kcal_food + MG_ug_per_kcal_food + LF_ug_per_kcal_food + SF_ug_per_kcal_food) %>%
  mutate(CML_plus_MG_g = CML_ug_per_g_food + MG_ug_per_g_food) %>%
  mutate(CML_plus_MG_kcal = CML_ug_per_kcal_food + MG_ug_per_kcal_food) %>%
  mutate(Combined_fluo_g= LF_AGE_ug_per_g_food + SF_ug_per_g_food) %>%
  mutate(Combined_fluo_kcal= LF_ug_per_kcal_food + SF_ug_per_kcal_food)


data_3or <- data_3or %>%
  mutate(Total_AGE_Score_g = CML_ug_per_g_food + MG_ug_per_g_food + LF_AGE_ug_per_g_food + SF_ug_per_g_food) %>%
  mutate(Total_AGE_Score_kcal = CML_ug_per_kcal_food + MG_ug_per_kcal_food + LF_ug_per_kcal_food + SF_ug_per_kcal_food) %>%
  mutate(CML_plus_MG_g = CML_ug_per_g_food + MG_ug_per_g_food) %>%
  mutate(CML_plus_MG_kcal = CML_ug_per_kcal_food + MG_ug_per_kcal_food) %>%
  mutate(Combined_fluo_g= LF_AGE_ug_per_g_food + SF_ug_per_g_food) %>%
  mutate(Combined_fluo_kcal= LF_ug_per_kcal_food + SF_ug_per_kcal_food)


View(data_3NA_forCML)
View(data_3or)

colnames(data_3or)

# 1a extract data for prism CML canned
canned <- data_3or %>%
  filter(Type == "Canned") %>%
  dplyr::select(CML_ug_per_kcal_food)
View (canned)

# 2a extract data for prism CML kibble
kibble <- data_3or %>%
  filter(Type== "Kibble") %>%
  dplyr::select(CML_ug_per_kcal_food)
View (kibble)

# 3a extract data for prism MG canned
canned_mg <- data_3or %>%
  filter(Type== "Canned") %>%
  dplyr::select(MG_ug_per_kcal_food)
View (canned_mg)

# 4a extract data for prism MG kibble
kibble_mg <- data_3or %>%
  filter(Type== "Kibble") %>%
  dplyr::select(MG_ug_per_kcal_food)
View (kibble_mg)

# 5a extract data for prism CML plus MG canned (this is the kcal version)
canned_cml_plus_mg <- data_3or %>%
  filter(Type== "Canned") %>%
  dplyr::select(CML_plus_MG_kcal)
View (canned_cml_plus_mg)

# 6a extract data for prism CML plus MG kibble 
kibble_cml_plus_mg <- data_3or %>%
  filter(Type== "Kibble") %>%
  dplyr::select(CML_plus_MG_kcal)
View (kibble_cml_plus_mg)

# 7a extract data for prism SF canned
canned_sf <- data_3or %>%
  filter(Type== "Canned") %>%
  dplyr::select(SF_ug_per_kcal_food)
View (canned_sf)

# 8a extract data for prism SF kibble
kibble_sf <- data_3or %>%
  filter(Type== "Kibble") %>%
  dplyr::select(SF_ug_per_kcal_food)
View (kibble_sf)

# 9a extract data for prism LF canned
canned_lf <- data_3or %>%
  filter(Type== "Canned") %>%
  dplyr::select(LF_ug_per_kcal_food)
View (canned_lf)

# 10a extract data for prism LF kibble
kibble_lf <- data_3or %>%
  filter(Type== "Kibble") %>%
  dplyr::select(LF_ug_per_kcal_food)
View (kibble_lf)

#Now start making same plots for ug per gram food
### Part 2 of fig 2==============================
#add a new column that calculates CML+ MG using ug per gram food

# 1 extract data for prism CML canned in ug per gram
canned <- data_3or %>%
  filter(Type == "Canned") %>%
  dplyr::select(CML_ug_per_g_food)
View (canned)

# 2 extract data for prism CML kibble
kibble <- data_3or %>%
  filter(Type== "Kibble") %>%
  dplyr::select(CML_ug_per_g_food)
View (kibble)

# 3 extract data for prism MG canned
canned_mg <- data_3or %>%
  filter(Type== "Canned") %>%
  dplyr::select(MG_ug_per_g_food)
View (canned_mg)

# 4 extract data for prism MG kibble
kibble_mg <- data_3or %>%
  filter(Type== "Kibble") %>%
  dplyr::select(MG_ug_per_g_food)
View (kibble_mg)

# 5 extract data for prism CML plus MG canned (this is the ug per g version)
canned_cml_plus_mg <- data_3or %>%
  filter(Type== "Canned") %>%
  dplyr::select(CML_plus_MG_g)
View (canned_cml_plus_mg)

# 6 extract data for prism CML plus MG kibble 
kibble_cml_plus_mg <- data_3or %>%
  filter(Type== "Kibble") %>%
  dplyr::select(CML_plus_MG_g)
View (kibble_cml_plus_mg)

# 7 extract data for prism SF canned
canned_sf <- data_3or %>%
  filter(Type== "Canned") %>%
  dplyr::select(SF_ug_per_g_food)
View (canned_sf)

# 8 extract data for prism SF kibble
kibble_sf <- data_3or %>%
  filter(Type== "Kibble") %>%
  dplyr::select(SF_ug_per_g_food)
View (kibble_sf)

# 9 extract data for prism LF canned
canned_lf <- data_3or %>%
  filter(Type== "Canned") %>%
  dplyr::select(LF_AGE_ug_per_g_food)
View (canned_lf)

# 10 extract data for prism LF kibble
kibble_lf <- data_3or %>%
  filter(Type== "Kibble") %>%
  dplyr::select(LF_AGE_ug_per_g_food)
View (kibble_lf)

# 11 extract data for prism total age score canned
canned_total <- data_3or %>%
  filter(Type== "Canned") %>%
  dplyr::select(Total_AGE_Score_g)
View (canned_total)

# 12 extract data for prism total kibble
kibble_total <- data_3or %>%
  filter(Type== "Kibble") %>%
  dplyr::select(Total_AGE_Score_g)
View (kibble_total)

# 13 extract data for prism total age score canned kcal
canned_total <- data_3or %>%
  filter(Type== "Canned") %>%
  dplyr::select(Total_AGE_Score_kcal)
View (canned_total)

# 14 extract data for prism total kibble kcal
kibble_total <- data_3or %>%
  filter(Type== "Kibble") %>%
  dplyr::select(Total_AGE_Score_kcal)
View (kibble_total)

#now re make all the plots for Fig 3, make R remember the function.

#need function here:
#September 19 2024 update, make pearson --> spearman
# Define the function get help from chat gpt # this is like "fat option 3" (puts labels to the right)
scatter_plot_by_type_label_right <- function(data, x, y, z = NULL) {
  
  # Ensure the arguments x, y, and z are characters
  x <- as.character(substitute(x))
  y <- as.character(substitute(y))
  
  # If z is specified, filter the data frame based on the z type
  if (!is.null(z)) {
    z <- as.character(substitute(z))
    filtered_data <- data[data$Type == z, ]
  } else {
    filtered_data <- data
  }
  
  #adding the number as n= something onto the plot
  n_count <- nrow(filtered_data)
  # Calculate the correlation coefficient only for the type of food selected
  # Calculate the Spearman correlation coefficient
  cor_test <- cor.test(filtered_data[[x]], filtered_data[[y]], method = "spearman")
  correlation <- cor_test$estimate
  p_value <- cor_test$p.value
  
  # Create the scatter plot
  p <- ggplot(filtered_data, aes_string(x = x, y = y)) +
    geom_point(size = 3) +
    labs(title = if (!is.null(z)) {
      paste("Scatter Plot of", x, "vs", y, "for type", z)
    } else {
      paste("Scatter Plot of", x, "vs", y, "for all food types")
    },
    x = x,
    y = y) +geom_smooth(method = "lm", col = "red", lty = 2) +
    labs(subtitle = paste("Number of Dog Foods Tested: n =", n_count))+
    annotate("text", x = Inf, y = Inf,  # Adjust x and y to position the text to the left
             label = paste0("\nSpearman r =", round(correlation, 2), 
                            "\np-value =", signif(p_value, 3)), 
             hjust = 1.1, vjust = 0.8, size = 10, color = "blue") +
    theme(axis.title.x = element_text(size = 22, face = "bold"),  # Bold and adjust font size of x-axis label
          axis.title.y = element_text(size = 22, face = "bold"),  # Bold and adjust font size of y-axis label
          axis.text.x = element_text(size = 16),   # Bold and adjust font size of x-axis tick labels
          axis.text.y = element_text(size = 16))   # Bold and adjust font size of y-axis tick labels
  
  # Print the plot
  print(p)
}

#need function here:
# Define the function (puts labels to the left) get help from chat gpt # this is like "fat option 3"
scatter_plot_by_type_label_left <- function(data, x, y, z = NULL) {
  
  # Ensure the arguments x, y, and z are characters
  x <- as.character(substitute(x))
  y <- as.character(substitute(y))
  
  # If z is specified, filter the data frame based on the z type
  if (!is.null(z)) {
    z <- as.character(substitute(z))
    filtered_data <- data[data$Type == z, ]
  } else {
    filtered_data <- data
  }
  
  #adding the number as n= something onto the plot
  n_count <- nrow(filtered_data)
  # Calculate the correlation coefficient only for the type of food selected
  # Calculate the Pearson correlation coefficient
  cor_test <- cor.test(filtered_data[[x]], filtered_data[[y]], method = "spearman")
  correlation <- cor_test$estimate
  p_value <- cor_test$p.value
  
  # Create the scatter plot
  p <- ggplot(filtered_data, aes_string(x = x, y = y)) +
    geom_point(size = 3) +
    labs(title = if (!is.null(z)) {
      paste("Scatter Plot of", x, "vs", y, "for type", z)
    } else {
      paste("Scatter Plot of", x, "vs", y, "for all food types")
    },
    x = x,
    y = y) +geom_smooth(method = "lm", col = "red", lty = 2) +
    labs(subtitle = paste("Number of Dog Foods Tested: n =", n_count))+
    annotate("text", x = -Inf, y = Inf,  # Adjust x and y to position the text to the left
             label = paste0("\nSpearman r =", round(correlation, 2), 
                            "\np-value =", signif(p_value, 3)), 
             hjust = -0.1, vjust = 0.8, size = 10, color = "blue") +
    theme(axis.title.x = element_text(size = 22, face = "bold"),  # Bold and adjust font size of x-axis label
          axis.title.y = element_text(size = 22, face = "bold"),  # Bold and adjust font size of y-axis label
          axis.text.x = element_text(size = 16),   # Bold and adjust font size of x-axis tick labels
          axis.text.y = element_text(size = 16))   # Bold and adjust font size of y-axis tick labels
  
  # Print the plot
  print(p)
}


#make a new scatter plot to see AGE measure vs ingredients to find which ones are p value signif

# here we no longer need to use the kcal normalization
#also now we switch to using the data_3NA_forCML file which has the 3 high CML values only NAed 
# while keeping other AGE measures of the same dogfoods available.
# CML
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_max_Crude_fat", "CML_ug_per_g_food","Canned") #0.064
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_max_Crude_protein" ,"CML_ug_per_g_food", "Canned") # 0.39
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_Crude_fiber" ,"CML_ug_per_g_food", "Canned") #0.44
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_moisture" ,"CML_ug_per_g_food", "Canned") #0.517

scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_max_Crude_fat", "CML_ug_per_g_food","Kibble")  #0.498
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_max_Crude_protein" ,"CML_ug_per_g_food", "Kibble")  #0.524
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_Crude_fiber" ,"CML_ug_per_g_food", "Kibble")  #0.854
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_moisture" ,"CML_ug_per_g_food", "Kibble") #signif #0.00303

#MG
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_max_Crude_fat", "MG_ug_per_g_food","Canned") #0.0661
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_max_Crude_protein" ,"MG_ug_per_g_food", "Canned") # very signif ******
scatter_plot_by_type_label_right(data_3NA_forCML,"Percent_Crude_fiber" ,"MG_ug_per_g_food", "Canned") # 0.000843*********
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_moisture" ,"MG_ug_per_g_food", "Canned") # 0.0435 *********

scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_max_Crude_fat", "MG_ug_per_g_food","Kibble")  #Not signif 0.558
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_max_Crude_protein" ,"MG_ug_per_g_food", "Kibble")  #Not signif 0.494
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_Crude_fiber" ,"MG_ug_per_g_food", "Kibble")  #Not signif 0.886
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_moisture" ,"MG_ug_per_g_food", "Kibble")  #Not signif 0.737

#CML plus MG
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_max_Crude_fat", "CML_plus_MG_g","Canned")  # signif 0.0484
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_max_Crude_protein" ,"CML_plus_MG_g", "Canned") # not signif 0.414
scatter_plot_by_type_label_right(data_3NA_forCML,"Percent_Crude_fiber" ,"CML_plus_MG_g", "Canned")  ## ** 0.0325
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_moisture" ,"CML_plus_MG_g", "Canned") ## 0.15 not signif

scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_max_Crude_fat", "CML_plus_MG_g","Kibble")  #Not signif 0.378
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_max_Crude_protein" ,"CML_plus_MG_g", "Kibble")  #Not signif 0.289
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_Crude_fiber" ,"CML_plus_MG_g", "Kibble") #Not signif 0.798
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_moisture" ,"CML_plus_MG_g", "Kibble") # signif 0.0433


#SF
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_max_Crude_fat", "SF_ug_per_g_food","Canned")  ## 0.0517
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_max_Crude_protein" ,"SF_ug_per_g_food", "Canned") #Not signif 0.143
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_Crude_fiber" ,"SF_ug_per_g_food", "Canned") #Not signif 0.232
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_moisture" ,"SF_ug_per_g_food", "Canned") #Not signif 0.315
scatter_plot_by_type_label_left(data_3NA_forCML,"kcal_per_kg" ,"SF_ug_per_g_food", "Canned") # signif 0.0125

scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_max_Crude_fat", "SF_ug_per_g_food","Kibble") #Not signif 0.531
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_max_Crude_protein" ,"SF_ug_per_g_food", "Kibble")  #Not signif 0.153
scatter_plot_by_type_label_right(data_3NA_forCML,"Percent_Crude_fiber" ,"SF_ug_per_g_food", "Kibble") #Not signif 0.656
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_moisture" ,"SF_ug_per_g_food", "Kibble") #Not signif 0.832

#LF plots
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_max_Crude_fat", "LF_AGE_ug_per_g_food","Canned")   # ********* 0.00436
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_max_Crude_protein" ,"LF_AGE_ug_per_g_food", "Canned")  # ********* 0.000131
scatter_plot_by_type_label_right(data_3NA_forCML,"Percent_Crude_fiber" ,"LF_AGE_ug_per_g_food", "Canned") # ********* 0.00247
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_moisture" ,"LF_AGE_ug_per_g_food", "Canned")  # ********* 0.00132

scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_max_Crude_fat", "LF_AGE_ug_per_g_food","Kibble") # not signif 0.47
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_max_Crude_protein" ,"LF_AGE_ug_per_g_food", "Kibble") # signif 0.0305
scatter_plot_by_type_label_right(data_3NA_forCML,"Percent_Crude_fiber" ,"LF_AGE_ug_per_g_food", "Kibble") #not signif 0.716
scatter_plot_by_type_label_right(data_3NA_forCML,"Percent_moisture" ,"LF_AGE_ug_per_g_food", "Kibble") # not signif 0.0805




scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_moisture" ,"Combined_fluo_g", "Canned") # not signif 0.0738
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_max_Crude_fat", "Combined_fluo_g","Canned") # **** 0.000601
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_max_Crude_protein" ,"Combined_fluo_g", "Canned") # ***** 0.0011
scatter_plot_by_type_label_right(data_3NA_forCML,"Percent_Crude_fiber" ,"Combined_fluo_g", "Canned") # ***** 0.0057

scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_moisture" ,"Combined_fluo_g", "Kibble") # not signif 0.173
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_max_Crude_fat", "Combined_fluo_g","Kibble") #not signif 0.0366
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_max_Crude_protein" ,"Combined_fluo_g", "Kibble") # signif 0.0153
scatter_plot_by_type_label_right(data_3NA_forCML,"Percent_Crude_fiber" ,"Combined_fluo_g", "Kibble") #not signif 0.779

#total AGE score

scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_moisture" ,"Total_AGE_Score_g", "Canned") # not signif 0.0511
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_max_Crude_fat", "Total_AGE_Score_g","Canned") # **** 0.00134
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_max_Crude_protein" ,"Total_AGE_Score_g", "Canned") # ***** 0.00274
scatter_plot_by_type_label_right(data_3NA_forCML,"Percent_Crude_fiber" ,"Total_AGE_Score_g", "Canned") # ***** 0.00383

scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_moisture" ,"Total_AGE_Score_g", "Kibble") # not signif 0.115
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_max_Crude_fat", "Total_AGE_Score_g","Kibble") #not signif 0.362
scatter_plot_by_type_label_left(data_3NA_forCML,"Percent_max_Crude_protein" ,"Total_AGE_Score_g", "Kibble") # signif 0.0228
scatter_plot_by_type_label_right(data_3NA_forCML,"Percent_Crude_fiber" ,"Total_AGE_Score_g", "Kibble") #not signif 0.545


#Figure 2 related plots
scatter_plot_by_type_label_left(data_3or,"CML_ug_per_g_food", "MG_ug_per_g_food","Canned") #0.146
scatter_plot_by_type_label_right(data_3or,"CML_ug_per_g_food", "MG_ug_per_g_food","Kibble") # 0.0288

#responding to Vick's question about should we remove 3 CML high values
#these could include the fresh

scatter_plot_by_type_label_right(data_3or,"CML_ug_per_g_food", "MG_ug_per_g_food")
scatter_plot_by_type_label_right(data_3or,"CML_ug_per_g_food", "LF_AGE_ug_per_g_food")
scatter_plot_by_type_label_right(data_3or,"CML_ug_per_g_food", "SF_ug_per_g_food")

#just check how it looks when Fresh is excluded
data_3or_no_Fresh <- data_3or %>%
  filter (data_3or$Type != "Fresh")

scatter_plot_by_type_label_right(data_3or_no_Fresh,"CML_ug_per_g_food", "MG_ug_per_g_food")
scatter_plot_by_type_label_right(data_3or_no_Fresh,"CML_ug_per_g_food", "LF_AGE_ug_per_g_food")
scatter_plot_by_type_label_right(data_3or_no_Fresh,"CML_ug_per_g_food", "SF_ug_per_g_food")


#Figure 2 bottom plots, (made a different colored version below but this is preferred)
scatter_plot_by_type_label_right(data_3or,"CML_ug_per_g_food", "MG_ug_per_g_food","Canned") #not signif 0.146
scatter_plot_by_type_label_right(data_3or,"CML_ug_per_g_food", "MG_ug_per_g_food","Kibble") #signif 0.0288
scatter_plot_by_type_label_right(data_3or,"CML_ug_per_g_food", "SF_ug_per_g_food","Canned") # 0.000204
scatter_plot_by_type_label_right(data_3or,"CML_ug_per_g_food", "SF_ug_per_g_food","Kibble") #not signif 0.876
scatter_plot_by_type_label_left(data_3or,"CML_plus_MG_g", "SF_ug_per_g_food","Canned") # very signif
scatter_plot_by_type_label_right(data_3or,"CML_plus_MG_g", "SF_ug_per_g_food","Kibble") #0.00965

scatter_plot_by_type_label_left(data_3or,"MG_ug_per_g_food", "SF_ug_per_g_food","Canned") # 0.000122
scatter_plot_by_type_label_right(data_3or,"MG_ug_per_g_food", "SF_ug_per_g_food","Kibble") # 0.00109




#to label points if needed.
#c + geom_text(aes(label = label), nudge_x = 0.3, nudge_y = 0.3, check_overlap = FALSE) 
#k + geom_text(aes(label = label), nudge_x = 0.3, nudge_y = 0.3, check_overlap = FALSE)

#ask chat GPT what other analysis I can do======================================

library(ggplot2)
library(dplyr)
library(tidyr)

# Exclude the dog foods with the type "fresh"
data_3or <- data_3or %>%
  filter(Type != "Fresh")

# Convert to long format for plotting
long_data <- data_3or %>%
  select(Type, CML_ug_per_g_food, MG_ug_per_g_food, LF_AGE_ug_per_g_food, SF_ug_per_g_food) %>%
  gather(key = "Measure", value = "Value", CML_ug_per_g_food:SF_ug_per_g_food)

# Create the box plot
p <- ggplot(long_data, aes(x = Type, y = Value, fill = Type)) +
  geom_boxplot() +
  facet_wrap(~ Measure, scales = "free_y") +
  labs(title = "Distribution of AGE Measures by Dog Food Type",
       x = "Dog Food Type",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p

# Save the plot as a PNG file
png(filename = "boxplot_AGE_measures_by_type.png", width = 1200, height = 800)
print(p)
dev.off()

##================================================================================================
#summary statistics
library(tidyverse)
# Calculate summary statistics for each measure by type
summary_stats <- data_3or %>%
  select(Type, CML_ug_per_g_food, MG_ug_per_g_food, LF_AGE_ug_per_g_food, SF_ug_per_g_food, CML_ug_per_kcal_food, 
         MG_ug_per_kcal_food, LF_ug_per_kcal_food, SF_ug_per_kcal_food, CML_plus_MG_g, CML_plus_MG_kcal, 
         Combined_fluo_g, Combined_fluo_kcal, Total_AGE_Score_g, Total_AGE_Score_kcal) %>%
  group_by(Type) %>%
  summarise(across(everything(), list(min = ~min(., na.rm = TRUE),
                                      max = ~max(., na.rm = TRUE),
                                      mean = ~mean(., na.rm = TRUE), 
                                      median = ~median(., na.rm = TRUE), 
                                      sd = ~sd(., na.rm = TRUE))))

# Save the summary statistics as a CSV file
write.csv(summary_stats, 'summary_statistics_by_type10-21-24.csv', row.names = FALSE)
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
  mutate(Total_AGE_Score_g = CML_ug_per_g_food + MG_ug_per_g_food + LF_AGE_ug_per_g_food + SF_ug_per_g_food) %>%
  mutate(Total_AGE_Score_kcal = CML_ug_per_kcal_food + MG_ug_per_kcal_food + LF_ug_per_kcal_food + SF_ug_per_kcal_food) %>%
  mutate(CML_plus_MG_g = CML_ug_per_g_food + MG_ug_per_g_food) %>%
  mutate(CML_plus_MG_kcal = CML_ug_per_kcal_food + MG_ug_per_kcal_food) %>%
  mutate(Combined_fluo_g= LF_AGE_ug_per_g_food + SF_ug_per_g_food) %>%
  mutate(Combined_fluo_kcal= LF_ug_per_kcal_food + SF_ug_per_kcal_food)

# ======= Figure 5C Order the dog foods by increasing total AGE score in ug per gram
data <- data %>%
  arrange(Total_AGE_Score_g)

# Ensure the factor levels of DogFoodLabel are in the correct order
#data$DogFoodLabel <- factor(data$DogFoodLabel, levels = data$DogFoodLabel) #old version with food names.
data$DogFoodLabel2 <- factor(data$DogFoodLabel2, levels = data$DogFoodLabel2)

# Create a long format for the AGE measures (has dogfood names)
# data_long <- data %>%
 # select(DogFoodLabel, CML_ug_per_g_food, MG_ug_per_g_food, LF_AGE_ug_per_g_food, SF_ug_per_g_food) %>%
 # pivot_longer(cols = c(CML_ug_per_g_food, MG_ug_per_g_food, LF_AGE_ug_per_g_food, SF_ug_per_g_food),
      #         names_to = "AGE_Measure",
        #       values_to = "Value")

# Create a long format for the AGE measures (no dogfood names)
data_long <- data %>%
  select(DogFoodLabel2, CML_ug_per_g_food, MG_ug_per_g_food, LF_AGE_ug_per_g_food, SF_ug_per_g_food) %>%
  pivot_longer(cols = c(CML_ug_per_g_food, MG_ug_per_g_food, LF_AGE_ug_per_g_food, SF_ug_per_g_food),
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
  scale_fill_manual(values = c("CML_ug_per_g_food" = "red",
                               "MG_ug_per_g_food" = "blue",
                               "LF_AGE_ug_per_g_food" = "darkgreen",
                               "SF_ug_per_g_food" = "darkgray"))
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
  mutate(Total_AGE_Score_g = CML_ug_per_g_food + MG_ug_per_g_food + LF_AGE_ug_per_g_food + SF_ug_per_g_food) %>%
  mutate(Total_AGE_Score_kcal = CML_ug_per_kcal_food + MG_ug_per_kcal_food + LF_ug_per_kcal_food + SF_ug_per_kcal_food) %>%
  mutate(CML_plus_MG_g = CML_ug_per_g_food + MG_ug_per_g_food) %>%
  mutate(CML_plus_MG_kcal = CML_ug_per_kcal_food + MG_ug_per_kcal_food) %>%
  mutate(Combined_fluo_g= LF_AGE_ug_per_g_food + SF_ug_per_g_food) %>%
  mutate(Combined_fluo_kcal= LF_ug_per_kcal_food + SF_ug_per_kcal_food)

# Order the dog foods by increasing total AGE score in ug per gram
data <- data %>%
  arrange(Total_AGE_Score_g)

# Ensure the factor levels of DogFoodLabel are in the correct order
data$DogFoodLabel <- factor(data$DogFoodLabel, levels = data$DogFoodLabel)

# Create a long format for the AGE measures
data_long <- data %>%
  select(DogFoodLabel, CML_ug_per_g_food, MG_ug_per_g_food, LF_AGE_ug_per_g_food, SF_ug_per_g_food) %>%
  pivot_longer(cols = c(CML_ug_per_g_food, MG_ug_per_g_food, LF_AGE_ug_per_g_food, SF_ug_per_g_food),
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
  scale_fill_manual(values = c("CML_ug_per_g_food" = "red",
                               "MG_ug_per_g_food" = "blue",
                               "LF_AGE_ug_per_g_food" = "darkgreen",
                               "SF_ug_per_g_food" = "darkgray"))
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

##=========================================== 9-11-24 version================
## this is still the stacked plot, but making it interactive high resolution and color coded.
# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)

# Load data
data <- ELISA_and_fluorescence_restruc_GB1_norm_kcal_3_super_high_cml_removed

# Exclude the dog foods with the type "fresh"
data <- data %>%
  filter(Type != "Fresh")

# Concatenate Make, Description, Type, and ID for better labeling
data <- data %>%
  mutate(DogFoodLabelFull = paste(Make, Description,ID, sep = " - "))

# Big mutate block to add necessary calculations as new columns
data <- data %>%
  mutate(Total_AGE_Score_g = CML_ug_per_g_food + MG_ug_per_g_food + LF_AGE_ug_per_g_food + SF_ug_per_g_food) %>%
  mutate(Total_AGE_Score_kcal = CML_ug_per_kcal_food + MG_ug_per_kcal_food + LF_ug_per_kcal_food + SF_ug_per_kcal_food) %>%
  mutate(CML_plus_MG_g = CML_ug_per_g_food + MG_ug_per_g_food) %>%
  mutate(CML_plus_MG_kcal = CML_ug_per_kcal_food + MG_ug_per_kcal_food) %>%
  mutate(Combined_fluo_g= LF_AGE_ug_per_g_food + SF_ug_per_g_food) %>%
  mutate(Combined_fluo_kcal= LF_ug_per_kcal_food + SF_ug_per_kcal_food)

# Order the dog foods by increasing total AGE score in ug per gram
data <- data %>%
  arrange(Total_AGE_Score_g)

# Ensure the factor levels of DogFoodLabelFull are in the correct order
data$DogFoodLabelFull <- factor(data$DogFoodLabelFull, levels = data$DogFoodLabelFull)

# Create a long format for the AGE measures
data_long <- data %>%
  select(DogFoodLabelFull, Type, CML_ug_per_g_food, MG_ug_per_g_food, LF_AGE_ug_per_g_food, SF_ug_per_g_food) %>%
  pivot_longer(cols = c(CML_ug_per_g_food, MG_ug_per_g_food, LF_AGE_ug_per_g_food, SF_ug_per_g_food),
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
  scale_fill_manual(values = c("CML_ug_per_g_food" = "red",
                               "MG_ug_per_g_food" = "blue",
                               "LF_AGE_ug_per_g_food" = "darkgreen",
                               "SF_ug_per_g_food" = "darkgray"))

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

# Load data
data <- ELISA_and_fluorescence_restruc_GB1_norm_kcal_3_super_high_cml_removed

# Exclude the dog foods with the type "fresh"
data <- data %>%
  filter(Type != "Fresh")

# Concatenate Make, Description, Type, and ID for better labeling
data <- data %>%
  mutate(DogFoodLabelFull = paste(Make, Description,ID, sep = " - "))

# Big mutate block to add necessary calculations as new columns
data <- data %>%
  mutate(Total_AGE_Score_g = CML_ug_per_g_food + MG_ug_per_g_food + LF_AGE_ug_per_g_food + SF_ug_per_g_food) %>%
  mutate(Total_AGE_Score_kcal = CML_ug_per_kcal_food + MG_ug_per_kcal_food + LF_ug_per_kcal_food + SF_ug_per_kcal_food) %>%
  mutate(CML_plus_MG_g = CML_ug_per_g_food + MG_ug_per_g_food) %>%
  mutate(CML_plus_MG_kcal = CML_ug_per_kcal_food + MG_ug_per_kcal_food) %>%
  mutate(Combined_fluo_g= LF_AGE_ug_per_g_food + SF_ug_per_g_food) %>%
  mutate(Combined_fluo_kcal= LF_ug_per_kcal_food + SF_ug_per_kcal_food)

# Create function to generate a horizontal stacked bar plot and save it
generate_plot <- function(filtered_data, file_name) {
  
  # Order the dog foods by increasing total AGE score in ug per gram
  filtered_data <- filtered_data %>%
    arrange(Total_AGE_Score_g)
  
  # Ensure the factor levels of DogFoodLabelFull are in the correct order
  filtered_data$DogFoodLabelFull <- factor(filtered_data$DogFoodLabelFull, levels = filtered_data$DogFoodLabelFull)
  
  # Create a long format for the AGE measures
  data_long <- filtered_data %>%
    select(DogFoodLabelFull, Type, CML_ug_per_g_food, MG_ug_per_g_food, LF_AGE_ug_per_g_food, SF_ug_per_g_food) %>%
    pivot_longer(cols = c(CML_ug_per_g_food, MG_ug_per_g_food, LF_AGE_ug_per_g_food, SF_ug_per_g_food),
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
    scale_fill_manual(values = c("CML_ug_per_g_food" = "red",
                                 "MG_ug_per_g_food" = "blue",
                                 "LF_AGE_ug_per_g_food" = "darkgreen",
                                 "SF_ug_per_g_food" = "darkgray")) 
  
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

#====================== 8-26-24 Vick's question can we plot only CML as ranked bars: for Fig 5A (cml in ug per g) and B (cml kcal)
#Version 1 for 8-26-24 is ug per gram food

# Concatenate Make, Description, and ID for better labeling
data <-  ELISA_and_fluorescence_restruc_GB1_norm_kcal_3_super_high_cml_removed

# Exclude the dog foods with the type "fresh"
data <- data %>%
  filter(Type != "Fresh")

data <- data %>%
  mutate(Total_AGE_Score_g = CML_ug_per_g_food + MG_ug_per_g_food + LF_AGE_ug_per_g_food + SF_ug_per_g_food) %>%
  mutate(Total_AGE_Score_kcal = CML_ug_per_kcal_food + MG_ug_per_kcal_food + LF_ug_per_kcal_food + SF_ug_per_kcal_food) %>%
  mutate(CML_plus_MG_g = CML_ug_per_g_food + MG_ug_per_g_food) %>%
  mutate(CML_plus_MG_kcal = CML_ug_per_kcal_food + MG_ug_per_kcal_food) %>%
  mutate(Combined_fluo_g= LF_AGE_ug_per_g_food + SF_ug_per_g_food) %>%
  mutate(Combined_fluo_kcal= LF_ug_per_kcal_food + SF_ug_per_kcal_food)

data <- data %>%
  mutate(DogFoodLabel2 = ID)

# Order the dog foods by increasing CML
data <- data %>%
  arrange(CML_ug_per_g_food)

# Ensure the factor levels of DogFoodLabel are in the correct order
data$DogFoodLabel2 <- factor(data$DogFoodLabel2, levels = data$DogFoodLabel2)

# Create a long format for the AGE measures
data_long <- data %>%
  select(DogFoodLabel2, CML_ug_per_g_food) %>%
  pivot_longer(cols = c(CML_ug_per_g_food),
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
  scale_fill_manual(values = c("CML_ug_per_g_food" = "red"))
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
age_columns <- c('CML_ug_per_g_food', 'MG_ug_per_g_food', 'LF_AGE_ug_per_g_food', 'SF_ug_per_g_food','Type')

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
features <- c('CML_ug_per_g_food', 'MG_ug_per_g_food', 'LF_AGE_ug_per_g_food', 'SF_ug_per_g_food', 'CML_ug_per_kcal_food', 'MG_ug_per_kcal_food', 'LF_ug_per_kcal_food', 'SF_ug_per_kcal_food')

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
features <- c('CML_ug_per_g_food', 'MG_ug_per_g_food', 'LF_AGE_ug_per_g_food', 'SF_ug_per_g_food')
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
features <- c('CML_ug_per_g_food', 'MG_ug_per_g_food', 'LF_AGE_ug_per_g_food', 'SF_ug_per_g_food')
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
features <- c('CML_ug_per_g_food', 'MG_ug_per_g_food', 'LF_AGE_ug_per_g_food', 'SF_ug_per_g_food',
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



#====================================================================================
# how about we make a ginormous table of all correlations

# Load required libraries
library(readxl)
library(dplyr)

# Load the dataset
data <- ELISA_and_fluorescence_restruc_GB1_norm_kcal_3_super_high_cml_removed

# Remove Fresh foods
data <- data[data$Type != 'Fresh', ]

data <- data %>%
     mutate(Total_AGE_Score_g = CML_ug_per_g_food + MG_ug_per_g_food + LF_AGE_ug_per_g_food + SF_ug_per_g_food) %>%
     mutate(Total_AGE_Score_kcal = CML_ug_per_kcal_food + MG_ug_per_kcal_food + LF_ug_per_kcal_food + SF_ug_per_kcal_food) %>%
     mutate(CML_plus_MG_g = CML_ug_per_g_food + MG_ug_per_g_food) %>%
     mutate(CML_plus_MG_kcal = CML_ug_per_kcal_food + MG_ug_per_kcal_food) %>%
     mutate(Combined_fluo_g= LF_AGE_ug_per_g_food + SF_ug_per_g_food) %>%
     mutate(Combined_fluo_kcal= LF_ug_per_kcal_food + SF_ug_per_kcal_food)

# Ensure the relevant columns are numeric
columns_to_convert <- c('CML_ug_per_g_food', 'MG_ug_per_g_food', 'SF_ug_per_g_food', 'LF_AGE_ug_per_g_food', 
                        'CML_ug_per_kcal_food', 'MG_ug_per_kcal_food', 'SF_ug_per_kcal_food', 'LF_ug_per_kcal_food', 
                        'CML_plus_MG_g', 'CML_plus_MG_kcal', 'Combined_fluo_g', 'Combined_fluo_kcal', 
                        'Total_AGE_Score_g', 'Total_AGE_Score_kcal', 'Percent_moisture', 
                        'Percent_max_Crude_protein', 'Percent_max_Crude_fat', 'Percent_Crude_fiber', 'kcal/kg')

data[columns_to_convert] <- lapply(data[columns_to_convert], function(x) as.numeric(as.character(x)))

# Remove rows with any NA values in the relevant columns
data <- data %>% drop_na(all_of(columns_to_convert))

# Define the function to compute correlations and p-values
compute_correlations <- function(data, x, y) {
  data_x <- data[[x]]
  data_y <- data[[y]]
  
  print(paste("Variable1:", x, "Type:", class(data_x)))
  print(paste("Variable2:", y, "Type:", class(data_y)))
  
  # Filter out NA values
  valid_indices <- !is.na(data_x) & !is.na(data_y)
  data_x <- data_x[valid_indices]
  data_y <- data_y[valid_indices]
  
  # Check if there are enough observations
  if (length(data_x) > 2 & length(data_y) > 2) {
    cor_test <- cor.test(data_x, data_y, method = "spearman")
    correlation <- cor_test$estimate
    p_value <- cor_test$p.value
  } else {
    correlation <- NA
    p_value <- NA
  }
  
  return(data.frame(Variable1 = x, Variable2 = y, Correlation = correlation, P_Value = p_value))
}

# List of AGE measurements and ingredients
age_measurements <- c('CML_ug_per_g_food', 'MG_ug_per_g_food', 'SF_ug_per_g_food', 'LF_AGE_ug_per_g_food', 
                      'CML_ug_per_kcal_food', 'MG_ug_per_kcal_food', 'SF_ug_per_kcal_food', 'LF_ug_per_kcal_food', 
                      'CML_plus_MG_g', 'CML_plus_MG_kcal', 'Combined_fluo_g', 'Combined_fluo_kcal', 
                      'Total_AGE_Score_g', 'Total_AGE_Score_kcal')
ingredients <- c('Percent_moisture', 'Percent_max_Crude_protein', 'Percent_max_Crude_fat', 'Percent_Crude_fiber', 'kcal/kg')

# Initialize an empty data frame to store the results
results <- data.frame()

# Compute correlations for all food types
for (age_measure in age_measurements) {
  for (ingredient in ingredients) {
    print(paste("Computing correlation for:", age_measure, "and", ingredient))
    result <- compute_correlations(data, age_measure, ingredient)
    result$Type <- 'All'
    print(result)
    results <- rbind(results, result)
  }
}

# Compute correlations for canned food only
data_canned <- data %>% filter(Type == 'Canned') %>% drop_na(all_of(columns_to_convert))
for (age_measure in age_measurements) {
  for (ingredient in ingredients) {
    print(paste("Computing correlation for Canned:", age_measure, "and", ingredient))
    result <- compute_correlations(data_canned, age_measure, ingredient)
    result$Type <- 'Canned'
    print(result)
    results <- rbind(results, result)
  }
}

# Compute correlations for kibble food only
data_kibble <- data %>% filter(Type == 'Kibble') %>% drop_na(all_of(columns_to_convert))
for (age_measure in age_measurements) {
  for (ingredient in ingredients) {
    print(paste("Computing correlation for Kibble:", age_measure, "and", ingredient))
    result <- compute_correlations(data_kibble, age_measure, ingredient)
    result$Type <- 'Kibble'
    print(result)
    results <- rbind(results, result)
  }
}

# Save the results to a CSV file
write.csv(results, 'correlations_results.csv', row.names = FALSE)

# Print the results
print(results)
#this code was working, I need to find which library had drop_na


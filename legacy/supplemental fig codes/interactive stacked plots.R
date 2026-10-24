# interactice stacked plots for Bobzilla meeting
# run the "pre processing" code first.

##=========================================== 11-06-24 version moist================
## this is still the stacked plot, but making it interactive high resolution and color coded.
# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)

# Load data
data2 <- data_filtered_moist 

# Concatenate Make, Description, Type, and ID for better labeling
data2 <- data2 %>%
  mutate(DogFoodLabelFull = paste(Make, Description,ID, sep = " - "))

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

# read in data
data <- data_filtered_moist

# Concatenate Make, Description, Type, and ID for better labeling
data <- data %>%
  mutate(DogFoodLabelFull = paste(Make, Description, ID, sep = " - "))

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


#==================
# Save the data_filtered_moist_ss to a CSV
write.csv(data_filtered_moist_ss, "data_filtered_moist_ss.csv", row.names = FALSE)

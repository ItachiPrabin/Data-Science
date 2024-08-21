# Load necessary libraries for data manipulation and visualization
library(tidyverse)
library(ggplot2)    
library(fmsb)       
library(scales)     

# Read in the broadband speed data from a CSV file
broadband_data = read_csv("X:/College projects/4th sem/Data Science/Cleaned/cleanedbroadband_speed.csv")

# Create a boxplot to compare the average download speeds between counties
ggplot(broadband_data, aes(x = County, y = `Average download speed (Mbit/s)`, fill = County)) +
  geom_boxplot() +
  labs(title = "Average Download Speed by County",
       x = "County",
       y = "Average Download Speed (Mbit/s)") +
  theme_minimal()

# Calculate average and maximum download speeds for each town/city in Cornwall
cornwall_speed = broadband_data %>%
  filter(County == "CORNWALL") %>%
  group_by(`Town/City`) %>% 
  summarize(
    avg_download_speed = mean(`Average download speed (Mbit/s)`),  
    max_download_speed = max(`Maximum download speed (Mbit/s)`)    
  ) %>% 
  pivot_longer(cols = c(avg_download_speed, max_download_speed), names_to = "SpeedType", values_to = "Speed")  

# Bar chart for average and maximum download speeds by town/city in Cornwall
ggplot(cornwall_speed, aes(x = `Town/City`, y = Speed, fill = SpeedType)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average and Maximum Download Speeds by Town/City in Cornwall",
       x = "Town/City",
       y = "Speed (Mbit/s)") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),  # Rotate x-axis labels for better readability
        legend.position = "top")  

# Calculate average and maximum download speeds for each town/city in Bristol
bristol_speed = broadband_data %>%
  filter(County == "CITY OF BRISTOL") %>%
  group_by(`Town/City`) %>% 
  summarize(
    avg_download_speed = mean(`Average download speed (Mbit/s)`),  
    max_download_speed = max(`Maximum download speed (Mbit/s)`)    
  ) %>% 
  pivot_longer(cols = c(avg_download_speed, max_download_speed), names_to = "SpeedType", values_to = "Speed")     # Reshape the data for easier plotting (long format)

# Bar chart for average and maximum download speeds by town/city in Bristol
ggplot(bristol_speed, aes(x = `Town/City`, y = Speed, fill = SpeedType)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average and Maximum Download Speeds in Bristol",
       x = "Speed Type",    # Note: The x-axis label should probably be "Town/City" instead of "Speed Type"
       y = "Speed (Mbit/s)") +
  theme_minimal()

# Load necessary libraries for data manipulation and visualization
library(tidyverse)
library(ggplot2)    
library(fmsb)       # For radar charts and related plotting
library(scales)     

# Read in the cleaned housing data from a CSV file
housing_data = read_csv("X:/College projects/4th sem/Data Science/Cleaned/cleanedhousing.csv")
head(housing_data)

# Filter data for years 2020 and onwards, and calculate the average price by Year and Town/City
housing_summary = housing_data %>%
  filter(Year >= 2020) %>%                       
  group_by(Year, `Town/City`) %>%                
  summarise(avg_price = mean(Price))             

# Filter the summarized data to include only the year 2023
housing_data_2023 = housing_summary %>%
  filter(Year == 2023)                           

view(housing_data_2023)  # View the data for 2023 to verify the filtered results

# Create a bar chart for average house prices in 2023, categorized by Town/City
ggplot(housing_data_2023, aes(x = `Town/City`, y = avg_price, fill = `Town/City`)) +
  geom_bar(stat = "identity") +                  # stat = "identity" means heights of bars represent values in the data
  ggtitle("Average House Price in 2023 by Town") +  
  ylab("Average Price") +                        
  xlab("Town") +                                 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for better readability

# Create a boxplot to visualize the distribution of house prices in 2023, categorized by County
ggplot(housing_data %>% filter(Year == 2023), aes(x = County, y = Price, fill = County)) +
  geom_boxplot() +                               
  ggtitle("House Prices in 2023") +              
  ylab("Price") +                               
  xlab("County") +                               
  theme_minimal()                               

# Group the data by Year and County
house_years_c = housing_data %>%
  filter(Year >= 2020) %>%                       
  group_by(Year, County) %>%                     
  summarise(avg_price = mean(Price))             

# Create a line graph showing the trend of average house prices from 2020 to 2023, categorized by County
ggplot(house_years_c, aes(x = Year, y = avg_price, color = County)) +
  geom_line(size = 1) +                          # set line thickness with size
  geom_point(size = 2) +                         # Add points to emphasize data points
  ggtitle("Average House Price from 2020 to 2023") +  
  ylab("Average Price") +                        
  xlab("Year") +                                 
  theme_minimal()                               # Apply a minimalistics theme to the plot













#theme_minimal()= Apply a minimalistics theme to the plot
#library(fmsb)= For radar charts and related plotting

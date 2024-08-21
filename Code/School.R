# Load necessary libraries for data manipulation and visualization
library(tidyverse)
library(ggplot2)   
library(scales)     

# Read in the school data from a CSV file
school_data = read_csv("X:/College projects/4th sem/Data Science/Cleaned/cleanedschool.csv")

# Filter the school data to only include records from the year 2022
data_2022 = school_data %>%
  filter(YEAR == 2022)

# Create a boxplot to compare Attainment 8 Scores between counties for the year 2022
ggplot(data_2022, aes(x = COUNTY, y = ATT8SCR, fill = COUNTY)) +
  geom_boxplot() +
  labs(title = "Average Attainment 8 Scores for 2023 by County",
       x = "County",
       y = "Attainment 8 Score") +
  theme_minimal()

# Filter the school data for Bristol schools in the year 2021
bristol_attainment = school_data %>%
  filter(YEAR == 2021) %>%
  filter(COUNTY == "Bristol")

# Line graph showing the Attainment 8 Scores for individual schools in Bristol in 2021
ggplot(bristol_attainment, aes(x = SCHNAME, y = ATT8SCR, group = 1)) +
  geom_line(color = "purple") +
  geom_point() +
  labs(title = "Bristol Schools: Average Attainment 8 Score (2021-2022)",
       x = "School Name",
       y = "Average Attainment 8 Score") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate school names on x-axis for readability

# Filter the school data for Cornwall schools in the year 2021
cornwall_attainment = school_data %>%
  filter(YEAR == 2021) %>%
  filter(COUNTY == "Cornwall")

# Line graph showing the Attainment 8 Scores for individual schools in Cornwall in 2021
ggplot(cornwall_attainment, aes(x = SCHNAME, y = ATT8SCR, group = 1)) +
  geom_line(color = "purple") +
  geom_point() +
  labs(title = "Cornwall Schools: Average Attainment 8 Score (2021-2022)",
       x = "School Name",
       y = "Average Attainment 8 Score") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate school names on x-axis for readability

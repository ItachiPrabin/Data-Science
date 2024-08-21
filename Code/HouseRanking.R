# Load necessary libraries
library(tidyverse)

# Load the data
housing_data = read_csv("X:/College projects/4th sem/Data Science/Cleaned/cleanedhousing.csv")

# Preprocessing for Housing Ranking
house_price = housing_data %>%
  filter(Year == 2023) %>%
  mutate(TOWN = str_trim(toupper(`Town/City`))) %>%
  group_by(TOWN) %>%
  summarise(avg_price = mean(Price)) %>%
  select(avg_price, TOWN) %>%
  na.omit() %>%
  distinct()

# Calculate normalized price
min_price = min(house_price$avg_price)
max_price = max(house_price$avg_price)

houserank = house_price %>%
  mutate(norm_price = 1 - (avg_price - min_price) / (max_price - min_price)) %>%
  arrange(desc(norm_price))

# View the housing ranking
View(houserank)

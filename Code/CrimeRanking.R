library(tidyverse)
# Load the data
crime_data = read_csv("X:/College projects/4th sem/Data Science/Cleaned/cleanedcrime.csv")
housing_data = read_csv("X:/College projects/4th sem/Data Science/Cleaned/cleanedhousing.csv")

# Preprocessing for Crime Rate Ranking
town = housing_data %>%
  mutate(postcode = str_trim(substring(Postcode, 1, 6))) %>%
  mutate(TOWN = str_trim(toupper(`Town/City`))) %>%
  select(postcode, TOWN) %>%
  distinct()

sel_crime = crime_data %>%
  filter(Year == 2023) %>%
  group_by(postcode) %>%
  summarise(crimeno = n()) %>%
  arrange(desc(crimeno)) %>%
  select(postcode, crimeno)

final_crime = sel_crime %>%
  left_join(town, by = "postcode") %>%
  na.omit() %>%
  distinct()

last_crime = final_crime %>%
  group_by(TOWN) %>%
  summarise(crimerate = sum(crimeno)) %>%
  select(TOWN, crimerate)

# Calculate normalized crime rate
min_crimerate = min(last_crime$crimerate)
max_crimerate = max(last_crime$crimerate)

crimerank = last_crime %>%
  mutate(norm_crimerate = 1 - (crimerate - min_crimerate) / (max_crimerate - min_crimerate)) %>%
  arrange(desc(norm_crimerate))

# View the crime rate ranking
View(crimerank)

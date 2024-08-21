# Load necessary libraries
library(tidyverse)

# Load the data
broadband_data = read_csv("X:/College projects/4th sem/Data Science/Cleaned/cleanedbroadband_speed.csv")

# Preprocessing for Broadband Speed Ranking
broadband_sel = broadband_data %>%
  group_by(`Town/City`) %>%
  summarise(
    avg_upl_speed = mean(`Average upload speed (Mbit/s)`),
    avg_down_speed = mean(`Average download speed (Mbit/s)`)
  ) %>%
  mutate(TOWN = str_trim(toupper(`Town/City`))) %>%
  select(TOWN, avg_upl_speed, avg_down_speed)

# Calculate normalized download speed
min_download_speed = min(broadband_sel$avg_down_speed)
max_download_speed = max(broadband_sel$avg_down_speed)

broadbandrank = broadband_sel %>%
  mutate(norm_download_speed = (avg_down_speed - min_download_speed) / (max_download_speed - min_download_speed)) %>%
  arrange(desc(norm_download_speed))

# View the broadband speed ranking
View(broadbandrank)

# Load necessary libraries
library(tidyverse)

# Load the data
school_data = read_csv("X:/College projects/4th sem/Data Science/Cleaned/cleanedschool.csv")

# Preprocessing for School Ranking
att8 = school_data %>%
  group_by(TOWN) %>%
  summarise(av_att8 = mean(ATT8SCR)) %>%
  select(TOWN, av_att8) %>%
  distinct() %>%
  mutate(TOWN = str_trim(toupper(TOWN)))

# Calculate normalized Attainment 8 score
min_att8_score = min(att8$av_att8)
max_att8_score = max(att8$av_att8)

schoolrank = att8 %>%
  mutate(norm_att8_score = (av_att8 - min_att8_score) / (max_att8_score - min_att8_score)) %>%
  arrange(desc(norm_att8_score))

# View the school ranking
View(schoolrank)

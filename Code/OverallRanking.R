library(tidyverse)
# Final Overall Ranking
ranking_data = house_price %>%
  left_join(att8, by = "TOWN") %>%  
  left_join(broadband_sel, by = "TOWN") %>%  
  left_join(last_crime, by = "TOWN") %>%
  na.omit()

# Calculate normalized scores and final score
min_max <- ranking_data %>%
  summarise(
    min_download_speed = min(avg_down_speed),
    max_download_speed = max(avg_down_speed),
    min_upload_speed = min(avg_upl_speed),
    max_upload_speed = max(avg_upl_speed),
    min_att8_score = min(av_att8),
    max_att8_score = max(av_att8),
    min_price = min(avg_price),
    max_price = max(avg_price),
    min_crimerate = min(crimerate),
    max_crimerate = max(crimerate)
  )

ranking_data <- ranking_data %>%
  mutate(
    norm_download_speed = (avg_down_speed - min_max$min_download_speed) / (min_max$max_download_speed - min_max$min_download_speed),
    norm_upload_speed = (avg_upl_speed - min_max$min_upload_speed) / (min_max$max_upload_speed - min_max$min_upload_speed),
    norm_att8_score = (av_att8 - min_max$min_att8_score) / (min_max$max_att8_score - min_max$min_att8_score),
    norm_price = 1 - (avg_price - min_max$min_price) / (min_max$max_price - min_max$min_price),
    norm_crimerate = 1 - (crimerate - min_max$min_crimerate) / (min_max$max_crimerate - min_max$min_crimerate),
    final_score = norm_download_speed + norm_upload_speed + norm_att8_score + norm_price + norm_crimerate
  )

final_rank = ranking_data %>%
  select(TOWN, avg_price, crimerate, av_att8, avg_down_speed, final_score) %>%
  arrange(desc(final_score))

# View the final overall ranking
View(final_rank)

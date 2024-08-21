# Load necessary libraries for data manipulation and visualization
library(tidyverse)  
library(ggplot2)    
library(fmsb)      
library(scales)     

# Read in the crime data from a CSV file
crime_data = read_csv("X:/College projects/4th sem/Data Science/Cleaned/cleanedcrime.csv")

# Filter data specifically for vehicle crime
vehicle_crime_data = crime_data %>%
  filter(`Crime type` == "Vehicle crime")

# Summarize vehicle crime data by year
vehicle_crime_summary = vehicle_crime_data %>%
  group_by(Year) %>%
  summarise(total_vehicle_crime = sum(count, na.rm = TRUE))

# Prepare data for radar chart (transposing and renaming columns)
radar_chart_data = as.data.frame(t(vehicle_crime_summary$total_vehicle_crime))
colnames(radar_chart_data) = vehicle_crime_summary$Year

# Add max and min rows for proper scaling in the radar chart
radar_chart_data = rbind(rep(max(vehicle_crime_summary$total_vehicle_crime), length(years)), 
                         rep(0, length(years)), 
                         radar_chart_data)

# Adjust plot margins and create the radar chart
par(mar = c(2, 2, 2, 2))  # Adjust plot margins for better fit

radarchart(radar_chart_data,
           axistype = 1,            # Axis type (1 = clockwise)
           pcol = "purple",         
           pfcol = "yellow",        
           plwd = 4,                # Line width
           cglcol = "black",        
           axislabcol = "black",    
           caxislabels = seq(0, max(vehicle_crime_summary$total_vehicle_crime), length.out = 5),
           title = "Vehicle Crime Rate from 2021 to 2024"
)

# Create a pie chart to show the distribution of robberies by month in 2023
robbery_data = crime_data %>%
  filter(`Crime type` == "Robbery" & Year == "2023") %>%
  group_by(Month) %>%
  summarise(robbery_count = n()) %>%
  mutate(robbery_percentage = robbery_count / sum(robbery_count) * 100)

# Generate the pie chart with percentage labels
ggplot(robbery_data, aes(x = "", y = robbery_percentage, fill = as.factor(Month))) +
  geom_bar(width = 1, stat = "identity") +       # Use a bar chart in polar coordinates
  coord_polar("y") +                             # Convert bar chart to pie chart
  geom_text(aes(label = paste0(round(robbery_percentage, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Distribution of Robberies by Month (2023)", fill = "Month") +
  theme_minimal()

# Filter and summarize drug offence data for Cornwall
cornwall_drug_data = drug_offence_data %>%
  filter(counties == "Cornwall") %>%
  distinct(`LSOA code`, .keep_all = TRUE) %>%    # Remove duplicate rows based on LSOA code
  summarise(total_population_cornwall = sum(count),
            total_drug_offences_cornwall = n())

# Filter and summarize drug offence data for Bristol
bristol_drug_data = drug_offence_data %>%
  filter(counties == "Bristol, City of") %>%
  distinct(`LSOA code`, .keep_all = TRUE) %>%
  summarise(total_population_bristol = sum(count),
            total_drug_offences_bristol = n())

# Calculate drug offence rates per 10,000 people for Cornwall
cornwall_drug_data = cornwall_drug_data %>%
  mutate(drug_offence_rate_cornwall = (total_drug_offences_cornwall / total_population_cornwall) * 10000)

# Calculate drug offence rates per 10,000 people for Bristol
bristol_drug_data = bristol_drug_data %>%
  mutate(drug_offence_rate_bristol = (total_drug_offences_bristol / total_population_bristol) * 10000)

# Combine data for Cornwall and Bristol
combined_drug_data = bind_rows(
  cornwall_drug_data %>% mutate(county = "Cornwall"),
  bristol_drug_data %>% mutate(county = "Bristol, City of")
)

# Create a boxplot to compare drug offence rates between Cornwall and Bristol
ggplot(combined_data, aes(x = county, y = offence_rate, fill = county)) +
  geom_boxplot() +
  labs(title = "Distribution of Drug Offence Rates (2023)",
       x = "Location",
       y = "Offence Rate (per 10,000)") +
  theme_minimal()

library(tidyverse)
library(ggplot2)

#----------------------------------------------------------------------------------
crime_data = read_csv("X:/College projects/4th sem/Data Science/Cleaned/cleanedcrime.csv")
school_data = read_csv("X:/College projects/4th sem/Data Science/Cleaned/cleanedschool.csv")
housing_data = read_csv("X:/College projects/4th sem/Data Science/Cleaned/cleanedhousing.csv")
broadband_data = read_csv("X:/College projects/4th sem/Data Science/Cleaned/cleanedbroadband_speed.csv")
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#House Price vs Average Download Speed
housing_data <- housing_data %>%
  select(Postcode, Price)

broadband_data <- broadband_data %>%
  select(postcode_space, `Average download speed (Mbit/s)`)%>%
  rename(Postcode = postcode_space)

combined_data <- merge(housing_data, broadband_data, by = "Postcode")

view(combined_data)


ggplot(data = combined_data, aes(x = Price, y = `Average download speed (Mbit/s)`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "purple") +
  labs(title = "House Prices vs Average Download Speed", 
       x = "House Price", 
       y = "Average Download Speed (Mbit/s)") +
  theme_minimal()
view(combined_data)
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#Attainment 8 Score VS House Price
school_data=school_data%>%
  filter(YEAR==2022)%>%
  select(PCODE,ATT8SCR)%>%
  rename(Postcode="PCODE")

housing_data <- housing_data %>%
  select(Postcode, Price)

combined_data <- merge(school_data, housing_data, by = "Postcode")
#View(combined_data)

ggplot(data = combined_data, aes(x = ATT8SCR, y = Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "purple") +
  labs(title = "Average attainment 8 score vs House Price", 
       x = "ATT8SCR", 
       y = "Price") +
  theme_minimal()

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#Average Download Speed VS Attainment 8 Score

broadband_data <- broadband_data %>%
  select(postcode_space, `Average download speed (Mbit/s)`)%>%
  rename(Postcode = postcode_space)
view(school_data)

school_data=school_data%>%
  filter(YEAR==2022)%>%
  select(PCODE,ATT8SCR)%>%
  rename(Postcode="PCODE")

combined_data <- merge(broadband_data, school_data, by = "Postcode")
#View(combined_data)

ggplot(data = combined_data, aes(x = `Average download speed (Mbit/s)`, y = ATT8SCR)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "purple") +
  labs(title = "Average download speed VS Attainment 8 Score", 
       x = "Average download speed", 
       y = "Attainment 8 Score") +
  theme_minimal()

merge_crime = read_csv("D:/Sem 4/data science/assignment/Cleaned/finalcleanedcrime.csv")

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#house price vs Drug Rates
housing_data = read_csv("X:/College projects/4th sem/Data Science/Cleaned/cleanedhousing.csv")
crime_data = read_csv("X:/College projects/4th sem/Data Science/Cleaned/cleanedcrime.csv")

housing_2023 = housing_data %>%
  filter(Year == 2023) %>%
  mutate(Postcode = str_trim((substring(Postcode, 1, 6))))%>%
  distinct()

housing_2023
# Filter and aggregate crime data for drug-related crimes in 2023
drug_crime_2023 <- crime_data %>%
  filter(Year == 2023, `Crime type` == "Drugs") %>%
  group_by(postcode) %>%
  summarize(Drug_Rates = sum(count)) %>%
  distinct() %>%
  na.omit()

drug_crime_2023

# Perform a left join instead of a merge
combined_data=housing_2023 %>%
  left_join(drug_crime_2023, by = c("Postcode" = "postcode")) %>%
  distinct()%>%
  na.omit()

view(combined_data)
# Create the linear model plot
ggplot(data = combined_data, aes(x = Drug_Rates, y = Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "purple") +
  labs(title = "House Prices vs Drug Rates (2023)", 
       x = "House Price", 
       y = "Drug Rates") +
  theme_minimal()

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#Average download speed VS drug offence rates (per ten thousand people)

broadband_data = read_csv("X:/College projects/4th sem/Data Science/Cleaned/cleanedbroadband_speed.csv")
crime_data = read_csv("X:/College projects/4th sem/Data Science/Cleaned/cleanedcrime.csv")

pop = crime_data %>%
  select(postcode, count, counties, Year) %>%
  distinct()

pop_counties = pop %>%
  group_by(counties, Year)%>%
  summarize(count = sum(count))

drug=crime_data %>%
  filter(`Crime type` == "Drugs") %>%
  group_by(counties, Year) %>%
  summarize(Drug_Rates = n()) %>%
  distinct() %>%
  na.omit()

drug_pop=left_join(drug, pop_counties, by = c("counties", "Year"))%>%
  mutate(per10k = (Drug_Rates / count) * 10000)
view(drug_pop)

broadband_sel = broadband_data %>%
  select(postcode_space, `Average download speed (Mbit/s)`)%>%
  mutate(postcode = str_trim((substring(postcode_space, 1, 6))))%>%
  select(postcode, `Average download speed (Mbit/s)`)
view(broadband_sel)

final = left_join(crime_data, broadband_sel, by= c("postcode"))
view(final)

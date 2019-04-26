library(tidyverse)
library(stringr)
library(janitor)
library(gt)
library(dplyr)

download.file("https://github.com/BeauMeche/Final_proj_milestone_3/raw/master/Census_2017_edu_attainment.csv",
              destfile = "2017_data.csv",
              mode = "wb")

download.file("https://github.com/BeauMeche/Final_proj_milestone_3/raw/master/Census_edu_attainment_2016.csv",
              destfile = "2016_data.csv", 
              mode = "wb")

download.file("https://github.com/BeauMeche/Final_proj_milestone_3/raw/master/Census_edu_attainment_2015.csv",
              destfile = "2015_data.csv",
              mode = "wb")

data_2017 <- read_csv(file = "2017_data.csv", skip = 1) %>% 
  clean_names()

census_2017_nomargin <- data_2017[, ! str_detect(names(data_2017), pattern = "margin_of_error")] 


# ORIGINAL DATA_______________________

data_2016 <- read_csv(file = "2016_data.csv", skip = 1)
census_2016_nomargin <- data_2016[, ! str_detect(names(data_2016), pattern = "margin_of_error")]

data_2015 <- read_csv(file = "2015_data.csv", skip = 1)
census_2015_nomargin <- data_2015[, ! str_detect(names(data_2015), pattern = "margin_of_error")]

# CREATE GENDER OPTIONS_________________________

males_2017 <- census_2017_nomargin[, str_detect(names(census_2017_nomargin), pattern = "male")|
                                     str_detect(names(census_2017_nomargin), pattern = "geography")] %>% 
  mutate(gender = "male")

females_2017 <- census_2017_nomargin[, str_detect(names(census_2017_nomargin), pattern = "female")|
                                     str_detect(names(census_2017_nomargin), pattern = "geography")] %>% 
  mutate(gender = "female")

# CREATE TOTALS

total_2017 <- census_2017_nomargin[, str_detect(names(census_2017_nomargin), pattern = "total")|
                                     str_detect(names(census_2017_nomargin), pattern = "geography")] %>% 
  mutate(gender = "NA")

# GET RID OF UNNEEDED NAME COMPONENTS

names(total_2017) <- total_2017 %>% 
  names() %>% 
  gsub(x = colnames(total_2017),
       pattern = "total_estimate_",
       replacement = "")

names(males_2017) <- males_2017 %>% 
  names() %>% 
  gsub(x = colnames(males_2017),
       pattern = "male_estimate_",
       replacement = "")

names(females_2017) <- females_2017 %>% 
  names() %>% 
  gsub(x = colnames(females_2017),
       pattern = "female_estimate_",
       replacement = "")

nice_2017 <- census_2017_nomargin %>% 
  gather(total_estimate_population_18_to_24_years:percent_female_estimate_median_earnings_in_the_past_12_months_in_2017_inflation_adjusted_dollars_population_25_years_and_over_with_earnings_graduate_or_professional_degree,
         key = "key",
         value = "value", 
         na.rm = TRUE)

#Looking at rows of the census data dealing in percentages. Parsed key to get
#age ranges, so I can start with a graphic of where the elderly people, middle
#age, and young people tend to congregate toward. Need to filter the "Value" col
#still, but good progress so far.

percents_of_2017 <- nice_2017 %>% 
  filter(str_detect(key, pattern = "percent_estimate_population_")) %>% 
  separate(key, c("type", "accuracy", "type_2", "age_min", "to", "age_max", "right")) %>% 
  unite(ages, c(age_min, to, age_max), sep = "_", remove = TRUE) %>% 
  filter(ages != "25_years_and") %>% 
  select(-type_2, -type, -accuracy)


# %>% 
#   separate(key, into = c("type", "condifence", "type_2", "age_min", "to", "age_max"))


# old plot, not quite relevant
#census_2017_nomargin <- census_2017[, ! str_detect(names(census_2017), pattern = "Margin of Error")]

# census_2017_nomargin %>% 
# ggplot() +
#   geom_jitter(aes(x = "Total; Estimate; Population 18 to 24 years",
#                  y = "Geography")) + 

#   theme_minimal() + 
#   labs(x = "States", y = "Estimated Population of Men")


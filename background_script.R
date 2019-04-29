library(tidyverse)
library(stringr)
library(janitor)
library(gt)
library(dplyr)
library(ggthemes)

download.file("https://github.com/BeauMeche/Final_proj_milestone_3/raw/master/Census_2017_edu_attainment.csv",
              destfile = "2017_data.csv",
              mode = "wb")

download.file("https://github.com/BeauMeche/Final_proj_milestone_3/raw/master/Census_edu_attainment_2016.csv",
              destfile = "2016_data.csv", 
              mode = "wb")


data_2017 <- read_csv(file = "2017_data.csv", skip = 1) %>% 
  clean_names()

census_2017_nomargin <- data_2017[, ! str_detect(names(data_2017), pattern = "margin_of_error")] 


# ORIGINAL DATA_______________________

data_2016 <- read_csv(file = "2016_data.csv", skip = 1) %>% 
  clean_names()

census_2016_nomargin <- data_2016[, ! str_detect(names(data_2016), pattern = "margin_of_error")]

# CREATE GENDER OPTIONS_________________________

males_2017 <- census_2017_nomargin[, str_detect(names(census_2017_nomargin), pattern = "male")|
                                     str_detect(names(census_2017_nomargin), pattern = "geography")] %>% 
  mutate(gender = "male")



            # This code is wrong, but the processs works so I am keeping it for reference
            # for now.
            
            percents_of_2017 <- nice_2017 %>%
              filter(str_detect(key, pattern = "percent_estimate_population_")) %>%
              separate(key, c("type", "accuracy", "type_2", "age_min", "to", "age_max", "right")) %>%
              unite(ages, c(age_min, to, age_max), sep = "_", remove = TRUE) %>%
              filter(ages != "25_years_and") %>%
              select(-type_2, -type, -accuracy)

# plot for milestone 4: where are the young people? 

pops_of_2017 <- nice_2017 %>% 
  filter(! str_detect(key, pattern = "percent")) %>% 
  separate(key, c("type", "accuracy", "type_2", "age_min", "to", "age_max", "right")) %>% 
  unite(ages, c(age_min, to, age_max), sep = "_", remove = TRUE) %>% 
  filter(ages %in% c("18_to_24", "35_to_44", "45_to_64", "65_years_and")) %>% 
  select(-type_2, -type, -accuracy) %>% 
  filter(id2 != 72) %>% 
  
  # duplicated() tells me that the first 51 geographical terms are repeated, so
  # I am removing them.
  
  head(51) %>% 
  mutate(value = as.numeric(value)) %>%
  arrange(desc(value)) 


ggplot(pops_of_2017) + 
  geom_bar(stat = "identity", aes(x = geography, y = value, 
                                  fill = geography), show.legend = FALSE) +
  coord_flip() + theme_fivethirtyeight() +
  labs(
    title = "Where are the young people?",
    subtitle = "Shown: the 10 states with the most people aged 18 to 24", 
    caption = "Source: the U.S. Census")
  










# test <- nice_2017 %>% 
#   if (str_detect(key, "18_to")) {
#     separate(key, pattern = str_extract(key, age)) } else{
#       str_replace(key, "__")
#     }
      


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


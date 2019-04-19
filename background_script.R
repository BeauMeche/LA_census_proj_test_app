library(tidyverse)
library(stringr)

download.file("https://github.com/BeauMeche/Final_proj_milestone_3/raw/master/Census_2017_edu_attainment.csv",
              destfile = "2017_data.csv",
              mode = "wb")

download.file("https://github.com/BeauMeche/Final_proj_milestone_3/raw/master/Census_edu_attainment_2016.csv",
              destfile = "2016_data.csv", 
              mode = "wb")

download.file("https://github.com/BeauMeche/Final_proj_milestone_3/raw/master/Census_edu_attainment_2015.csv",
              destfile = "2015_data.csv",
              mode = "wb")

data_2017 <- read_csv(file = "2017_data.csv", skip = 1)
census_2017_nomargin <- data_2017[, ! str_detect(names(data_2017), pattern = "Margin of Error")]

data_2016 <- read_csv(file = "2016_data.csv", skip = 1)
census_2016_nomargin <- data_2016[, ! str_detect(names(data_2016), pattern = "Margin of Error")]

data_2015 <- read_csv(file = "2015_data.csv", skip = 1)
census_2015_nomargin <- data_2015[, ! str_detect(names(data_2015), pattern = "Margin of Error")]

#census_2017_nomargin <- census_2017[, ! str_detect(names(census_2017), pattern = "Margin of Error")]

census_2017_nomargin %>% 
ggplot() +
  geom_jitter(aes(x = "Total; Estimate; Population 18 to 24 years",
                 y = "Geography")) + 
  theme_minimal() + 
  labs(x = "States", y = "Estimated Population of Men")

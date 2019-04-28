library(tidyverse)
library(stringr)
library(janitor)
library(gt)
library(dplyr)
library(ggthemes)




total_pop_2017 <- census_2017_nomargin[, str_detect(names(census_2017_nomargin), pattern = "total_estimate_population")|
                                       str_detect(names(census_2017_nomargin), pattern = "geography")] %>%
  mutate(totalpop = total_estimate_population_18_to_24_years + total_estimate_population_25_to_34_years +
           total_estimate_population_35_to_44_years + total_estimate_population_45_to_64_years +
           total_estimate_population_65_years_and_over) %>%
  select(geography, totalpop)

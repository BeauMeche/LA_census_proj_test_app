library(tidyverse)
library(stringr)
library(janitor)
library(gt)
library(dplyr)
library(ggthemes)


# dealing with total population change here

total_pop_2017 <- census_2017_nomargin[, str_detect(names(census_2017_nomargin), pattern = "total_estimate_population")|
                                       str_detect(names(census_2017_nomargin), pattern = "geography")] %>%
  mutate(totalpop = total_estimate_population_18_to_24_years + total_estimate_population_25_to_34_years +
           total_estimate_population_35_to_44_years + total_estimate_population_45_to_64_years +
           total_estimate_population_65_years_and_over) %>%
  select(geography, totalpop)


total_pop_2016 <- census_2016_nomargin[, str_detect(names(census_2016_nomargin), pattern = "total_estimate_population")|
                                         str_detect(names(census_2016_nomargin), pattern = "geography")] %>%
  mutate(totalpop = total_estimate_population_18_to_24_years + total_estimate_population_25_to_34_years +
           total_estimate_population_35_to_44_years + total_estimate_population_45_to_64_years +
           total_estimate_population_65_years_and_over) %>%
  select(geography, totalpop)

# this data shows population flux by state from 2016-2017. Percentage change is
# negligible (ie. .22%), so whole #s seems preferable here

agg_total_pop <- left_join(by = "geography", total_pop_2017, total_pop_2016,
                           suffix = c(".17", ".16")) %>% 
  mutate(sum = totalpop.17 + totalpop.16) %>% 
  mutate(change = ((totalpop.17 - totalpop.16)))

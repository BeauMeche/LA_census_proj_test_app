library(stringr)
library(janitor)
library(gt)
library(dplyr)
library(ggthemes)
library(leaflet)
library(geojsonio)
library(tigris)
library(tidyverse)

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
  mutate(change = ((totalpop.17 - totalpop.16))) %>% 
  write_rds("aggregate_pop")

# library(maps)
# mapStates = map("state", fill = TRUE, plot = FALSE)
# leaflet(data = mapStates) %>% addTiles() %>%
#   addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)


# from the web: i cant get to the json file :(
# From http://leafletjs.com/examples/choropleth/us-states.js
# download.file("https://leafletjs.com/examples/choropleth/us-states.js",
#               destfile = "us-states.geojson",
#               mode = "wb")






#interactivity map

states <- states()
all_us <- geo_join(states, agg_total_pop, "NAME", "geography")

bins <- c(-45000, -15000, 0, 15000, 30000,
          45000, 100000, 200000, Inf)

pal <- colorBin("YlOrRd", domain = all_us$change, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%g people",
  all_us$geography, all_us$change
) %>% lapply(htmltools::HTML)

leaflet(all_us) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
  addPolygons(
    fillColor = ~pal(change),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#750",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal, values = ~all_us$change, opacity = 0.7, title = NULL,
            position = "bottomright")


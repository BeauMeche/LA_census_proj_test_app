library(tidyverse)
library(shiny)
library(ggplot2)
library(ggthemes)
library(stringr)
library(janitor)
library(gt)
library(readr)
library(dplyr)
library(ggthemes)
library(leaflet)
library(geojsonio)
library(tigris)
library(tidyverse)

agg_total_pop_2 <- read_rds("aggregate_pop")


ui <- fluidPage(
  #title
  titlePanel("A Smoother Look at U.S. Census data"),
  
  mainPanel( 
      tabPanel("Population Flux",
               selectInput("info_1", "What Do You Find Interesting?",
               c("Total Pop. in 2017" = "totalpop.17",
                 "Total Pop. in 2016" = "totalpop.16",
                 "What Changed?" = "change"),
               leafletOutput("all_states")
    )
  )
  ))



server <- function(input, output) {
  
  output$all_states <- renderLeaflet({
    
    states <- states()
    all_us <- geo_join(states, agg_total_pop_2, "NAME", "geography")
    
    bins <- c(-45000, -15000, 0, 15000, 30000,
              45000, 100000, 200000, Inf)
    
    pal <- colorBin("YlOrRd", domain = all_us$get(info_1), bins = bins)
    
    # labels <- sprintf(
    #   "<strong>%s</strong><br/>%g people",
    #   all_us$geography, all_us$change
    # ) %>% lapply(htmltools::HTML)
    
    leaflet(all_us) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
      addPolygons(
        fillColor = ~pal(all_us$get(info_1)),
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
      addLegend(pal = pal, values = ~all_us$get(info_1), opacity = 0.7, title = NULL,
                position = "bottomright")
  })
    

}


shinyApp(ui = ui, server = server)

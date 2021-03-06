library(tidyverse)
library(shiny)
library(ggplot2)
library(ggthemes)
library(stringr)
library(janitor)
library(gt)
library(dplyr)
library(ggthemes)
library(leaflet)
library(geojsonio)
library(tigris)
library(tidyverse)



younger_data <- read_rds("young_people_states.rds")

agg_total_pop_2 <- read_rds("aggregate_pop")


# make the UI

ui <- fluidPage(
  #title
  titlePanel("A Smoother Look at U.S. Census data"),
  
  mainPanel( 
    tabsetPanel(
             tabPanel("Young People",
                      plotOutput("younger")),
             tabPanel("Prime Age Men",
                      leafletOutput("all_states"),
                      selectInput(info_1, "What Do You Find Interesting?"),
                      c("Total Pop. in 2017" = "totalpop.17",
                        "Total Pop. in 2016" = "totalpop.16",
                        "What Changed?" = "change"))
             )
  )
  
    )


 

server <- function(input, output) {
  
  output$younger <- renderPlot({
    ggplot(younger_data) + 
      geom_bar(stat = "identity", aes(x = geography, y = value, 
                                      fill = geography), show.legend = FALSE) +
      coord_flip() + theme_fivethirtyeight() +
      labs(
        title = "Where are the young people?",
        subtitle = "Shown: the 10 states with the most people aged 18 to 24", 
        caption = "Source: the U.S. Census")
  })
  
  output$all_states <- renderLeaflet({
    
    states <- states()
    all_us <- geo_join(states, agg_total_pop_2, "NAME", "geography")
    
    bins <- c(-45000, -15000, 0, 15000, 30000,
              45000, 100000, 200000, Inf)
    
    pal <- colorBin("YlOrRd", domain = all_us$change, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g people",
      all_us$geography, all_us$all_us$get(input$info_1)
    ) %>% lapply(htmltools::HTML)
    
    leaflet(all_us) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
      addPolygons(
        fillColor = ~pal(all_us$get(input$info_1)),
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
      addLegend(pal = pal, values = ~all_us$get(input$info_1), opacity = 0.7, title = NULL,
                position = "bottomright")
  })
  
  
}


shinyApp(ui = ui, server = server)

library(tidyverse)
library(shiny)
library(ggplot2)
library(ggthemes)

download.file("https://github.com/BeauMeche/Final_proj_milestone_3/raw/master/Census_2017_edu_attainment.csv",
              destfile = "2017_data.csv",
              mode = "wb")

data_2017 <- read_csv(file = "2017_data.csv", skip = 1)

census_2017_nomargin <- data_2017[, ! str_detect(names(data_2017), pattern = "Margin of Error")]

# make the UI

ui <- fluidPage(
  #title
  titlePanel("Amount of Prime Aged People"),
  
  mainPanel( 
    tabsetPanel(tabPanel("Prime Age Men",
                      plotOutput("plot_1")),
             tabPanel("Prime Age Women",
                      plotOutput("plot_2"))
             )
  )
  
    )


 

server <- function(input, output) {
  
  output$plot_1 <- renderPlot({
    census_2017_nomargin %>% ggplot() +
      geom_jitter(aes(y = "Male; Estimate; Population 18 to 24 years",
                     x = "US States and Territories")) +
      labs(x = NULL, y = NULL) + 
      theme_economist_white()
                              })
  output$plot_2 <- renderPlot({
    census_2017_nomargin %>% ggplot() +
      geom_jitter(aes(y = "Female; Estimate; Population 18 to 24 years",
                      x = "US States and Territories")) +
      labs(x = NULL, y = NULL) + 
      theme_economist_white()
    
  })
  
  
}
  

shinyApp(ui = ui, server = server)

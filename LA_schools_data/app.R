library(tidyverse)
library(shiny)
library(ggplot2)
library(ggthemes)

download.file("https://github.com/BeauMeche/Final_proj_milestone_3/raw/master/Census_2017_edu_attainment.csv",
              destfile = "2017_data.csv",
              mode = "wb")

data_2017 <- read_csv(file = "2017_data.csv", skip = 1, 
                      col_types = cols(
                        .default = col_double(),
                        Id = col_character(),
                        Geography = col_character(),
                        `Percent; Estimate; Population 18 to 24 years` = col_character(),
                        `Percent; Margin of Error; Population 18 to 24 years` = col_character(),
                        `Percent Male; Estimate; Population 18 to 24 years` = col_character(),
                        `Percent Male; Margin of Error; Population 18 to 24 years` = col_character(),
                        `Percent Female; Estimate; Population 18 to 24 years` = col_character(),
                        `Percent Female; Margin of Error; Population 18 to 24 years` = col_character(),
                        `Percent; Estimate; Population 25 years and over` = col_character(),
                        `Percent; Margin of Error; Population 25 years and over` = col_character(),
                        `Percent Male; Estimate; Population 25 years and over` = col_character(),
                        `Percent Male; Margin of Error; Population 25 years and over` = col_character(),
                        `Percent Female; Estimate; Population 25 years and over` = col_character(),
                        `Percent Female; Margin of Error; Population 25 years and over` = col_character(),
                        `Total; Estimate; Percent high school graduate or higher` = col_character(),
                        `Total; Margin of Error; Percent high school graduate or higher` = col_character(),
                        `Male; Estimate; Percent high school graduate or higher` = col_character(),
                        `Male; Margin of Error; Percent high school graduate or higher` = col_character(),
                        `Female; Estimate; Percent high school graduate or higher` = col_character(),
                        `Female; Margin of Error; Percent high school graduate or higher` = col_character()))

census_2017_nomargin <- data_2017[, ! str_detect(names(data_2017), pattern = "Margin of Error")]


# make the UI

ui <- fluidPage(
  #title
  titlePanel("A Smoother Look at U.S. Census data"),
  
  mainPanel( 
    tabsetPanel(
             tabPanel("Young People",
                      plotOutput("younger")),
             tabPanel("Prime Age Men",
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
  
  output$younger <- renderPlot({
    ggplot(pops_of_2017) + 
      geom_bar(stat = "identity", aes(x = geography, y = value, 
                                      fill = geography), show.legend = FALSE) +
      coord_flip() + theme_fivethirtyeight() +
      labs(
        title = "Where are the young people?",
        subtitle = "Shown: the 10 states with the most people aged 18 to 24", 
        caption = "Source: the U.S. Census")
  })
  
  
}


shinyApp(ui = ui, server = server)

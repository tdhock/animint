library(shiny)
library(animint)
data(WorldBank)

nms <- c(
  "fertility.rate", "life.expectancy", "population",
  "GDP.per.capita.Current.USD", "literacy", "region",
  "longitude", "latitude", "income", "lending")

shinyUI(fluidPage(
  
  titlePanel("animint meets shiny"),
  
  h4(HTML("<a href='https://github.com/tdhock/animint/tree/master/inst/examples/shiny-WorldBank'> Click here </a> to view source")),
  
  br(),
  
  fluidRow(
    column(2,
      selectInput("x", "Choose an x variable", choices = nms,
                  selected = "fertility.rate"),
      selectInput("y", "Choose an y variable", choices = nms,
                  selected = "life.expectancy"),
      selectInput("color", "Choose an color variable", choices = nms,
                  selected = "region"),
      selectInput("size", "Choose a size variable", choices = nms,
                  selected = "population")
    ),
    column(5,
      plotOutput("ggplot", height = "300px")
    ),
    column(5, 
      animintOutput("animint")
    )
  )
    
))

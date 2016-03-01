library(shiny)
library(animint)
data(WorldBank)

nms <- c(
  "fertility.rate", "life.expectancy", "population",
  "GDP.per.capita.Current.USD", "literacy", "region",
  "longitude", "latitude", "income", "lending")

shinyUI(fluidPage(
  
  titlePanel("WorldBank data viz, select variable mappings"),
  
  h4(HTML("<a href='https://github.com/tdhock/animint/tree/master/inst/examples/shiny-WorldBank'> Click here </a> to view source")),
  
  br(),
  
  fluidRow(
    column(1,
      selectInput("x", "x variable", choices = nms,
                  selected = "fertility.rate"),
      selectInput("y", "y variable", choices = nms,
                  selected = "life.expectancy"),
      selectInput("color", "color variable", choices = nms,
                  selected = "region"),
      selectInput("size", "size variable", choices = nms,
                  selected = "population")
    ),
    column(2, 
      animintOutput("animint")
    )
  )
    
))

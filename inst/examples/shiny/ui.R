library(shiny)
library(animint)

nms <- names(mtcars)

# This example follows the shiny layout guide -- http://shiny.rstudio.com/articles/layout-guide.html
# Buy you can use whatever layout you wish :)
shinyUI(fluidPage(
  
  titlePanel("animint meets shiny"),
  
  h4(HTML("<a href='https://github.com/tdhock/animint/tree/master/inst/examples/shiny'> Click here </a> to view source")),
  
  br(),
  
  fluidRow(
    column(2,
      selectInput("x", "Choose an x variable", choices = nms, selected = nms[1]),
      selectInput("y", "Choose an y variable", choices = nms, selected = nms[3]),
      selectInput("col", "Choose an color variable", choices = nms, selected = nms[6]),
      selectInput("size", "Choose a size variable", choices = nms, selected = nms[5])
    ),
    column(5,
      plotOutput("ggplot", height = "300px")
    ),
    column(5, 
      animintOutput("animint")
    )
  )
    
))

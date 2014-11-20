shinyUI(fluidPage(
  
  titlePanel("Pirates"),
  
  h4(HTML("<a href='https://github.com/tdhock/animint/tree/master/inst/examples/shiny-pirates'> Click here </a> to view source")),
  
  br(),
  
  fluidRow(
    column(2,
      sliderInput("lat", "Set latitude range:",
                  min = min_lat, max = max_lat, value = c(min_lat, max_lat)),
      sliderInput("long", "Set longitude range:",
                  min = min_long, max = max_long, value = c(min_long, max_long))
    ),
    column(10,
      animintOutput("animint")
    )
  )
  
))

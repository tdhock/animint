library(shiny)
library(animint)

shinyServer(function(input, output) {
  
  getPlot <- reactive({
    ggplot(mapping = aes_string(x = input$x, y = input$y, 
                                color = input$col)) + 
      geom_point(data = mtcars)
  })
  
  output$ggplot <- renderPlot({
    print(getPlot())
  }, width = 300, height = 300)
  
  # renderAnimint() expects a list of ggplots and animint options
  output$animint <- renderAnimint({
    # unlike plotOutput, height/width is controlled with theme_animint()
    p <- getPlot() + theme_animint(height = 300, width = 300)
    list(plot = p)
  })
  
})

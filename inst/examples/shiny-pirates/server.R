shinyServer(function(input, output) {
  
  subsetDat <- function(dat) {
    reactive( {
      dat <- dat[input$lat[1] <= dat$lat & dat$lat <= input$lat[2], ]
      dat[input$long[1] <= dat$long & dat$long <= input$long[2], ]
    })()
  }
  
  plotWorld <- reactive({
    world <- subsetDat(world)
    pirates <- subsetDat(pirates)
    ggplot() + 
      geom_polygon(data = world, 
                   aes(x = long, y = lat, group = group), 
                   fill = "black", colour = "grey", alpha = I(0.1)) +
      geom_point(data = pirates, aes(x = long, y = lat, tooltip = Desc1,
                                     showSelected = year), color = "red", 
                                  alpha = I(0.3)) +
     xlab("") + ylab("") + #coord_fixed() + 
      theme_animint(height = 600, width = 800)
  })
  
  # renderAnimint() expects a list of ggplots and animint options
  output$animint <- renderAnimint({ 
    list(plot1 = plotWorld(),
         time = list(variable = "year", ms = 2000))
  })
  
})

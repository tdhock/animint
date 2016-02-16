library(shiny)
library(animint)
data(WorldBank)
WorldBank$literacy <- WorldBank[["15.to.25.yr.female.literacy"]]
WorldBank$latitude <- as.numeric(paste(WorldBank$latitude))
WorldBank$longitude <- as.numeric(paste(WorldBank$longitude))
not.na <- subset(WorldBank, !(is.na(life.expectancy) | is.na(fertility.rate)))
subset(not.na, is.na(not.na$population))
subset(not.na, country == "Kuwait" & 1991 <= year & year <= 1995)
not.na[not.na$country=="Kuwait", "population"] <- 1700000
years <- unique(not.na[, "year", drop=FALSE])
by.country <- split(not.na, not.na$country)
min.years <- do.call(rbind, lapply(by.country, subset, year == min(year)))
min.years$year <- 1958
is.discrete <- function(x){
  is.factor(x) || is.character(x) || is.logical(x)
}

shinyServer(function(input, output) {
  
  getViz <- reactive({
    BOTH <- function(df, top, side){
      data.frame(df,
                 top=factor(top, c(input$x, "Years")),
                 side=factor(side, c("Years", input$y)))
    }
    TS <- function(df)BOTH(df, "Years", input$y)
    SCATTER <- function(df)BOTH(df, input$x, input$y)
    TS2 <- function(df)BOTH(df, input$x, "Years")
    x <- not.na[[input$x]]
    y <- not.na[[input$y]]
    years$x <- (max(x)+min(x))/2
    years$y <- max(y)

    gg <-
      ggplot()+
        theme_bw()+
        theme(panel.margin=grid::unit(0, "lines"))+
        xlab("")+
        ylab("")+
        geom_tallrect(aes(xmin=year-1/2, xmax=year+1/2,
                          clickSelects=year),
                      data=TS(years), alpha=1/2)+
        theme_animint(width=1000, height=800)+
        geom_line(aes_string(
          x="year",
          y=input$y,
          group="country",
          colour=input$color,
          clickSelects="country"),
                  data=TS(not.na), size=4, alpha=3/5)+
        geom_point(aes_string(
          x="year",
          y=input$y,
          color=input$color,
          size=input$size,
          showSelected="country",
          clickSelects="country"),
                   data=TS(not.na))+
        geom_text(aes_string(
          x="year",
          y=input$y,
          colour=input$color,
          label="country",
          showSelected="country",
          clickSelects="country"),
                  data=TS(min.years), hjust=1)+
        geom_widerect(aes(ymin=year-1/2, ymax=year+1/2,
                          clickSelects=year),
                      data=TS2(years), alpha=1/2)+
        geom_path(aes_string(
          x=input$x,
          y="year",
          group="country",
          colour=input$color,
          clickSelects="country"),
                  data=TS2(not.na), size=4, alpha=3/5)+
        geom_point(aes_string(
          x=input$x,
          y="year",
          color=input$color,
          size=input$size,
          showSelected="country",
          clickSelects="country"),
                   data=TS2(not.na))+
        geom_point(aes_string(
          x=input$x,
          y=input$y,
          clickSelects="country",
          id="country",
          showSelected="year",
          colour=input$color,
          size=input$size,
          key="country"),
                   data=SCATTER(not.na))+
        geom_text(aes_string(
          x=input$x,
          y=input$y,
          label="country",
          showSelected="country",
          showSelected2="year",
          showSelected3=input$color,
          clickSelects="country",
          key="country"), 
                  data=SCATTER(not.na))+
        facet_grid(side ~ top, scales="free")+
        geom_text(aes(x, y,
          label=paste0("year = ", year),
          showSelected=year),
                  data=SCATTER(years))
      if(is.discrete(not.na[[input$size]])){
        gg <- gg+scale_size_discrete()
      }else{
        gg <- gg+scale_size_animint()
      }
    list(ts=gg,
         time=list(variable="year",ms=2000),
         duration=list(year=1000),
         first=list(year=1975, country=c("United States", "Vietnam")),
         selector.types=list(country="multiple"),
         title="World Bank data (multiple selection, facets)")
  })
  
  output$animint <- renderAnimint({
    # unlike plotOutput, height/width is controlled with theme_animint()
    getViz()
  })
  
})

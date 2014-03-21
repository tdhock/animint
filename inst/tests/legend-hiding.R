context("hiding legends")

data(WorldBank)
breaks <- 10^(4:9)
wbViz <-
  list(ts=ggplot()+
       make_tallrect(WorldBank, "year")+
       geom_line(aes(year, life.expectancy, group=country, colour=region,
                     clickSelects=country),
                 data=WorldBank, size=3, alpha=3/5),
       time=list(variable="year",ms=3000),
       duration=list(year=1000),
       scatter=ggplot()+
       geom_point(aes(fertility.rate, life.expectancy, clickSelects=country,
                      showSelected=year, colour=region, size=population),
                  data=WorldBank)+
       geom_text(aes(fertility.rate, life.expectancy, label=country,
                     showSelected=country, showSelected2=year),
                 data=WorldBank)+
       make_text(WorldBank, 5, 80, "year")+
       continuous_scale("size","area",palette=function(x){
         scales:::rescale(sqrt(abs(x)), c(2,20), c(0,1))
       },breaks=breaks))

test_that('hiding the color legend works with scale_color(guide="none")',{
  leg.name.list <- list(ts="region",
                        scatter=c("population", "region"))
  hidden.list <- list(ts=NULL,
                      scatter=c("population"))
  leg.none.list <- lapply(leg.name.list, function(x)c("legend", "none"))
  leg.none.df <- do.call(expand.grid, leg.none.list)
  for(row.i in 1:nrow(leg.none.df)){
    r <- leg.none.df[row.i,]
    noLeg <- wbViz
    for(plot.name in names(r)){
      noLeg[[plot.name]] <- noLeg[[plot.name]]+
        scale_color_discrete(guide=as.character(r[[plot.name]]))
    }
    info <- gg2animint(noLeg, open.browser=FALSE)
    for(plot.name in names(r)){
      expected.names <- if(r[[plot.name]] == "legend"){
        leg.name.list[[plot.name]]
      }else{
        hidden.list[[plot.name]]
      }
      generated.names <- names(info$plots[[plot.name]]$legend)
      expect_identical(generated.names, expected.names)
    }
  }
})

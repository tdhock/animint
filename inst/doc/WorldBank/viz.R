## Make a Gapminder plot (aka Google motion chart), which is actually
## just a scatterplot with size and color that moves over time.
data(WorldBank)
pop.range <- range(WorldBank$pop,na.rm=TRUE)
viz <-
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
         scales:::rescale(sqrt(abs(x)), c(2,20))
       },breaks=seq(pop.range[1], pop.range[2], l=5)))

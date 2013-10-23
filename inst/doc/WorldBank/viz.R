## Make a Gapminder plot (aka Google motion chart), which is actually
## just a scatterplot with size and color that moves over time.
data(WorldBank)
viz <-
  list(ts=ggplot()+
       make_tallrect(WorldBank, "year")+
       geom_line(aes(year, life.expectancy, group=country,
                     clickSelects=country),
                 data=WorldBank, size=4, alpha=3/5),
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
       },breaks=seq(5e8, 1e9, by=1e8)))

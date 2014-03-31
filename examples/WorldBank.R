library(animint)
data(WorldBank)

## M <- gvisMotionChart(WorldBank,
##                      idvar="country", timevar="year",
##                      xvar="life.expectancy", yvar="fertility.rate",
##                      colorvar="region", sizevar="population",
##                      options=list(width=700, height=600))

## A google motion chart is just a scatterplot with size and color
## that moves over time.
pop.range <- range(WorldBank$pop,na.rm=TRUE)
log.scale <- seq(ceiling(log10(pop.range[1])),floor(log10(pop.range[2])),by=1)
pop.breaks <- sort(c(10^log.scale))
motion <-
  list(scatter=ggplot()+
       geom_point(aes(life.expectancy, fertility.rate, clickSelects=country,
                      showSelected=year, colour=region, size=population),
                  data=WorldBank)+
       ##scale_size_area(max_size=40)+
       make_text(WorldBank, 55, 9, "year"),
       timeSeries=ggplot()+
       make_tallrect(WorldBank, "year")+
       geom_line(aes(year, life.expectancy, group=country,
                     clickSelects=country),
                 data=WorldBank, size=4, alpha=3/5),
       time=list(variable="year",ms=3000),
       duration=list(year=1000))
gg2animint(motion, "motion")

## Why can't we use range with scale_size_area? Because the number of
## pixels should be proportional to the number represented i.e. it is
## a linear, not affine function.
motion.area <-
  list(scatter=ggplot()+
       geom_point(aes(life.expectancy, fertility.rate, clickSelects=country,
                      showSelected=year, colour=region, size=population),
                  data=WorldBank)+
       geom_text(aes(life.expectancy, fertility.rate, label=country,
                     showSelected=country, showSelected2=year),
                 data=WorldBank)+
       continuous_scale("size","area",palette=function(x){
         scales:::rescale(sqrt(abs(x)), c(2,20), c(0,1))
       },breaks=10^(4:9))+
       make_text(WorldBank, 55, 9, "year"),
       ts=ggplot()+
       make_tallrect(WorldBank, "year")+
       geom_line(aes(year, life.expectancy, group=country, colour=region,
                     clickSelects=country),
                 data=WorldBank, size=4, alpha=3/5),
       time=list(variable="year",ms=3000),
       bar=ggplot()+
       geom_bar(aes(country, life.expectancy, fill=region,
                    showSelected=year, clickSelects=country),
                data=WorldBank, stat="identity", position="identity")+
       coord_flip(),
       duration=list(year=1000),
       height=list(bar=1500))
gg2animint(motion.area, "motion-area")

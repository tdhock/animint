library(animint)
data(WorldBank)

## M <- gvisMotionChart(WorldBank,
##                      idvar="country", timevar="year",
##                      xvar="life.expectancy", yvar="fertility.rate",
##                      colorvar="region", sizevar="population",
##                      options=list(width=700, height=600))

## This example is BAD because it does not specify a key variable, so
## the animated transitions result in points flying over the
## scatterplot, which is not good for understand the evolution over
## time of these countries' data.
not.na <- subset(WorldBank, !(is.na(life.expectancy) | is.na(fertility.rate)))
subset(not.na, is.na(not.na$population))
not.na[not.na$country=="Kuwait", "population"] <- 1700000
bad <-
  list(scatter=ggplot()+
       geom_point(aes(life.expectancy, fertility.rate, clickSelects=country,
                      showSelected=year, colour=region, size=population),
                  data=not.na)+
       geom_text(aes(life.expectancy, fertility.rate, label=country,
                     showSelected=country, showSelected2=year),
                 data=not.na)+
       scale_size_animint(breaks=10^(5:9))+
       make_text(WorldBank, 55, 9, "year"),
       ts=ggplot()+
       make_tallrect(WorldBank, "year")+
       geom_line(aes(year, life.expectancy, group=country, colour=region,
                     clickSelects=country),
                 data=WorldBank, size=4, alpha=3/5),
       time=list(variable="year",ms=3000),
       bar=ggplot()+
       theme_animint(height=2400)+
       geom_bar(aes(country, life.expectancy, fill=region,
                    showSelected=year, clickSelects=country),
                data=WorldBank, stat="identity", position="identity")+
       coord_flip(),
       duration=list(year=1000))
animint2dir(bad, "WorldBank-bad")

## This example is good because it uses constancy
## http://bost.ocks.org/mike/constancy/
good <-
  list(scatter=ggplot()+
       geom_point(aes(life.expectancy, fertility.rate, clickSelects=country,
                      showSelected=year, colour=region, size=population,
                      key=country), # key aesthetic for animated transitions!
                  data=not.na)+
       geom_text(aes(life.expectancy, fertility.rate, label=country,
                     showSelected=country, showSelected2=year,
                     key=country), #also use key here!
                 data=not.na)+
       scale_size_animint(breaks=10^(5:9))+
       make_text(WorldBank, 55, 9, "year"),
       ts=ggplot()+
       make_tallrect(WorldBank, "year")+
       geom_line(aes(year, life.expectancy, group=country, colour=region,
                     clickSelects=country),
                 data=WorldBank, size=4, alpha=3/5),
       time=list(variable="year",ms=3000),
       bar=ggplot()+
       theme_animint(height=2400)+
       geom_bar(aes(country, life.expectancy, fill=region,
                    showSelected=year, clickSelects=country),
                data=WorldBank, stat="identity", position="identity")+
       coord_flip(),
       duration=list(year=1000),
       first=list(year=1975, country="United States"),
       title="World Bank data (single selection)")
animint2dir(good, "WorldBank-good")

## This example additionally uses multiple selection on countries.
good <-
  list(scatter=ggplot()+
       geom_point(aes(life.expectancy, fertility.rate, clickSelects=country,
                      showSelected=year, colour=region, size=population,
                      key=country), # key aesthetic for animated transitions!
                  data=not.na)+
       geom_text(aes(life.expectancy, fertility.rate, label=country,
                     showSelected=country, showSelected2=year,
                     key=country), #also use key here!
                 data=not.na)+
       scale_size_animint(breaks=10^(5:9))+
       make_text(WorldBank, 55, 9, "year"),
       ts=ggplot()+
       make_tallrect(WorldBank, "year")+
       geom_line(aes(year, life.expectancy, group=country, colour=region,
                     clickSelects=country),
                 data=WorldBank, size=4, alpha=3/5),
       time=list(variable="year",ms=3000),
       bar=ggplot()+
       theme_animint(height=2400)+
       geom_bar(aes(country, life.expectancy, fill=region,
                    showSelected=year, clickSelects=country),
                data=WorldBank, stat="identity", position="identity")+
       coord_flip(),
       duration=list(year=1000),
       first=list(year=1975, country="United States"),
       selector.types=list(country="multiple"),
       title="World Bank data (multiple selection)")
animint2dir(good, "WorldBank-good")

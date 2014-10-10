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
library(dplyr)
max.years <- not.na %.%
  group_by(country) %.%
  filter(year==max(year)) %.%
  mutate(year=2012)
wb.mult <-
  list(ts=ggplot()+
       make_tallrect(not.na, "year")+
       theme_animint(width=500)+
       geom_line(aes(year, life.expectancy, group=country, colour=region,
                     clickSelects=country),
                 data=not.na, size=4, alpha=3/5)+
       geom_point(aes(year, life.expectancy, color=region,
                      showSelected=country, clickSelects=country),
                  data=not.na)+
       scale_x_continuous(limits=c(1960, 2030), breaks=seq(1960, 2010, by=10))+
       geom_text(aes(year, life.expectancy, colour=region, label=country,
                     showSelected=country,
                     clickSelects=country),
                  data=max.years, hjust=0),
       scatter=ggplot()+
       geom_point(aes(fertility.rate, life.expectancy, clickSelects=country,
                      showSelected=year, colour=region, size=population,
                      key=country), # key aesthetic for animated transitions!
                  data=not.na)+
       geom_text(aes(fertility.rate, life.expectancy, label=country,
                     showSelected=country, showSelected2=year,
                     clickSelects=country,
                     key=country), #also use key here!
                 data=not.na)+
       scale_size_animint(breaks=10^(5:9))+
       make_text(not.na, 5, 85, "year"),
       time=list(variable="year",ms=3000),
       bar=ggplot()+
       theme_animint(height=2400)+
       geom_bar(aes(country, life.expectancy, fill=region,
                    showSelected=year, clickSelects=country,
                    key=country),
                data=not.na, stat="identity", position="identity")+
       coord_flip(),
       duration=list(year=1000),
       first=list(year=1975, country=c("United States", "Vietnam")),
       selector.types=list(country="multiple"),
       title="World Bank data (multiple selection)")
animint2dir(wb.mult, "WorldBank-multiple")

BOTH <- function(df, top, side){
  data.frame(df,
             top=factor(top, c("Fertility rate", "Years")),
             side=factor(side, c("Years", "Life expectancy")))
}
TS <- function(df)BOTH(df, "Years", "Life expectancy")
SCATTER <- function(df)BOTH(df, "Fertility rate", "Life expectancy")
TS2 <- function(df)BOTH(df, "Fertility rate", "Years")
years <- unique(not.na[, "year", drop=FALSE])
min.years <- not.na %.%
  group_by(country) %.%
  filter(year==min(year)) %.%
  mutate(year=1958)
wb.facets <-
  list(ts=ggplot()+
       xlab("")+
       ylab("")+
       geom_tallrect(aes(xmin=year-1/2, xmax=year+1/2,
                         clickSelects=year),
                     data=TS(years), alpha=1/2)+
       theme_animint(width=1000, height=800)+
       geom_line(aes(year, life.expectancy, group=country, colour=region,
                     clickSelects=country),
                 data=TS(not.na), size=4, alpha=3/5)+
       geom_point(aes(year, life.expectancy, color=region, size=population,
                      showSelected=country, clickSelects=country),
                  data=TS(not.na))+
       geom_text(aes(year, life.expectancy, colour=region, label=country,
                     showSelected=country,
                     clickSelects=country),
                  data=TS(min.years), hjust=1)+
       
       geom_line(aes(fertility.rate, year, group=country, colour=region,
                     clickSelects=country),
                 data=TS2(not.na), size=4, alpha=3/5)+
       geom_point(aes(fertility.rate, year, color=region, size=population,
                      showSelected=country, clickSelects=country),
                  data=TS2(not.na))+
       geom_widerect(aes(ymin=year-1/2, ymax=year+1/2,
                         clickSelects=year),
                     data=TS2(years), alpha=1/2)+
       
       geom_point(aes(fertility.rate, life.expectancy, clickSelects=country,
                      showSelected=year, colour=region, size=population,
                      key=country), # key aesthetic for animated transitions!
                  data=SCATTER(not.na))+
       geom_text(aes(fertility.rate, life.expectancy, label=country,
                     showSelected=country, showSelected2=year,
                     clickSelects=country,
                     key=country), #also use key here!
                 data=SCATTER(not.na))+
       scale_size_animint(breaks=10^(5:9))+
       facet_grid(side ~ top, scales="free")+
       geom_text(aes(5, 85, label=paste0("year = ", year),
                     showSelected=year),
                 data=SCATTER(years)),
       time=list(variable="year",ms=3000),
       bar=ggplot()+
       theme_animint(height=2400)+
       geom_bar(aes(country, life.expectancy, fill=region,
                    showSelected=year, clickSelects=country,
                    key=country),
                data=not.na, stat="identity", position="identity")+
       coord_flip(),
       duration=list(year=1000),
       first=list(year=1975, country=c("United States", "Vietnam")),
       selector.types=list(country="multiple"),
       title="World Bank data (multiple selection, facets)")
animint2dir(wb.facets, "WorldBank-facets")

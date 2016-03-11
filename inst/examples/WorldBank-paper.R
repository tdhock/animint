library(animint)
data(WorldBank)
WorldBank$region <-
  sub(" (all income levels)", "", WorldBank$region, fixed=TRUE)
wb.paper <-
  list(ts=ggplot()+
       make_tallrect(WorldBank, "year")+
       guides(color="none")+
       geom_line(aes(year, life.expectancy, group=country, colour=region,
                     showSelected=region,
                     clickSelects=country),
                 data=WorldBank, size=4, alpha=3/5),
       scatter=ggplot()+
       geom_point(aes(fertility.rate, life.expectancy, clickSelects=country,
                      showSelected=year, colour=region, size=population,
                      tooltip=paste(country, "population", population),
                      key=country), # key aesthetic for animated transitions!
                  data=WorldBank)+
       geom_text(aes(fertility.rate, life.expectancy, label=country,
                     clickSelects=country,
                     showSelected=country, showSelected2=year,
                     showSelected3=region,
                     key=country), #also use key here!
                 data=WorldBank)+
       scale_size_animint(breaks=10^(9:5))+
       make_text(WorldBank, 5, 80, "year"),
       time=list(variable="year",ms=3000),
       duration=list(year=1000),
       selector.types=list(country="multiple", region="multiple"),
       first=list(
         year=1979,
         country=c("United States", "Vietnam"),
         region=c("East Asia & Pacific", "North America")
         ),
       title="World Bank data (Animint paper version)")
animint2dir(wb.paper, "WorldBank-paper")


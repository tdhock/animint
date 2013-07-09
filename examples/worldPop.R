library(animint)
data(worldPop)
years <- unique(worldPop[,"year",drop=FALSE])
popPlots <-
  list(bars=ggplot()+
       geom_bar(aes(subcontinent, population,
                    clickSelects=subcontinent, showSelected=year),
                data=worldPop, stat="identity")+
       coord_flip(),
       lines=ggplot()+
       geom_vline(aes(xintercept=year, clickSelects=year),
                  data=years, alpha=1/2, size=12)+
       geom_line(aes(year, population, group=subcontinent,
                     clickSelects=subcontinent),
                 data=worldPop, alpha=3/4, size=4))
gg2animint(popPlots)

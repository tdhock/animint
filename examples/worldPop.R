library(animint)
data(worldPop)
levs <- levels(worldPop$subcontinent)
subcont.means <- sapply(levs,function(l)mean(worldPop$pop[worldPop$sub==l]))
worldPop$subcontinent <- factor(worldPop$sub, levs[order(subcont.means)])
years <- unique(worldPop[,"year",drop=FALSE])
years$title <- factor(sprintf("Population in %d", years$year))
years$subcontinent <- factor(levels(worldPop$sub)[1])
years$population <- 4e7
## TODO: this should be similar to the example on polychartjs.com
popPlots <-
  list(bars=ggplot()+
       geom_bar(aes(subcontinent, population,
                    clickSelects=subcontinent, showSelected=year),
                data=worldPop, stat="identity")+
       geom_text(aes(subcontinent, population,
                     label=title, showSelected=year),
                 data=years)+
       coord_flip(),
       lines=ggplot()+
       geom_vline(aes(xintercept=year, clickSelects=year),
                  data=years, alpha=1/2, size=12)+
       geom_line(aes(year, population, group=subcontinent,
                     clickSelects=subcontinent),
                 data=worldPop, alpha=3/4, size=4))
gg2animint(popPlots)

## TODO: we should at least see the bars in this simpler test.
onebar <- ggplot()+
  geom_bar(aes(subcontinent), data=worldPop)
gg2animint(list(bar=onebar))

## Population barplots broken down by year.
library(grid)
popPlots$bars+
  facet_wrap("year")+
  theme_bw()+
  theme(panel.margin=unit(0,"cm"))

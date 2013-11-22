data(worldPop)
## Linked bar and line plots of world population by subcontinent,
## inspired by polychartjs.
popPlots <-
  list(bars=ggplot()+
       geom_bar(aes(x=subcontinent, y=population,
                    clickSelects=subcontinent, showSelected=year),
                data=worldPop, stat="identity", position="identity")+
       ## This make_text creates a geom_text that shows the current
       ## selected value of the year variable.
       make_text(worldPop, 1, 3e6, "year")+
       coord_flip(),
       lines=ggplot()+
       ## This make_tallrect tiles the background of the lineplot with
       ## rects that can be clicked to select the year variable.
       make_tallrect(worldPop, "year")+
       ## This geom_point does not have aes(clickSelects) so its alpha
       ## transparency behaves normally: all points have alpha=1/4.
       geom_point(aes(year, population, colour=type),
                  data=worldPop, size=4, alpha=1/4)+
       ## This geom_line DOES have aes(clickSelects) so only the
       ## selected line has the specified alpha=3/4. The other
       ## unselected lines have 0.5 less (alpha=1/4).
       geom_line(aes(year, population, group=subcontinent,
                     clickSelects=subcontinent),
                 data=worldPop, size=4, alpha=3/4))
gg2animint(popPlots)
## Make it animated by specifying year as the variable to animate and
## an interval of 2000 milliseconds between animation frames.
popAnim <- c(popPlots, list(time=list(variable="year",ms=2000)))
gg2animint(popAnim)
## Make the animation smooth by specifying a duration of 1000 ms for
## geoms with aes(showSelected=year).
popSmooth <- c(popAnim, list(duration=list(year=1000)))
gg2animint(popSmooth)

## Make a Gapminder plot (aka Google motion chart), which is actually
## just a scatterplot with size and color that moves over time.
data(WorldBank)
gapminder <-
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
         scales:::rescale(sqrt(abs(x)), c(2,20), c(0,1))
       },breaks=10^(4:9)))
gg2animint(gapminder)

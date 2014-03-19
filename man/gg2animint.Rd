\name{gg2animint}
\alias{animint}
\alias{gg2animint}
\title{gg2animint}
\usage{
gg2animint(plot.list, out.dir = tempfile(), open.browser = interactive())
}
\arguments{
  \item{plot.list}{a named list of ggplots and option
  lists.}

  \item{out.dir}{directory to store html/js/csv files.}

  \item{open.browser}{Should R open a browser? If yes, be
  sure to configure your browser to allow access to local
  files, as some browsers block this by default (e.g.
  chrome).}
}
\value{
invisible list of ggplots in list format.
}
\description{
Convert a list of ggplots to an interactive animation that
can be viewed in a web browser. Two new aesthetics control
interactivity: \itemize{ \item
\code{aes(showSelected=variable)} means that only the
subset of the data that corresponds to the selected value
of variable will be shown. \item
\code{aes(clickSelects=variable)} means that clicking this
geom will change the currently selected value of variable.
}
}
\details{
Supported ggplot2 geoms: \itemize{ \item point \item jitter
\item line \item rect \item tallrect (new with this
package) \item segment \item hline \item vline \item bar
\item text \item tile \item raster \item ribbon \item
abline \item density \item path \item polygon \item
histogram \item violin \item linerange \item step \item
contour \item density2d \item area \item freqpoly \item hex
} Unsupported geoms: \itemize{ \item rug \item dotplot
\item quantile - should *theoretically* work but in
practice does not work \item smooth - can be created using
geom_line and geom_ribbon \item boxplot - can be created
using geom_rect and geom_segment \item crossbar - can be
created using geom_rect and geom_segment \item pointrange -
can be created using geom_linerange and geom_point \item
bin2d - bin using ddply() and then use geom_tile() \item
map - can be created using geom_polygon or geom_path }
Supported scales: \itemize{ \item alpha, \item fill/colour
(brewer, gradient, identity, manual) \item linetype \item x
and y axis scales, manual break specification, label
formatting \item x and y axis theme elements: axis.line,
axis.ticks, axis.text, axis.title can be set to
element_blank(); other theme modifications not supported at
this time, but would be possible with custom css files.
\item area \item size } Unsupported scales: \itemize{ \item
shape. Open and closed circles can be represented by
manipulating fill and colour scales and using default
(circle) points, but d3 does not support many R shape
types, so mapping between the two is difficult. }
}
\examples{
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
       scale_size_animint(pixel.range=c(2,20), breaks=10^(4:9)))
gg2animint(gapminder)
}
\seealso{
\code{\link{ggplot2}}
}


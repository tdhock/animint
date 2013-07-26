\name{make_tallrect}
\alias{make_tallrect}
\title{Make a clickSelects geom_tallrect that completely tiles the x
range. This makes it easy to construct tallrects for the common
case of selecting a particular x value.}
\usage{
  make_tallrect(data, x.name, alpha = 1/2)
}
\arguments{
  \item{data}{data.frame to analyze for unique x.name
  values.}

  \item{x.name}{variable to be used for x, clickSelects.}

  \item{alpha}{transparency of a selected tallrect, default
  1/2.}
}
\value{
  a geom_tallrect layer.
}
\description{
  Make a clickSelects geom_tallrect that completely tiles
  the x range. This makes it easy to construct tallrects
  for the common case of selecting a particular x value.
}
\examples{
data(worldPop)
popPlot <- ggplot()+
  make_tallrect("year", worldPop)+
  geom_line(aes(year, population, group=subcontinent),
            data=worldPop, size=4)
print(popPlot)
gg2animint(list(popPlot=popPlot))
}
\author{
  Toby Dylan Hocking
}


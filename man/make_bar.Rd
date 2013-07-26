\name{make_bar}
\alias{make_bar}
\title{Convenience function for an interactive bar that might otherwise be created using stat_bin.}
\usage{
  make_bar(data, x.name, alpha = 1)
}
\arguments{
  \item{x.name}{variable to be used for x, clickSelects.}

  \item{data}{data.frame to analyze for unique x.name
  values.}

  \item{alpha}{transparency of selected bar, default 1.}
}
\value{
  a geom_bar layer.
}
\description{
  Convenience function for an interactive bar that might
  otherwise be created using stat_bin.
}
\examples{
data(Tornadoes)
tornado.bar <-
list(map=ggplot()+
       geom_polygon(aes(x=long, y=lat, group=group),
                    data=USpolygons, fill="black", colour="grey") +
       geom_segment(aes(x=startLong, y=startLat, xend=endLong, yend=endLat,
                        showSelected=year),
                    colour="#55B1F7", data=UStornadoes),
     ts=ggplot()+
       make_bar(UStornadoes, "year"))
gg2animint(tornado.bar)
}
\author{
  Toby Dylan Hocking
}


\name{make_text}
\alias{make_text}
\title{Convenvience function for a showSelected plot label.}
\usage{
  make_text(data, x, y, label.var, format = NULL)
}
\arguments{
  \item{data}{data.frame of relevant data}

  \item{x}{x coordinate of label position}

  \item{y}{y coordinate of label position}

  \item{label.var}{variable matching showSelected, used to
  obtain label value}

  \item{format}{String format for label. Use %d, %f, etc.
  to insert relevant label.var value.}
}
\value{
  a geom_text layer.
}
\description{
  Convenvience function for a showSelected plot label.
}
\examples{
tornado.ts.bar <-
  list(map=ggplot()+
         make_text(UStornadoCounts, -100, 50, "year", "Tornadoes in \%d")+
         geom_polygon(aes(x=long, y=lat, group=group, clickSelects=state),
                      data=USpolygons, fill="black", colour="grey") +
         geom_segment(aes(x=startLong, y=startLat, xend=endLong, yend=endLat,
                          showSelected=year),
                      colour="#55B1F7", data=UStornadoes),
       ts=ggplot()+
         make_text(UStornadoes, 1980, 200, "state")+
         geom_bar(aes(year, count, clickSelects=year, showSelected=state),
                  data=UStornadoCounts, stat="identity", position="identity"))
gg2animint(tornado.ts.bar, "tornado-ts-bar")
}
\author{
  Toby Dylan Hocking
}


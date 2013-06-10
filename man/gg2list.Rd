\name{gg2list}
\alias{gg2list}
\title{Convert a ggplot to a list. Called from gg2animint().}
\usage{
  gg2list(p)
}
\arguments{
  \item{p}{ggplot2 plot}
}
\value{
  list representing a ggplot, with elements, ranges, axes,
  scales, geoms, and options
}
\description{
  Convert a ggplot to a list. Called from gg2animint().
}
\examples{
gg2list(ggplot() + geom_point(data=data.frame(x=rnorm(100, 3, 1), y=rnorm(100, 5, 1))), aes(x=x, y=y))
}
\seealso{
  \code{\link{gg2animint}}
}


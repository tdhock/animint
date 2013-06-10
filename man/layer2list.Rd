\name{layer2list}
\alias{layer2list}
\title{Convert a layer to a list. Called from gg2list()}
\usage{
  layer2list(p, i, plistextra)
}
\arguments{
  \item{p}{ggplot2 plot}

  \item{i}{index of layer, in order of call.}

  \item{plistextra}{output from ggplot2::ggplot_build(p)}
}
\value{
  list representing a layer, with corresponding aesthetics,
  ranges, and groups.
}
\description{
  Convert a layer to a list. Called from gg2list()
}
\seealso{
  \code{\link{gg2animint}}
}


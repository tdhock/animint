\name{worldPop}
\alias{worldPop}
\docType{data}
\title{
World population by subcontinent
}
\description{
World population data are used as a simple example on the polychart.js
website, and so these data can be used to recreate that example using animint.
}
\usage{data(worldPop)}
\format{
  A data frame with 294 observations on the following 4 variables.
  \describe{
    \item{\code{subcontinent}}{factor: the subcontinent name}
    \item{\code{year}}{integer: year of measurement}
    \item{\code{population}}{integer: number of people in that
      subcontinent during that year}
    \item{\code{type}}{factor with levels \code{actual} \code{estimate}}
  }
}
\source{
https://github.com/Polychart/polychart2/blob/master/example/population.coffee
}
\keyword{datasets}

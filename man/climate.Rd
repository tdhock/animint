\name{climate}
\alias{climate}
\docType{data}
\title{
  Climate data in central America from 1995 to 2000
}
\description{
Monthly observations of atmospheric variables 1995-2000, 
between 113.75W-56.25W, 21.25S-36.25N with 2.5 degree grid
spacing. Variables: pressure, temperature, ozone and low, medium, 
high cloud cover. 
Temperatures are given in degrees celsius (original data had Kelvin).
}
\usage{data(climate)}
\format{
  A data frame with 41472 observations on the following 16 variables.
  \describe{
    \item{\code{time}}{a numeric vector}
    \item{\code{y}}{a numeric vector}
    \item{\code{x}}{a numeric vector}
    \item{\code{lat}}{a numeric vector}
    \item{\code{long}}{a numeric vector}
    \item{\code{date}}{a POSIXct}
    \item{\code{cloudhigh}}{a numeric vector}
    \item{\code{cloudlow}}{a numeric vector}
    \item{\code{cloudmid}}{a numeric vector}
    \item{\code{ozone}}{a numeric vector}
    \item{\code{pressure}}{a numeric vector}
    \item{\code{surftemp}}{a numeric vector}
    \item{\code{temperature}}{a numeric vector}
    \item{\code{id}}{a character vector}
    \item{\code{month}}{a numeric vector}
    \item{\code{year}}{a numeric vector}
  }
}
\source{
NASA Goddard Institute for Space Studies (GISS)
subset of the monthly climatology of the International 
Satellite Cloud Climatology Project (ISCCP)
}
\keyword{datasets}

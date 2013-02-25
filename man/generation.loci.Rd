\name{generation.loci}
\alias{generation.loci}
\docType{data}
\title{
Evolution simulation
}
\description{
  Allele frequencies for 100 loci and 12 populations were simulated
  over 100 generations.
}
\usage{data(generation.loci)}
\format{
  A data frame with 120000 observations on the following 4 variables.
  \describe{
    \item{\code{locus}}{a numeric vector}
    \item{\code{population}}{a numeric vector}
    \item{\code{generation}}{a numeric vector}
    \item{\code{frequency}}{a numeric vector}
  }
}
\source{
Data generated using nicholsonppp::sim.drift.selection, see code in
etc/generation.loci.R.
}
\keyword{datasets}

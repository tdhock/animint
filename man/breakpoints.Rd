\name{breakpoints}
\alias{breakpoints}
\docType{data}
\title{
  The breakpointError of simulated signals
}
\description{
  Two noisy signals were sampled from a latent signal with known
  breakpoints, which were used to measure the error of estimated
  models with 1,...,20 segments.
}
\usage{data(breakpoints)}
\format{
A list of 5 data.frames: error contains the breakpointError of the
estimated models, signals contains the noisy signals, breaks contains
the breakpoints in the estimated signals, segments contains the
estimated segments, and imprecision contains the normalized imprecision
curves which were used to evaluate the error.
}
\source{
The breakpointError package was used to measure the model error, see
etc/breakpoints.R. 
}
\keyword{datasets}

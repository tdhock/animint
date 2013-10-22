\name{change}
\alias{change}
\docType{data}
\title{
Graphical model structure change
}
\description{
A model for detecting graphical model structure changes was applied to
some simulated data for which we know the real changes.
}
\usage{data(change)}
\format{
List of 4 data.frames: coefs contains the coefficients for every
variable at every point in the regularization path,
truth contains information about which
variables are real changes, likelihood contains the train and test
likelihood, and position contains positions for laying out the graphical
model nodes on the 2D plane.
}
\source{Song Liu, personal communication.}
\references{
  Liu S, Quinn JA, Gutmann MU, Sugiyama M. Direct Learning of Sparse
  Changes in Markov Networks by Density Ratio Estimation. ECML/PKDD
  2013. 
}
\keyword{datasets}

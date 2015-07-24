\name{intreg}
\alias{intreg}
\docType{data}
\title{
Interval regression
}
\description{
Learning model complexity using max-margin interval regression. We have
observed several noisy piecewise constant signals, and we have
weak labels about how many change-points occur in several regions. Max
margin interval regression is an algorithm that uses this information to
learn a penalty function for accurate change-point detection.
}
\usage{data(intreg)}
\format{There are 7 related data.frames: signals contains the noisy
  piecewise constant signals, annotations contains the weak labels,
  segments and breaks contain the segmentation model, selection contains
  the penalty and cost information, intervals contains the target
  intervals of penalty values for each signal, and model describes the
  learned max margin interval regression model.
}
\keyword{datasets}

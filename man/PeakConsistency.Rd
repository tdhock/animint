\name{PeakConsistency}
\alias{PeakConsistency}
\docType{data}
\title{
  Consistency of segmentation models in simulated Poisson peaks
}
\description{
  For each of 4 seed and ten samples, a sequence of noisy Poisson data
  was generated, and then PeakSeg and PeakSegJoint were used to detect
  the peak.
}
\usage{data("PeakConsistency")}
\format{A list of four data.frames: model, truth, signal, guess.
}
\source{
  https://github.com/tdhock/PeakSegJoint-paper/blob/master/figure-consistency.R
}
\keyword{datasets}

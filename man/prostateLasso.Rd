\name{prostateLasso}
\alias{prostateLasso}
\docType{data}
\title{
  Lasso model of the prostate cancer data set
}
\description{
  The lars package was used to obtain a full path of lasso solutions.
}
\usage{data("prostateLasso")}
\format{
  A list of 4 data.frames: path for the piecewise linear coefficient
  path, residuals for the prediction error of every model at every data
  point, models with one row per regularization parameter, error for
  total train/validation prediction error for each regularization parameter.
}
\source{
  https://github.com/tdhock/animint-book/blob/master/Ch11-lasso.Rmd
}
\keyword{datasets}

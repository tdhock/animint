\name{VariantModels}
\alias{VariantModels}
\docType{data}
\title{
  Error rates of supervised learning methods for variant calling
}
\description{
  Several supervised machine learning models were applied to the binary
  classification task of predicting True Positive or False Positive
  variants, using several filtering scores as input.
}
\usage{data("VariantModels")}
\format{List of 6 data.frames: roc, auc, error, ranks, thresholds, minima.}
\source{
  https://github.com/tdhock/malaria-evolution-viz/blob/master/figure-folds.R
}
\keyword{datasets}

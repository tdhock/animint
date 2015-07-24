\name{compare}
\alias{compare}
\docType{data}
\title{
Testing rank and compare SVM on simulated patterns
}
\description{
50, ..., 800 labeled pairs were generated according to the squared L1,
L2, and Linf norms, with input features in [-3, 3]. Labels are -1 when
there is no significant difference between the ranks of Xt and Xtp,
and 1 when the rank of Xtp is significantly higher than the rank of
Xt. SVMrank ignores the pairs labeled -1, but SVMcompare uses them to
learn a more accurate predictive model, as shown by the test error and
level curves of the learned functions.
}
\usage{data(compare)}
\format{
List of 4 data.frames: error contains the test error of the learned
models, bayes contains the Bayes classification error of the latent
ranking function applied to the test data, rank contains the ranking
functions evaluated on a grid of input points, train.pairs contains the
data that were used to train the algorithms.
}
\references{
Hocking TD, Spanurattana S, Sugiyama M. Support vector comparison
machines (2013).
}
\keyword{datasets}

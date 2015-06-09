\name{saveChunks}
\alias{saveChunks}
\title{Split data set into chunks and save them to separate files.}
\usage{
saveChunks(x, vars, meta)
}
\arguments{
  \item{x}{data.frame.}

  \item{vars}{character vector of variable names to split
  on.}

  \item{meta}{environment.}
}
\value{
recursive list of chunk file names.
}
\description{
Split data set into chunks and save them to separate files.
}
\author{
Toby Dylan Hocking
}


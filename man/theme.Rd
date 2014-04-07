\name{theme}
\alias{theme}
\title{Theme without checks. This allows us to write
theme(animint.width=500), which would give an error under standard
ggplot2.}
\usage{
theme(..., complete = FALSE)
}
\arguments{
  \item{...}{theme options.}

  \item{complete}{TRUE or FALSE.}
}
\value{
ggplot theme list.
}
\description{
Theme without checks. This allows us to write
theme(animint.width=500), which would give an error under
standard ggplot2.
}
\author{
Toby Dylan Hocking
}


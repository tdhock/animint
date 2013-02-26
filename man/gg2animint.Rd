\name{gg2animint}
\alias{gg2animint}
\title{gg2animint}
\description{Convert a list of ggplots to an interactive animation.}
\usage{gg2animint(plot.list, out.dir = tempfile(), open.browser = interactive())}
\arguments{
  \item{plot.list}{List of ggplots with showSelected, time, and/or clickSelects aes.}
  \item{out.dir}{Directory to store html/js/csv files.}
  \item{open.browser}{Open a web browser?}
}

\value{The R representation of the exported JSON, so we can easily do
checks.}

\author{Toby Dylan Hocking}






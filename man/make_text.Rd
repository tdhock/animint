\name{make_text}
\alias{make_text}
\title{Convenvience function for a showSelected plot label.}
\usage{
  make_text(data, x, y, label.var, format = NULL)
}
\arguments{
  \item{data}{data.frame of relevant data}

  \item{x}{x coordinate of label position}

  \item{y}{y coordinate of label position}

  \item{label.var}{variable matching showSelected, used to
  obtain label value}

  \item{format}{String format for label. Use %d, %f, etc.
  to insert relevant label.var value.}
}
\value{
  a geom_text layer.
}
\description{
  Convenvience function for a showSelected plot label.
}
\author{
  Toby Dylan Hocking
}


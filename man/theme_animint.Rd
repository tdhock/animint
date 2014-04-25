\name{theme_animint}
\alias{theme_animint}
\title{Theme without checks. This allows us to write
theme_animint(width=500), instead of theme(animint.width=500)
which gives an error in ggplot2 because users should be informed
if they mis-type standard theme element
names. https://github.com/hadley/ggplot2/issues/938}
\usage{
theme_animint(...)
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
theme_animint(width=500), instead of
theme(animint.width=500) which gives an error in ggplot2
because users should be informed if they mis-type standard
theme element names.
https://github.com/hadley/ggplot2/issues/938
}
\author{
Toby Dylan Hocking
}


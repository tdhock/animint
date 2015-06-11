\name{animint2gist}
\alias{animint2gist}
\title{Convert a list of ggplots to an interactive animation and post files as a gist}
\usage{
animint2gist(plot.list, description = plot.list$title, browse = TRUE, ...)
}
\arguments{
  \item{plot.list}{a named list of ggplots and option
  lists.} \item{description}{Brief description of gist.
  This becomes the plot title on the bl.ocks/username page.}
  \item{browse}{logical. Prompt browser to view viz on
  \url{http://bl.ocks.org}} \item{...}{options passed onto
  \code{animint2dir} and \code{gistr::gist_create}}
}
\description{
Before using this function set your appropriate
'github.username' and 'github.password' \link{options}
}
\examples{
\dontrun{
library(animint)
iris$id <- 1:nrow(iris)
viz <- list(petal=ggplot()+
         geom_point(aes(Petal.Width, Petal.Length, fill=Species,
                        clickSelects=id), data=iris),
       sepal=ggplot()+
         geom_point(aes(Sepal.Width, Sepal.Length, fill=Species,
                        clickSelects=id), data=iris))
animint2gist(viz, description = "My animint plot")
}
}


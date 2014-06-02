\name{animint2gist}
\alias{animint2gist}
\title{Convert a list of ggplots to an interactive animation and post files as a gist}
\usage{
animint2gist(plot.list, out.dir = tempfile(), json.file = "plot.json", ...,
  domain = paste("http://bl.ocks.org", getOption("github.username"), "raw",
  sep = "/"))
}
\arguments{
  \item{plot.list}{a named list of ggplots and option
  lists.}

  \item{out.dir}{local directory to store html/js/csv
  files.}

  \item{json.file}{character string that names the JSON
  file with metadata associated with the plot.}

  \item{domain}{website domain for viewing result. Browser
  is prompted only if there is \link{httr::url_success}.}

  \item{...}{options passed onto \link{gistr::gist}}
}
\description{
Before using this function set your appropriate
'github.username' and 'github.password' \link{options}
}
\examples{
\dontrun{
devtools::install_github("rOpenSci/gistr")
options(github.username = "foo", github.password = "bar")
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


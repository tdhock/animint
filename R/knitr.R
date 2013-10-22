##' Insert an interactive animation into an R markdown document.
##' @param plot.list named list of ggplots and option lists to pass to
##' gg2animint.
##' @return Nothing, but outputs HTML that will render the interactive
##' animation.
##' @author Toby Dylan Hocking
##' @export
gg2animint_knitr <- function(plot.list){
  gg2animint(plot.list, ".", FALSE)
  html <- system.file(file.path("htmljs","scripts.html"), package="animint")
  slines <- readLines(html)
  cat(slines,sep="\n")
}

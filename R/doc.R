##' Make documentation web site.
##' @param path containing example subdirectories.
##' @return nothing.
##' @author Toby Dylan Hocking
##' @export
makeDocs <- function(doc.dir){
  require(knitr)
  sub.dirs <- dir(doc.dir, full.names=TRUE)
  Rmd.tmp.file <- system.file("template.Rmd", package="animint")
  Rmd.tmp.lines <- readLines(Rmd.tmp.file)
  Rmd.template <- paste(Rmd.tmp.lines, collapse="\n")
  owd <- getwd()
  on.exit(setwd(owd))
  for(sub.dir in sub.dirs){
    setwd(sub.dir)
    vizLines <- readLines("viz.R")
    vizCode <- paste(vizLines, collapse="\n")
    Rmd.filled <- sub("VIZ", vizCode, Rmd.template)
    writeLines(Rmd.filled, "viz.Rmd")
    knit2html("viz.Rmd")
    setwd(owd)
  }
}

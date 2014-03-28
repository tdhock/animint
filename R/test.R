## Define code required for testing.

##' Run gg2animint and then applyJS.
##' @param gg.opt.list 
##' @return HTML generated after evaluation of JavaScript.
##' @author Toby Dylan Hocking
animint2HTML <- function(gg.opt.list){
  out.dir <- tempdir()
  gg2animint(gg.opt.list, out.dir=out.dir, open.browser=FALSE)
  html.file <- file.path(out.dir, "index.html")
  applyJS(html.file)
}

applyJS <- function(html.file){
  "TODO: figure out how to run the JavaScript in html.file outside a web browser"
}

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


#' Insert an interactive animation into an R markdown document using a customized print method.
#' @param x named list of ggplots and option lists to pass to gg2animint.
#' @references https://github.com/yihui/knitr/blob/master/vignettes/knit_print.Rmd
#' @author Carson Sievert
#' @import knitr
#' @export
knit_print.animint <- function(x, options, ...) {
  if (!require(knitr)) warning("Please install.packages('knitr')")
  wd <- getwd()
  on.exit(setwd(wd))
  # This function should be evaluated in knitr's output directory
  setwd(knitr::opts_knit$get()[["output.dir"]])
  # the current knitr chunk 'label' defines a directory to place the animints 
  # hopefully this regular expression is safe enough to workaround bad chunk names
  # http://stackoverflow.com/questions/8959243/r-remove-non-alphanumeric-symbols-from-a-string
  dir <- gsub("[^[:alnum:]]", "", options$label)
  # modify the directory name until we find a unique one
  while (file.exists(dir)) {
    dir <- paste0(dir, "2")
  }
  animint2dir(x, out.dir = dir, json.file = 'plot.json', open.browser = FALSE)
  res <- new_animint(id = dir, json.file = file.path(dir, 'plot.json'))
  # if this is the first plot, place scripts just before the plot
  # there has to be a better way to do this, but this will do for now -- http://stackoverflow.com/questions/14308240/how-to-add-javascript-in-the-head-of-a-html-knitr-document
  if (length(knit_meta(class = "animint", clean = FALSE)) == 0) {
    res <- paste0('<script type="text/javascript" src="', dir, '/vendor/d3.v3.js"></script>\n<script type="text/javascript" src="', dir, '/animint.js"></script>', res)
  }
  knitr::asis_output(res, meta = list(animint = structure("", class = "animint")))
}

# Helper function to embed animint plots based on an id and filename for JSON object with the plot metadata
new_animint <- function(id, json.file) {
  jsonFile <- paste0("'", json.file, "'")
  idjs <- paste0("'#", id, "'")
  # using chunk labels is problematic for JS variable names is problematic since '-', '.', etc are illegal
  paste0('<p></p>\n<div id="', id, '"></div>\n<script>var plot = new animint(', idjs, ', ', jsonFile, ');</script>')
}


#' Create an shiny output binding
#' @export
animintOutput <- function(inputId) {
  # attach the appropriate class to the animint div
  gsub('></div>', paste0(' class="shiny-animint-binding"></div>'), 
       new_animint(inputId, "plot.json"))
}

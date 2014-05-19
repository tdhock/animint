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


##' Insert an interactive animation into an R markdown document using a customized print method.
##' 
##' @param x named list of ggplots and option lists to pass to gg2animint.
##' @references https://github.com/yihui/knitr/blob/master/vignettes/knit_print.Rmd
##' @author Carson Sievert
##' @export
knit_print.animint <- function(x, options, ...) {
  # if available, use the chunk 'label' for naming JSON output and plot ids
  #options$label <- if (!is.null(options$label)) "damnyou"
  jsonFile <- paste0(options$label, '.json')
  gg2animint(x, out.dir = '.', json.file = jsonFile, open.browser = FALSE)
  # remove redundant files created by calling gg2animint()
  file.remove("scripts.html", "index.html") 
  res <- new_animint(id = options$label, json.file = jsonFile)
  # if this is the first plot, place scripts just before the plot
  # there has to be a better way to do this, but this will do for now -- http://stackoverflow.com/questions/14308240/how-to-add-javascript-in-the-head-of-a-html-knitr-document
  if (length(knit_meta(class = "animint", clean = FALSE)) == 0) {
    res <- paste0('<script type="text/javascript" src="vendor/d3.v3.js"></script>\n<script type="text/javascript" src="animint.js"></script>', res)
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


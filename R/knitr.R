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
#' @export
knit_print.animint <- function(x, options, ...) {
  if (!requireNamespace("knitr")) warning("Please install.packages('knitr')")
  wd <- getwd()
  on.exit(setwd(wd))
  # This function should be evaluated in knitr's output directory
  setwd(knitr::opts_knit$get()[["output.dir"]])
  # the current knitr chunk 'label' defines a directory to place the animints 
  # hopefully this regular expression is safe enough to workaround bad chunk names
  # http://stackoverflow.com/questions/8959243/r-remove-non-alphanumeric-symbols-from-a-string
  dir <- gsub("[^[:alnum:]]", "", options$label)
  # modify the directory name until we find a unique one
#   while (file.exists(dir)) {
#     dir <- paste0(dir, "2")
#   }
  animint2dir(x, out.dir = dir, json.file = 'plot.json', open.browser = FALSE)
  res <- new_animint(list(id = dir), json.file = file.path(dir, 'plot.json'))
  # if this is the first plot, place scripts just before the plot
  # there has to be a better way to do this, but this will do for now -- http://stackoverflow.com/questions/14308240/how-to-add-javascript-in-the-head-of-a-html-knitr-document
  if (length(knitr::knit_meta(class = "animint", clean = FALSE)) == 0) {
    res <- paste0('<script type="text/javascript" src="', dir, '/vendor/d3.v3.js"></script>\n<script type="text/javascript" src="', dir, '/animint.js"></script>', res)
  }
  knitr::asis_output(res, meta = list(animint = structure("", class = "animint")))
}

# Helper function to create the HTML needed to embed animint plots 
# Note htmltools provides a better of doing this, but trying to avoid yet another dependency
new_animint <- function(attrs, json.file) {
  jsonFile <- paste0('"', json.file, '"')
  nms <- names(attrs)
  attrz <- paste(nms, shQuote(attrs), sep = '=', collapse = ' ')
  idx <- which(nms == 'id')
  classx <- which(nms == 'class')
  if (length(idx)) {
    prefix <- '"#'
    nm <- attrs[[idx]]
  } else if (length(classx)) {
    prefix <- '".'
    nm <- attrs[[idx]]
  }  else warning('Unknown attribute')
  # using chunk labels is problematic for JS variable names is problematic since '-', '.', etc are illegal
  selectr <- paste0(prefix, nm)
  paste0('<p></p>\n<div ', attrz,
         '></div>\n<script>var plot = new animint(', selectr, '", ', jsonFile, ');</script>')
}

#' Shiny ui output function
#' @param outputId output variable to read the plot from
#' @seealso http://shiny.rstudio.com/articles/building-outputs.html
#' @export
#' 
animintOutput <- function(outputId) {
  # Note that requireNamespace("shiny") should load digest & htmltools (both used later on)
  if (!requireNamespace("shiny")) message("Please install.packages('shiny')")
  deps <- lapply(animint_dependencies(), shiny::createWebDependency)
  htmltools::attachDependencies(htmltools::tags$div(id = outputId, class = 'shinyAnimint'), deps)
}

#' Create an animint output element
#' 
#' Shiny server output function customized for animint plots 
#' (similar to \code{shiny::plotOutput} and friends).
#' 
#' @param expr
#' @param env
#' @param quoted
#' @seealso http://shiny.rstudio.com/articles/building-outputs.html
#' @export
#' 
renderAnimint <- function(expr, env = parent.frame(), quoted = FALSE) {
  # Note that requireNamespace("shiny") should load digest & htmltools (both used later on)
  if (!requireNamespace("shiny")) message("Please install.packages('shiny')")
  
  # Convert the expression + environment into a function
  func <- shiny::exprToFunction(expr, env, quoted)
  
  # this will tell knitr how to place animint into an interactive document
  # implementation is similar to htmlwidgets::shinyRenderWidget
  # we can't use that in our case since we must call animint2dir
  # everytime shiny calls renderFunc
  renderFunc <- function(shinysession, name, ...) {
    val <- func()
    # using digest will guarantee a unique json file name for each animint plot
    jsonFile <- paste0(digest::digest(val), '.json')
    tmp <- tempdir()
    stuff <- animint2dir(val, out.dir = tmp, json.file = jsonFile, open.browser = FALSE)
    addResourcePath("animintAssets", tmp)
    list(jsonFile = jsonFile)
  }
  shiny::markRenderFunction(animint::animintOutput, renderFunc)
}

# html dependencies according htmltools protocols
# these are here basically so we can take advantage of shiny::createWebDependency
animint_dependencies <- function() {
  list(html_dependency_d3(),
       html_dependency_animint(),
       html_dependency_shinyAnimint())
}

html_dependency_d3 <- function() {
  htmltools::htmlDependency(name = "d3",
                 version = "3.0.0",
                 src = system.file("htmljs/vendor", package = "animint"),
                 script = "d3.v3.js")
}

html_dependency_animint <- function() {
  htmltools::htmlDependency(name = "animint",
                 version = packageVersion("animint"),
                 src = system.file("htmljs", package = "animint"),
                 script = "animint.js")
}

html_dependency_shinyAnimint <- function() {
  htmltools::htmlDependency(name = "shinyAnimint",
                 version = packageVersion("animint"),
                 src = system.file("shiny", package = "animint"),
                 script = "shinyAnimint.js")
}

html_dependency_plotJSON <- function(path, fileName) {
  htmltools::htmlDependency(name = "plotJSON",
                 version = packageVersion("animint"),
                 src = path,
                 script = fileName)
}


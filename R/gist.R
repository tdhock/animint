#' Convert a list of ggplots to an interactive animation and post files as a gist
#' 
#' Before using this function set your appropriate 'github.username' and 'github.password' \link{options}
#' 
#' @param plot.list a named list of ggplots and option lists.
#' @param out.dir local directory to store html/js/csv files.
#' @param json.file character string that names the JSON file with metadata associated with the plot.
#' @param url_prefix first part of URL for viewing result, with
#' http:// but without trailing /
#' e.g. "http://bl.ocks.org/tdhock/raw". Browser is prompted only if
#' there is \link{httr::url_success}.
#' @param ... options passed onto \link{gistr::gist}
#' @export
#' 
#' @examples
#' \dontrun{
#' devtools::install_github("rOpenSci/gistr") 
#' options(github.username = "foo", github.password = "bar")
#' library(animint)
#' iris$id <- 1:nrow(iris)
#' viz <- list(petal=ggplot()+
#'          geom_point(aes(Petal.Width, Petal.Length, fill=Species,
#'                         clickSelects=id), data=iris),
#'        sepal=ggplot()+
#'          geom_point(aes(Sepal.Width, Sepal.Length, fill=Species,
#'                         clickSelects=id), data=iris))
#' animint2gist(viz, description = "My animint plot")
#' }
animint2gist <- function
(plot.list, out.dir = tempfile(), json.file = "plot.json", ...,
 url_prefix = sprintf("http://bl.ocks.org/%s/raw",
   getOption("github.username"))){
  animint2dir(plot.list, out.dir, json.file, open.browser = FALSE)
  #if (is.null(getOption("github.username")) || is.null(getOption("github.password"))) 
  #  warning("Make sure 'github.username'", 
  #          "and 'github.password' are",
  #          "set in options(...)")
  if (!require(gistr)) warning("Please run `devtools::install_github('rOpenSci/gistr')` before using this function")
  # use a flat file structure!
  vendor.path <- file.path(out.dir, "vendor")
  vendor.files <- list.files(vendor.path)
  vendor.path.files <- file.path(vendor.path, vendor.files)
  copied <- file.copy(vendor.path.files, file.path(out.dir, vendor.files))
  file.remove(vendor.path.files)
  file.remove(vendor.path)
  # reflect script path in index.html to reflect the change in file structure
  index.file <- file.path(out.dir, "index.html")
  html <- readLines(index.file)
  html <- gsub("vendor/", "", html)
  cat(html, file = index.file, sep = "\n")
  all.files <- Sys.glob(file.path(out.dir, "*"))
  all.file.info <- file.info(all.files)
  is.empty <- all.file.info$size == 0
  is.ignored <- all.file.info$isdir | is.empty
  to.post <- all.files[!is.ignored]
  gist <- gistr::gist_create(to.post, ...)
  elem <- strsplit(gist, split = "/")[[1]]
  gist.code <- elem[length(elem)]
  url_name <- file.path(url_prefix, gist.code)
  if (interactive() && httr::url_success(url_name)) browseURL(url_name)
}

  

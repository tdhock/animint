#' Convert a list of ggplots to an interactive animation and post files as a gist
#' 
#' Before using this function set your appropriate 'github.username' and 'github.password' \link{options}
#' 
#' @param plot.list a named list of ggplots and option lists.
#' @param out.dir local directory to store html/js/csv files.
#' @param json.file character string that names the JSON file with metadata associated with the plot.
#' @param domain website domain for viewing result. Browser is prompted only if there is \link{httr::url_success}.
#' @param ... options passed onto \link{gistr::gist}
#' @importFrom gistr gist
#' @importFrom httr url_success
#' @export
#' 
#' @examples
#' \dontrun{
#' devtools::install_github("rOpenSci/gistr") 
#' options(github.username = "foo", github.password = "bar")
#' }
#' library(animint)
#' iris$id <- 1:nrow(iris)
#' viz <- list(petal=ggplot()+
#'          geom_point(aes(Petal.Width, Petal.Length, fill=Species,
#'                         clickSelects=id), data=iris),
#'        sepal=ggplot()+
#'          geom_point(aes(Sepal.Width, Sepal.Length, fill=Species,
#'                         clickSelects=id), data=iris))
#' animint2gist(viz, description = "My animint plot")


animint2gist <- function(plot.list, out.dir = tempfile(), json.file = "plot.json", ...,
                         domain = paste("http://bl.ocks.org", getOption("github.username"), "raw", sep = "/")) {
  gg2animint(plot.list, out.dir, json.file, open.browser = FALSE)
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
  gist <- gistr::gist(file.path(out.dir, list.files(out.dir)), ...)
  elem <- strsplit(gist, split = "/")[[1]]
  gist.code <- elem[length(elem)]
  url_name <- file.path(domain, gist.code)
  if (httr::url_success(url_name)) browseURL(url_name)
}

  
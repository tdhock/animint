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
#' @param description Brief description of gist.
#' This is passed to gist_create
#' and it becomes the plot title on the bl.ocks/username page.
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
(plot.list, out.dir = tempfile(), json.file = "plot.json", 
 url_prefix = sprintf("http://bl.ocks.org/%s/raw",
   getOption("github.username")),
 description=plot.list$title,
 ...){
  if(!is.character(description))description <- ""
  if(length(description) == 0)description <- ""
  if(length(description) > 1)description <- description[[1]]
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
  ## Try rendering a screenshot using RSelenium.
  if(require(RSelenium)){
    startServer()
    dr <- remoteDriver$new()
    dr$open()
    if(dr$value$takesScreenshot){
      dr$navigate("http://bl.ocks.org/tdhock/raw/bfd7e9ae6650d5b64be9/")
      screenshot <- file.path(out.dir, "screenshot.png")
      dr$screenshot(file=screenshot)
      ## thumbnail.png is usually 230x120 pixels.
      thumbnail <- file.path(out.dir, "thumbnail.png")
      cmd <- sprintf("convert %s -trim -resize 230 %s", screenshot, thumbnail)
      status <- system(cmd)
      if(status != 0){ # just use the full size image if we don't have convert.
        file.copy(screenshot, thumbnail)
      }
    }
    dr$closeWindow()
    dr$quit()
  }
  ## Figure out which files to post.
  all.files <- Sys.glob(file.path(out.dir, "*"))
  all.file.info <- file.info(all.files)
  is.empty <- all.file.info$size == 0
  is.tilde <- grepl("~$", all.files)
  is.png <- grepl("[.]png$", all.files)
  is.ignored <- all.file.info$isdir | is.empty | is.tilde
  ## TODO: delete the next line when gist_create can upload PNGs.
  is.ignored <- is.ignored | is.png 
  to.post <- all.files[!is.ignored]
  gist <- gist_create(to.post,
                      description=description,
                      ...)
  elem <- strsplit(gist, split = "/")[[1]]
  gist.code <- elem[length(elem)]
  url_name <- file.path(url_prefix, gist.code)
  if(interactive() && httr::url_success(url_name)) browseURL(url_name)
}

  

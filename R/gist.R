#' Convert a list of ggplots to an interactive animation and post files as a gist
#'
#' Before using this function set your appropriate 'github.username' and 'github.password' \link{options}
#'
#' @param plot.list a named list of ggplots and option lists.
#' @param description Brief description of gist.
#' This becomes the plot title on the bl.ocks/username page.
#' @param browse logical. Prompt browser to view viz on \url{http://bl.ocks.org}
#' @param ... options passed onto \code{animint2dir} and \code{gistr::gist_create}
#' @export
#'
#' @examples
#' \dontrun{
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
animint2gist <- function(plot.list, description = plot.list$title, 
                         browse = TRUE, ...) {
  if (!is.character(description) || length(description) == 0) description <- ""
  if (length(description) > 1) description <- description[[1]]
  res <- animint2dir(plot.list, open.browser = FALSE, ...)
  if (!requireNamespace("gistr")) {
    stop("Please run \n",
         "devtools::install_github('rOpenSci/gistr')",
         "before using this function")
  }
  # use a flat file structure!
  vendor.path <- file.path(res$out.dir, "vendor")
  vendor.files <- list.files(vendor.path)
  vendor.path.files <- file.path(vendor.path, vendor.files)
  copied <- file.copy(vendor.path.files, file.path(res$out.dir, vendor.files))
  file.remove(vendor.path.files)
  file.remove(vendor.path)
  # reflect script path in index.html to reflect the change in file structure
  index.file <- file.path(res$out.dir, "index.html")
  html <- readLines(index.file)
  html <- gsub("vendor/", "", html)
  cat(html, file = index.file, sep = "\n")
  ## Figure out which files to post.
  all.files <- Sys.glob(file.path(res$out.dir, "*"))
  all.file.info <- file.info(all.files)
  is.empty <- all.file.info$size == 0
  is.tilde <- grepl("~$", all.files)
  is.png <- grepl("[.]png$", all.files)
  is.ignored <- all.file.info$isdir | is.empty | is.tilde
  ## TODO: delete the next line when gist_create can upload PNGs.
  is.ignored <- is.ignored | is.png
  to.post <- all.files[!is.ignored]
  if(40 < length(to.post)){
    print(to.post)
    stop("your animint has ", length(to.post),
         " files but the Gist API will not serve more than 40 files,",
         " so your animint will not be viewable on bl.ocks.org.",
         " Try using https://pages.github.com/ to share your animint,",
         " or the chunk_vars argument to reduce the number of tsv files",
         " http://bit.ly/21scnod")
  }
  if(any(1024 * 1024 < all.file.info$size)){
    print(all.file.info[, "size", drop=FALSE])
    stop("your animint has files bigger than 1MB,",
         " but the Gist API will truncate files bigger than 1MB,",
         " so your animint will not be viewable on bl.ocks.org.",
         " Try using https://pages.github.com/ to share your animint,",
         " or the chunk_vars argument to combine some tsv files",
         " http://bit.ly/21scnod")
  }
  gist <- gistr::gist_create(to.post, description = description, 
                             browse = FALSE, ...)
  if (browse) 
    browseURL(sprintf("http://bl.ocks.org/%s/raw/%s/", 
                      gist$owner$login, gist$id))
  gist
}

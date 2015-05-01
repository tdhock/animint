#' Convert a list of ggplots to an interactive animation and post files as a gist
#'
#' Before using this function set your appropriate 'github.username' and 'github.password' \link{options}
#'
#' @param plot.list a named list of ggplots and option lists.
#' @param description Brief description of gist.
#' This becomes the plot title on the bl.ocks/username page.
#' @param screenshot logical. If TRUE, attempt to take screenshot of result and include with gist.
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
                         screenshot = TRUE, ...){
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
  gist <- gistr::gist_create(to.post, description = description, 
                             browse = FALSE, ...)
  if (interactive())
    browseURL(sprintf("http://bl.ocks.org/%s/raw/%s/", gist$owner$login, gist$id))
  ## Try rendering a screenshot using RSelenium.
  has.cmds <- all(Sys.which(c("git", "phantomjs")) != "")
  has.selenium <- requireNamespace("RSelenium")
  if (has.cmds && has.selenium && screenshot) {
    cloned.dir.base <- paste0("gist-", gist$id)
    cloned.dir <- file.path(tempdir(), cloned.dir.base)
    clone.cmd <- sprintf("git clone git@github.com:%s.git %s",
                         gist$id, cloned.dir)
    system(clone.cmd)
    tests_exit() # just in case, try to shut down remote driver
    pJS <- RSelenium::phantom()
    on.exit(pJS$stop(), add = TRUE)
    Sys.sleep(5)
    dr <- RSelenium::remoteDriver(browserName = "phantomjs")
    Sys.sleep(5)
    dr$open(silent = TRUE)
    if (isTRUE(dr$value$takesScreenshot)) {
      raw <- file.path("http://bl.ocks.org", gist$owner$login, "raw", gist$id)
      dr$navigate(raw)
      screenfile <- file.path(cloned.dir, "screenshot.png")
      dr$screenshot(file = screenfile)
      ## thumbnail.png is usually 230x120 pixels.
      thumbnail <- file.path(cloned.dir, "thumbnail.png")
      cmd <- sprintf("convert %s -trim -resize 230 %s", screenfile, thumbnail)
      status <- system(cmd)
      if(status != 0){ # just use the full size image if we don't have convert.
        file.copy(screenshot, thumbnail)
      }
      git.cmds <-
        paste("cd", cloned.dir, "&&",
              "git add thumbnail.png &&",
              "git commit thumbnail.png -m thumbnail &&",
              "git push")
      system(git.cmds)
    }
    thumb.url <- sprintf("https://gist.github.com/%s#file-thumbnail-png", gist$id)
    message("pushed thumbnail.png from ", cloned.dir, " to ", thumb.url)
  }
  gist
}

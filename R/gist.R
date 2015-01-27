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
(plot.list, out.dir = tempfile(), json.file = "plot.json", css.file = "",
 open.browser = interactive(),
 url_prefix = "http://bl.ocks.org/%s/raw",
 description=plot.list$title,
 ...){
  if(!is.character(description))description <- ""
  if(length(description) == 0)description <- ""
  if(length(description) > 1)description <- description[[1]]
  animint2dir(plot.list, out.dir, json.file, css.file, open.browser = FALSE)
  if(!requireNamespace("gistr")){
    error("Please run `devtools::install_github('rOpenSci/gistr')` before using this function")
  }
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
  gist <- gistr::gist_create(to.post, description=description, browse=FALSE, ...)
  if (interactive()) print(gist)
  if (open.browser) browseURL(sprintf(url_prefix, gist$id))
  ## Try rendering a screenshot using RSelenium.
  has.cmds <- Sys.which(c("pkill", "git")) != ""
  has.selenium <- requireNamespace("RSelenium")
  if(has.cmds && has.selenium){
    cloned.dir.base <- paste0("gist-", gist.code)
    cloned.dir <- file.path(tempdir(), cloned.dir.base)
    clone.cmd <- sprintf("git clone git@github.com:%s.git %s",
                         gist.code, cloned.dir)
    system('pkill -f selenium-server-standalone') #kill the selenium server if it is currently running
    system(clone.cmd)
    RSelenium::startServer()
    Sys.sleep(2) # otherwise I get Error in function (type, msg, asError = TRUE)  : couldn't connect to host
    dr <- RSelenium::remoteDriver$new(browserName = "firefox", port = 4444)
    dr$open()
    if (isTRUE(dr$value$takesScreenshot)){
      dr$navigate(url_name)
      screenshot <- file.path(cloned.dir, "screenshot.png")
      dr$screenshot(file=screenshot)
      ## thumbnail.png is usually 230x120 pixels.
      thumbnail <- file.path(cloned.dir, "thumbnail.png")
      cmd <- sprintf("convert %s -trim -resize 230 %s", screenshot, thumbnail)
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
    dr$closeWindow()
    dr$quit()
    thumb.url <- sprintf("https://gist.github.com/%s/%s#file-thumbnail-png",
                         getOption("github.username"),
                         gist.code)
    message("pushed thumbnail.png from ",
            cloned.dir,
            " to ",
            thumb.url)
  }
}



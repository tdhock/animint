#### helper functions to be resued in testing #####

#' Convert a ggplot object to 
#' @details This function assumes an object named 'context' exists -- this should be a 
#' character string as it will be used specify a directory which is bound to a testing context.
#' @author Carson Sievert
#' @param plotList A named list of ggplot2 objects
#' @param dir a name for a directory (this should be specific to a testing context)
#' @param subdir a name for a subdirectory (under dir) to place files
animint2HTML <- function(plotList, dir = "", subdir = "") {
  path <- file.path(dir, subdir)
  # create the directory if it doesn't already exist
  if (!file_test("-d", path)) dir.create(path)
  res <- gg2animint(plotList, out.dir = path, open.browser = FALSE)
  # save the address to be served as an attribute
  attr(res, "address") <- paste0("http://localhost:4848/testthat", "/", dir, "/", subdir, "/")
  res
}

# find/get methods are kinda slow in RSelenium (here is an example)
# remDr$navigate(attr(info, "address"))
# t <- remDr$findElement(using = 'class name', value = 'title')
# t$getElementText()[[1]]
#
# I think parsing using XML::htmlParse() and XML::getNodeSet() is faster/easier
#' @param info invisible ggplot2 object returned by animint
#' @author Carson Sievert
parse_page <- function(info) {
  remDr$navigate(attr(info, "address"))
  XML::htmlParse(remDr$getPageSource(), asText = TRUE)
}



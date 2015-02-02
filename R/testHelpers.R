animintEnv <- new.env(parent = emptyenv())

#' Run animint tests
#'
#' Convenience function for running animint tests.
#'
#' @param browserName Name of the browser to use for testing.
#' See ?RSelenium::remoteDriver for details.
#' @param dir character string with the path to animint's source code.
#' @param ... passed onto RSelenium::remoteDriver
#' @param port port number used for local file server
#' @param show.ps show user invoked processes
#' @param filter If not NULL, only tests with file names matching
#' this regular expression will be executed. Matching will take on the
#' file name after it has been stripped of "test-" and ".r".
#' @export
#' @examples
#'
#' # testing requires that you load testthat first.
#' library("testthat")
#' # run all tests with phantomjs
#' run_tests()
#' # run tests in test-rotate.R with Firefox
#' run_tests("firefox", filter = "rotate")
#' run_tests("chrome")
#'

run_tests <- function(browserName = "phantomjs", dir = ".", ...,
                      filter = NULL, show.ps = FALSE) {
  # packages required to run the tests
  pkgs <- lapply(c("RSelenium", "servr", "shiny", "rmarkdown", "XML"),
                 requireNamespace)
  # make sure testthat is loaded
  if (!"package:testthat" %in% search())
    stop("Testing requires the testthat package. Please library('testthat') and try again.")
  ## To get the process long names look at (helps to debug)
  if (show.ps) system("ps u")
  old <- getwd()
  on.exit(setwd(old))
  dir <- normalizePath(dir, mustWork = TRUE)
  if (basename(dir) != 'animint')
    stop("basename(dir) != 'animint'")
  setwd("tests")
  # start a non-blocking local file server
  cmd <- "R -e \'cat(Sys.getpid(), file=\"pid-4848.txt\"); servr::httd(dir=\"testthat\", port=4848, browser=FALSE)\'"
  system(cmd, wait = FALSE)
  # kill the file server on exit
  on.exit({
    tools::pskill(readLines("pid-4848.txt", warn = F))
    file.remove("pid-4848.txt")
  }, add = TRUE)
  # quit selenium on exit
  on.exit(kill_dr(browserName), add = TRUE)
  if (browserName == "phantomjs") {
    ## phantomjs doesn't need a selenium server (and is lightning fast!)
    ## Idea is from -- vignette("RSelenium-headless", package = "RSelenium")
    animintEnv$pJS <- RSelenium::phantom()
  } else {
    RSelenium::checkForServer(dir = system.file("bin", package="RSelenium"))
    selenium <- try(RSelenium::startServer(), silent = TRUE)
  }
  Sys.sleep(5) # give the binary a moment
  animintEnv$remDr <- RSelenium::remoteDriver(browserName = browserName)
  animintEnv$remDr$open(silent = TRUE)
  # run the tests
  test_dir("testthat", filter = filter)
}

# http://stackoverflow.com/questions/5043808/how-to-find-processes-based-on-port-and-kill-them-all
kill_port <- function(port) {
  if (.Platform$OS.type == "windows") {
    # might get some ideas here https://github.com/ropensci/RSelenium/issues/25
    warning("Testing on Windows not yet fully supported")
  } else {
    pid.tab <- system(paste0("lsof -i tcp:", port), intern = TRUE)
    pid <- read.table(text = pid.tab)[,1]
  }
  tools::pskill(pid)
}

# Kill the selenium driver
kill_dr <- function(b) {
  if (b == "phantomjs") {
    animintEnv$pJS$stop()
  } else {
    animintEnv$remDr$quit()
  }
  return(invisible())
}
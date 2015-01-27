library(testthat)
library(animint)
library(RSelenium)
library(XML)
library(shiny)
library(servr)

#' @param browserName Name of the browser to use for testing.
#' See ?RSelenium::remoteDriver for details.
#' @param ... passed onto RSelenium::remoteDriver
#' @param port port number used for local file server
#' @param show.ps show user invoked processes
#' @param filter If not NULL, only tests with file names matching
#' this regular expression will be executed. Matching will take on the
#' file name after it has been stripped of "test-" and ".r".
#' @export
#' @examples
#'
#' # run all tests with phantomjs
#' run_tests()
#' # run tests in test-rotate.R with Firefox
#' run_tests("firefox", filter = "rotate")
#' run_tests("chrome")
#'

run_tests <- function(browserName = "phantomjs", ..., port = 4848,
                      filter = NULL, show.ps = FALSE) {
  library("testthat") #gotta be a better way!
  ## To get the process long names look at (helps to debug)
  if (show.ps) system("ps u")
  if (basename(getwd()) != "animint")
    warning("The basename of the working directory should be 'animint'.")
  old <- getwd()
  on.exit(setwd(old))
  setwd("tests")
  source("testthat/functions.R")
  # start a daemonized local file server
  server <- try({
    servr::httd(dir = file.path(getwd(), "testthat"),
                port = port, daemon = TRUE, browser = FALSE)
  }, silent = TRUE)
  # if error, we won't know the right server ID,
  # so it's best to kill by the port number?
  if (inherits(server, "try-error")) {
    on.exit(kill_port(port), add = TRUE)
  } else {
    on.exit(servr::daemon_stop(server), add = TRUE)
  }
  # quit selenium on exit
  on.exit(kill_dr(browserName), add = TRUE)
  # selenium runs on port 4444
  # on.exit(kill_port(4444), add = TRUE)
  if (browserName == "phantomjs") {
    ## phantomjs doesn't need a selenium server (and is lightning fast!)
    ## Idea is from -- vignette("RSelenium-headless", package = "RSelenium")
    pJS <<- RSelenium::phantom()
  } else {
    RSelenium::checkForServer(dir = system.file("bin", package="RSelenium"))
    selenium <- try(RSelenium::startServer(), silent = TRUE)
  }
  Sys.sleep(5) # give the binary a moment
  remDr <<- RSelenium::remoteDriver(browserName = browserName)
  remDr$open(silent = TRUE)
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
  if (b == "phantomjs") pJS$stop()
  remDr$quit()
}

# this will make travisCI run tests with phantomJS
run_tests()
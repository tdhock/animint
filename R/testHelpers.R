#' @export
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
#' \dontrun{
#' # testing requires that you load testthat first.
#' library("testthat")
#' # run all tests with phantomjs
#' run_tests()
#' # run tests in test-rotate.R with Firefox
#' run_tests("firefox", filter = "rotate")
#' run_tests("chrome")
#' }
#'

run_tests <- function(browserName = "phantomjs", dir = ".", ...,
                      filter = NULL, show.ps = FALSE) {
  # necessary evil?
  library("testthat")
  library("XML")
  library("animint")
  library("shiny")
  library("rmarkdown")
  library("RSelenium")

  # packages required to run the tests
  #pkgs <- lapply(c("RSelenium", "servr", "shiny", "rmarkdown", "XML"),
  #               requireNamespace)
  # make sure testthat is loaded
  #if (!"package:testthat" %in% search())
  #  stop("Testing requires the testthat package. Please library('testthat') and try again.")
  ## To get the process long names look at (helps to debug)
  if (show.ps) system("ps u")
  old <- getwd()
  dir <- normalizePath(dir, mustWork = TRUE)
  if (!grepl("animint", dir)) stop("animint must be in the directory")
  base_name <- basename(dir)
  if (base_name == 'animint') {
    setwd("tests/testthat")
  } else if (base_name == "tests") {
    setwd("testthat")
  } else if (base_name != "testthat") {
    stop("dir must point to either animint's source, its tests folder, or its tests/testthat folder.")
  }
  # start a non-blocking local file server
  pid.file <- run_servr()
  port <- names(pid.file)
  # kill the file server on exit
  on.exit({
    tools::pskill(readLines(pid.file, warn = F))
    file.remove(pid.file)
  }, add = TRUE)
  # quit selenium on exit
  on.exit(kill_dr(browserName), add = TRUE)
  if (browserName == "phantomjs") {
    animintEnv$pJS <- RSelenium::phantom()
  } else {
    RSelenium::checkForServer(dir = system.file("bin", package="RSelenium"))
    selenium <- try(RSelenium::startServer(), silent = TRUE)
  }
  Sys.sleep(5) # give the backend a moment
  animintEnv$remDr <- RSelenium::remoteDriver(browserName = browserName)
  animintEnv$remDr$open(silent = TRUE)
  animintEnv$.address <- sprintf("http://localhost:%s", port)
  # run the tests
  source("functions.R")
  # return to original directory on exit
  on.exit(setwd(old), add = TRUE)
  test_dir(".", filter = filter)
}

# keep trying to start a server until success
run_servr <- function(port = 4848) {
  pid.file <- tempfile("pid")
  success <- try_servr(port = port, pidfile = pid.file)
  while (!success) {
    # kill the process ID
    tools::pskill(readLines(pid.file, warn = F))
    file.remove(pid.file)
    # randomly alter port number
    port <- port + seq.int(-10, 10)[sample.int(n = 21, size = 1)]
    pid.file <- tempfile("pid")
    success <- try_servr(port = port, pid.file)
  }
  setNames(pid.file, port)
}

# try to start a server on a specific port
try_servr <- function(port, pidfile) {
  cmd <- sprintf(
    "'cat(Sys.getpid(), file=\"%s\"); servr::httd(dir=\".\", port=%d, browser=FALSE)'",
    pidfile, port
  )
  output <- tempfile(fileext = "txt")
  t <- suppressWarnings(system2("R", c("-e", cmd),
                                stdout = output, stderr = output, wait = FALSE))
  # give it a second to write output (maybe addTaskCallback would be better?)
  Sys.sleep(10)
  on.exit(unlink(output))
  # returns true if success
  !any(grepl("Error", readLines(output)))
}

# Kill the selenium driver
kill_dr <- function(b) {
  if (b == "phantomjs") {
    animintEnv$pJS$stop()
  } else {
    animintEnv$remDr$quit()
    system('pkill -f selenium-server-standalone')
  }
  return(invisible())
}
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
  # necessary evil? I think so, unless we want to put `::` everywhere
  # one hack for CRAN would be to *not* export this function and run
  # tests with animint:::run_tests()
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
  # start a non-blocking local file server and remember the address
  res <- run_servr()
  animintEnv$.address <- sprintf("http://localhost:%s", res$port)
  animintEnv$.outDir <- "htmltest"
  on.exit(unlink(animintEnv$.outDir, recursive = TRUE), add = TRUE)
  # stop file server and selenium on exit
  on.exit(tools::pskill(res$pid), add = TRUE)
  on.exit(kill_dr(browserName), add = TRUE)
  if (browserName == "phantomjs") {
    animintEnv$pJS <- RSelenium::phantom()
  } else {
    # make sure Firefox doesn't try to redirect requests to localhost
    # https://cdivilly.wordpress.com/2013/08/15/disable-firefox-redirecting-to-localhost-com/
    # prof <- RSelenium::makeFirefoxProfile(list(browser.fixup.alternate.enabled = FALSE))
    RSelenium::checkForServer(dir = system.file("bin", package="RSelenium"))
    selenium <- try(RSelenium::startServer(), silent = TRUE)
  }
  # give the backend a moment to start-up
  Sys.sleep(5)
  animintEnv$remDr <- RSelenium::remoteDriver(browserName = browserName)
  animintEnv$remDr$open(silent = TRUE)
  # run the tests
  source("functions.R")
  # return to original directory on exit
  on.exit(setwd(old), add = TRUE)
  test_dir(".", filter = filter)
}

# keep trying to start a server until success
run_servr <- function(port = 4848, ...) {
  res <- try_servr(port = port, ...)
  while (!res$success) {
    # randomly alter port number
    new_port <- res$port + seq.int(-10, 10)[sample.int(n = 21, size = 1)]
    res <- try_servr(port = new_port, ...)
  }
  list(pid = res$pid, port = res$port)
}

# try to run a blocking command (for example a file server or shiny app)
# on a specific port. If it fails, kill the R process according to it's process ID.
try_servr <- function(port, pidfile = tempfile("pid"),
                      command = "servr::httd(dir=\".\", port=%d, browser=FALSE)") {
  cmd <- sprintf(
    paste0("'library(methods); cat(Sys.getpid(), file=\"%s\"); ", command, "'"),
    pidfile, port
  )
  output <- tempfile(fileext = "txt")
  t <- suppressWarnings(system2("Rscript", c("-e", cmd),
                                stdout = output, stderr = output, wait = FALSE))
  # give it a second to write output (maybe addTaskCallback would be better?)
  Sys.sleep(2)
  # was the command successful?
  success <- !any(grepl("Error", readLines(output, warn = FALSE)))
  pid <- readLines(pidfile, warn = FALSE)
  # if not, kill the process
  if (!success) tools::pskill(pid)
  unlink(pidfile)
  unlink(output)
  list(pid = pid, port = port, success = success)
}

# Kill the selenium driver
kill_dr <- function(b) {
  if (b == "phantomjs") {
    animintEnv$pJS$stop()
  } else {
    animintEnv$remDr$quit()
    if (nchar(Sys.which("pkill")) == 0)
      warning("Did not find the UNIX command `pkill` which kills \n",
              "the selenium server used in animint testing. \n",
              "You may have to kill this process manually.")
    system('pkill -f selenium-server-standalone')
  }
  return(invisible())
}
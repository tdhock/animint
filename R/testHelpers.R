#' Initiate external processes necessary for running tests.
#'
#' Initiates a local file server and remote driver.
#'
#' @param browserName Name of the browser to use for testing.
#' See ?RSelenium::remoteDriver for details.
#' @param port port number used for local file server
#' @param dir character string with the path to animint's source code. Defaults to current directory
#' @param ... list of additional options passed onto RSelenium::remoteDriver
#' @return invisible version of the process ID of a child R session running a local file server.
#' @export
#' @seealso \link{tests_run()}
#' 

tests_init <- function(browserName = "phantomjs", dir = ".", port = 4848, ...) {
  # remoteDriver methods won't work RSelenium is in search path
  if (!"package:RSelenium" %in% search()) {
    warning("RSelenium must be loaded to run tests. Attempting to load for you...")
    library("RSelenium")
  }
  # avoid weird errors if this function is called via testhat::check()
  # https://github.com/hadley/testthat/issues/144
  #Sys.setenv("R_TESTS" = "")
  # start a non-blocking local file server under path/to/animint/tests/testhat
  res <- run_servr(port = port, directory = find_test_path(dir))
  address <- sprintf("http://localhost:%s", res$port)
  # start-up remote driver 
  if (browserName == "phantomjs") {
    message("Starting phantomjs binary. To shut it down, run: \n pJS$stop()")
    pJS <<- RSelenium::phantom()
  } else {
    message("Starting selenium binary. To shut it down, run: \n",
            "remDr$closeWindow() \n",
            "remDr$closeServer()")
    RSelenium::checkForServer(dir = system.file("bin", package = "RSelenium"))
    selenium <- RSelenium::startServer()
  }
  # give an binaries a moment to start up
  Sys.sleep(5)
  remDr <<- RSelenium::remoteDriver(browserName = browserName, ...)
  # give the backend a moment to start-up
  Sys.sleep(2)
  remDr$open(silent = TRUE)
  Sys.sleep(2)
  remDr$navigate(address)
  invisible(res$pid)
}

#' Run animint tests
#'
#' Convenience function for running animint tests.
#'
#' @param dir character string with the path to animint's source code. Defaults to current directory
#' @param filter If not NULL, only tests with file names matching
#' this regular expression will be executed. Matching will take on the
#' file name after it has been stripped of "test-" and ".r".
#' @export
#' @examples
#'
#' \dontrun{
#' # run tests in test-rotate.R with Firefox
#' pid <- tests_init("firefox")
#' tests_run(filter = "rotate")
#' # clean-up
#' tools::pskill(pid)
#' remDr$closeWindow()
#' remDr$closeServer()
#' }
#'

tests_run <- function(dir = ".", filter = NULL) {
  if (!"package:testthat" %in% search()) {
    warning("testthat must be loaded to run tests. Attempting to load for you...")
    library("testthat")
  }
  test_path <- find_test_path(dir)
  source(file.path(test_path, "functions.R"))
  test_check("animint", filter = filter)
}

# --------------------------
# Functions used in multiple places
# --------------------------

# resolve the path
find_test_path <- function(dir) {
  dir <- normalizePath(dir, mustWork = TRUE)
  if (!grepl("animint", dir, fixed = TRUE)) 
    stop("animint must appear somewhere in 'dir'")
  base_dir <- basename(dir)
  if (!base_dir %in% c("animint", "tests", "testhat"))
    stop("Basename of dir must be one of: 'animint', 'tests', 'testhat'")
  ext_dir <- switch(base_dir,
                    animint = "tests/testthat",
                    tests = "testthat")
  file.path(dir, ext_dir)
}

# keep trying to start a server until success
# note that command argument is convenient for running shiny apps during tests as well
run_servr <- function(port, directory = ".", 
                      command = "servr::httd(dir=\"%s\", port=%d, launch.browser=FALSE)") {
  res <- try_servr(port = port, directory = directory, command = command)
  while (!res$success) {
    # randomly alter port number
    new_port <- res$port + seq.int(-10, 10)[sample.int(n = 21, size = 1)]
    res <- try_servr(port = new_port, directory = directory, command = command)
  }
  message("Serving directory ", directory, 
          sprintf(" on http://localhost:%d", res$port), "\n",
          "To shut down the local file server, run: \n",
          sprintf(" tools::pskill(%d)", as.integer(res$pid)))
  list(pid = res$pid, port = res$port)
}
# try to run a blocking command (for example a file server or shiny app)
# on a specific port. If it fails, kill the R process according to it's process ID.
try_servr <- function(port, directory = ".", command) {
  pid_file <- tempfile()
  dir <- normalizePath(directory, mustWork = TRUE)
  cmd <- sprintf(
    paste0("'library(methods); cat(Sys.getpid(), file=\"%s\");", command, "'"),
    pid_file, dir, port
  )
  output <- tempfile(fileext = "txt")
  t <- system2("Rscript", c("-e", cmd), 
               stdout = output, stderr = output, wait = FALSE)
  # give it a second to write output (maybe addTaskCallback would be better?)
  Sys.sleep(5)
  success <- t == 0
  # if something wasn't written, the process probably isn't running, right?
  e <- try(readLines(pid_file, warn = FALSE), silent = TRUE)
  if (!inherits(e, "try-error")) {
    tools::pskill(e)
    pid <- e
  } else {
    pid <- NA
  }
  unlink(pid_file)
  unlink(output)
  list(pid = pid, port = port, success = success)
}

#' Initiate external processes necessary for running tests.
#'
#' Initiates a local file server and remote driver.
#'
#' @param browserName Name of the browser to use for testing.
#' See ?RSelenium::remoteDriver for details.
#' @param dir character string with the path to animint's source code. Defaults to current directory
#' @param port port number used for local file server
#' @param ... list of additional options passed onto RSelenium::remoteDriver
#' @return invisible(TRUE)
#' @export
#' @seealso \link{tests_run}
#' 

tests_init <- function(browserName = "phantomjs", dir = ".", port = 4848, ...) {
  # try to exit out of previously initated processes
  ex <- tests_exit()
  # start a non-blocking local file server under path/to/animint/tests/testhat
  testPath <- find_test_path(dir)
  port <- run_servr(port = port, directory = testPath)
  # animint tests are performed in path/to/testthat/animint-htmltest/
  # note this path has to match the out.dir argument in animint2THML...
  testDir <- file.path(testPath, "animint-htmltest")
  # if the htmltest directory exists, wipe clean, then create an empty folder
  unlink(testDir, recursive = TRUE)
  dir.create(testDir)
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
  Sys.sleep(8)
  remDr <<- RSelenium::remoteDriver(browserName = browserName, ...)
  # give the backend a moment to start-up
  Sys.sleep(6)
  remDr$open(silent = TRUE)
  Sys.sleep(2)
  # wait a maximum of 30 seconds when searching for elements.
  remDr$setImplicitWaitTimeout(milliseconds = 30000)
  # if we navigate to localhost:%s/htmltest directly, some browsers will
  # redirect to www.htmltest.com. A 'safer' approach is to navigate, then click.
  remDr$navigate(sprintf("http://localhost:%s", port))
  e <- remDr$findElement("xpath", "//a[@href='animint-htmltest/']")
  e$clickElement()
  invisible(TRUE)
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
#' tests_init("firefox")
#' tests_run(filter = "rotate")
#' # clean-up
#' tests_exit()
#' }
#'

tests_run <- function(dir = ".", filter = NULL) {
  testDir <- find_test_path(dir)
  # functions that are reused across tests
  source(file.path(testDir, "functions.R"))
  # testthat::test_check assumes we are in path/to/animint/tests
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(dirname(testDir))
  # avoid weird errors if this function is called via testhat::check()
  # https://github.com/hadley/testthat/issues/144
  Sys.setenv("R_TESTS" = "")
  testthat::test_check("animint", filter = filter)
}

#' Kill child process(es) that may have been initiated in animint testing
#'
#' Read process IDs from a file and kill those process(es)
#' 
#' @seealso \link{tests_run}
#' @export
tests_exit <- function() {
  res <- stop_binary()
  e <- try(readLines(pid_file(), warn = FALSE), silent = TRUE)
  if (!inherits(e, "try-error")) {
    pids <- as.integer(e)
    res <- c(res, tools::pskill(pids))
  }
  unlink(pid_file())
  invisible(all(res))
}

#' Spawn a child R session that runs a 'blocking' command
#' 
#' Run a blocking command in a child R session (for example a file server or shiny app)
#'
#' @param directory path that the  server should map to.
#' @param port port number to _attempt_ to run server on.
#' @param code R code to execute in a child session
#' @return port number of the successful attempt
run_servr <- function(directory = ".", port = 4848,
                      code = "servr::httd(dir=\"%s\", port=%d, browser=FALSE)") {
  dir <- normalizePath(directory)
  cmd <- sprintf(
    # escape all the things!
    paste0("'library(methods); cat(Sys.getpid(), file=\"%s\", sep=\"\\\\n\", append=TRUE);", code, "'"),
    pid_file(), dir, port
  )
  output <- tempfile(fileext = "txt")
  t <- suppressWarnings(system2("Rscript", c("-e", cmd), 
                                stderr = output, stdout = output, wait = FALSE))
  Sys.sleep(5)
  o <- paste(readLines(output, warn = FALSE), "\n")
  message(o)
  success <- !any(grepl("Error", o, fixed = TRUE))
  if (success) {
    return(port)
  } else {
    # if not successful, try a new port
    # do I need to kill the other R process that failed?
    new_port <- port + seq.int(-10, 10)[sample.int(n = 21, size = 1)]
    message("couldn't start a server on port ", port, "; trying ", 
            new_port, " instead")
    run_servr(directory = directory, port = new_port, code = code)
  }
}

# --------------------------
# Functions that are used in multiple places
# --------------------------

stop_binary <- function() {
  if (exists("pJS")) {
    pJS$stop()
  } else if (exists("remDr")) {
    # these methods are really queries to the server
    # thus, if it is already shut down, we get some arcane error message
    e <- try({
      remDr$closeWindow()
      remDr$closeServer()
    })
  }
  TRUE
}

# file that will keep track of all processes were initiated during animint testing
pid_file <- function() {
  f <- file.path(find_test_path(), "pids.txt")
  if (!file.exists(f)) file(f, "a")
  f
}

# find the path to animint's testthat directory
find_test_path <- function(dir = ".") {
  dir <- normalizePath(dir, mustWork = TRUE)
  if (!grepl("animint", dir, fixed = TRUE)) 
    stop("animint must appear somewhere in 'dir'")
  base_dir <- basename(dir)
  if (!base_dir %in% c("animint", "tests", "testthat")) 
    stop("Basename of dir must be one of: 'animint', 'tests', 'testhat'")
  ext_dir <- switch(base_dir,
                    animint = "tests/testthat",
                    tests = "testthat",
                    testthat = "")
  file.path(dir, ext_dir)
}

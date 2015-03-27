#' Initiate external processes necessary for running tests.
#'
#' Initiates a local file server and remote driver.
#'
#' @param browserName Name of the browser to use for testing.
#' See ?RSelenium::remoteDriver for details.
#' @param port port number used for local file server
#' @param dir character string with the path to animint's source code. Defaults to current directory
#' @param ... list of additional options passed onto RSelenium::remoteDriver
#' @return invisible version of the testing environment
#' @export
#' @seealso \link{tests_run()}

tests_init <- function(browserName = "phantomjs", dir = ".", port = 4848, ...) {
  # remoteDriver methods won't work RSelenium is in search path
  if (!"package:RSelenium" %in% search()) {
    warning("RSelenium must be loaded to run tests. Attempting to load for you...")
    library("RSelenium")
  }
  # avoid weird errors if this function is called via testhat::check()
  # https://github.com/hadley/testthat/issues/144
  Sys.setenv("R_TESTS" = "")
  # initiate environment that will store important testing info
  env <- new.env(parent = globalenv())
  dir <- normalizePath(dir, mustWork = TRUE)
  if (basename(dir) == 'animint') {
    env$.testDir <- file.path(dir, "tests/testthat")
  } else {
    stop("Please provide the path animint's source code in the 'dir' argument.")
  }
  env$.outDir <- file.path(env$.testDir, "htmltest")
  # start a non-blocking local file server; remember port and process ID
  res <- run_servr(port = port, directory = env$.testDir)
  env$.address <- sprintf("http://localhost:%s", res$port)
  env$.filepid <- res$pid
  # start-up remote driver 
  if (browserName == "phantomjs") {
    env$pJS <- RSelenium::phantom()
  } else {
    RSelenium::checkForServer(dir = system.file("bin", package = "RSelenium"))
    selenium <- RSelenium::startServer()
  }
  # give an binaries a moment to start up
  Sys.sleep(5)
  env$remDr <- RSelenium::remoteDriver(browserName = browserName, ...)
  # give the backend a moment to start-up
  Sys.sleep(2)
  env$remDr$open(silent = TRUE)
  invisible(env)
}

#' Run animint tests
#'
#' Convenience function for running animint tests.
#' 
#' @param envir an environmnent.
#' @param filter If not NULL, only tests with file names matching
#' this regular expression will be executed. Matching will take on the
#' file name after it has been stripped of "test-" and ".r".
#' @export
#' @examples
#'
#' \dontrun{
#' # run tests in test-rotate.R with Firefox
#' env <- tests_init("firefox")
#' tests_run(env, filter = "rotate")
#' tests_exit(env)
#' }
#'

tests_run <- function(envir, filter = NULL) {
  if (!is.environment(envir)) stop("Must provide an environment!")
  testDir <- envir$.testDir
  sys.source(file.path(testDir, "functions.R"), envir = envir)
  testthat::test_dir(testDir, filter = filter, env = envir)
}


#' Shut down external processes necessary for running tests
#'
#' Shuts down the local file server and remote driver initiated by 
#' \link{tests_init()}.
#'
#' @param envir an environmnent.
#' @return invisible(FALSE) if an error occurred; invisible(TRUE) otherwise
#' @export
#' @seealso \link{tests_run()}

tests_exit <- function(envir) {
  if (!is.environment(envir)) stop("Must provide a testing environment!")
  # try to shut down phantomjs, then remove it from environment
  has_phantom <- exists("pJS", envir = envir)
  has_driver <- exists("remDr", envir = envir)
  if (!has_phantom && !has_driver) {
    warning("No processes to shut down")
    return(invisible())
  }
  if (has_phantom) {
    e <- try(envir$pJS$stop())
    rm("pJS", envir = envir)
  } else {
    st <- envir$remDr$getStatus()
    # if running shut down; if not exit
    e <- try({
      envir$remDr$closeWindow()
      envir$remDr$closeServer()
    })
    rm("remDr", envir = envir)
  }
  # stop file server
  e2 <- tools::pskill(envir$.filepid)
  invisible(e2 && !inherits(e, "try-error"))
}


# --------------------------
# Helper functions
# --------------------------

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

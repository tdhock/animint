library(testthat)
library(animint)
library(servr)
library(RSelenium)
library(XML)
library(shiny)
# source some convenience functions
source(file.path(getwd(), "testthat", "functions.R"))

## To get the process long names look at (helps to debug)
system("ps u")

start_servr <- function(port = 4848) {
  # Thanks to Winston for making this process cleaner
  # https://github.com/wch/webshot/blob/master/R/appshot.R
  pidfile <- tempfile("pid")
  on.exit(unlink(pidfile))
  cmd <- sprintf(
    "'cat(Sys.getpid(), file=\"%s\"); servr::httd(port=%d, dir=file.path(getwd(), \"testthat\"), launch.browser=FALSE)'",
    pidfile,
    port
  )
  # Run app in background
  system2("R", args = c("--slave", "-e", cmd), wait = FALSE)
  # Return process id
  as.numeric(readLines(pidfile, warn = FALSE))
}

# Kill the selenium driver
kill_dr <- function() {
  remDr$quit()
  if (interactive()) {
    # close Firefox & stop selenium server
    remDr$closeWindow()
    remDr$closeServer()
  } else {
    # stop phantomjs
    pJS$stop()
  }
}

run_test <- function() {
  # Run local file server in a separate R session
  pid <- start_servr()
  # In case of potential errors in running the test, we make sure to kill
  # all the background processes upon exit
  on.exit(tools::pskill(pid), add = TRUE)
  on.exit(kill_dr(), add = TRUE)
  if (interactive()) {
    checkForServer(dir=system.file("bin", package="RSelenium"))
    startServer()
    Sys.sleep(5)
    remDr <<- remoteDriver(browserName="firefox")
  } else {
    ## phantomjs doesn't need a selenium server (and is lightning fast!)
    ## Idea is from -- vignette("RSelenium-headless", package = "RSelenium")
    pJS <<- RSelenium::phantom()
    Sys.sleep(5) # give the binary a moment
    remDr <<- remoteDriver(browserName = 'phantomjs')
  }
  remDr$open(silent = TRUE)
  # run the tests
  test_check("animint")
}

run_test()

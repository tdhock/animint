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

kill_all <- function() {
  # kill the local file server
  system('pkill -f "servr::httd\\(port=4848"')
  # quit the remote driver
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
  # In case of potential errors in running the test, we make sure to kill 
  # all the background processes upon exiting this function.
  on.exit(kill_all(), add = TRUE)
  
  # Run local file server in a separate R session
  # IDEA: If we can access the process ID (with Sys.getpid()) from this child session
  # we could kill the file server with tools::pskill(). This would help testing be 
  # platform independent -- I just don't know how to transfer objects between sessions
  cmd <- "R -e \'servr::httd(port=4848, dir=file.path(getwd(), \"testthat\"), launch.browser=FALSE)\'"
  system(cmd, wait = FALSE)
  
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

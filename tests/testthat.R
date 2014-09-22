library(testthat)
library(animint)
library(servr)
library(RSelenium)
library(XML)
# source some convenience functions
source(file.path(getwd(), "testthat", "functions.R"))

# Before starting the servers, kill any servers that may be already running.
killservr <- 'pkill -f "servr::httd\\(port=4848"'
system(killservr)
# ## To get the process long names look at
system("ps u")

# IDEA: If we can access the process ID (with Sys.getpid()) from this child session
# we could kill the file server with tools::pskill(). This would help testing be 
# platform independent -- I just don't know how to transfer objects between sessions
cmd <- "R -e \'servr::httd(port=4848, dir=file.path(getwd(), \"testthat\"), launch.browser=FALSE)\'"
system(cmd, intern = FALSE, wait = FALSE)

# Note: it might be a good idea to attempt to kill phantomjs at this point
# If phantomjs is already running, the driver returns error but the tests
# should still run.

## phantomjs doesn't need a selenium server (and is lightning fast!)
## Idea is from -- vignette("RSelenium-headless", package = "RSelenium")
pJS <- RSelenium::phantom()
Sys.sleep(5) # give the binary a moment
remDr <- remoteDriver(browserName = 'phantomjs')
remDr$open(silent = TRUE)

# run the tests
test_check("animint")

# stop phantomjs
pJS$stop()
# kill the file server
system(killservr)

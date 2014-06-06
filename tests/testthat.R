# Setup an infrastructure that all tests will use
# For more details, see this discussion  -- https://github.com/johndharrison/RSelenium/issues/17
# Thanks for all the help @johndharrison!
library(testthat)
library(animint)
library(servr)
library(RSelenium)
library(XML)
source("testthat/functions.R")

## Before starting the servers, kill any servers that are already
## running.
system('pkill -f "servr::httd\\(port=4848"')
system('pkill -f selenium-server-standalone')
## To get the process long names look at
system("ps u")

# Initialize local server in a seperate R process
cmd <- paste0('R -e \"servr::httd(port=4848)\"')
if (.Platform$OS.type != "unix") cmd <- paste0(cmd, " &")
system(cmd, intern = FALSE, wait = FALSE)

# Check to make sure that a selenium server exists, and if not, download it
checkForServer(dir = system.file("bin", package = "RSelenium"))
# We should use browser = "phantomjs" eventually, but it hangs during multiple tests
startServer()

Sys.sleep(2) # otherwise I get Error in function (type, msg, asError = TRUE)  : couldn't connect to host
remDr <- remoteDriver$new(browserName = "firefox")
##str(remDr)
remDr$open()


test_check("animint")

# Close the browser
remDr$quit()
# Kill the server
killcmd <- paste0('pkill -f "servr::httd\\(port=4848"')
system(killcmd)

# List relevant processes
#procs <- system('ps aux|grep "servr::httd"', intern = TRUE)

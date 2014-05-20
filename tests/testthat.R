# Setup an infrastructure that all tests will use
# For more details, see this discussion  -- https://github.com/johndharrison/RSelenium/issues/17
# @johndharrison
library(testthat)
library(animint)
library(servr)
library(RSelenium)
library(XML)

# Initialize local server in a seperate R process
cmd <- paste0('R -e \"servr::httd(port=4848)\"')
if (.Platform$OS.type != "unix") cmd <- paste0(cmd, " &")
system(cmd, intern = FALSE, wait = FALSE)

# Setup a selenium remote webdriver instance via RSelenium
# A much more sophisticated approach is located here -- https://github.com/johndharrison/RSelenium/blob/master/inst/tests/setup.r
remDr <- remoteDriver()
remDr$open(silent = TRUE)

test_check("animint", filter = "labels")

# Kill the local server
killcmd <- paste0('pkill -f "servr::httd\\(port=4848"')
system(killcmd)

# List relevant processes
#procs <- system('ps aux|grep "servr::httd"', intern = TRUE)


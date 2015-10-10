library("testthat")
library("animint")
library("RSelenium") # RSelenium works with firefox <= 37.0.2

## TDH usually tests using 

## thocking@silene:~/R/animint-mine(fix-common-chunk)$ firefox --version
## Mozilla Firefox 11.0

filter <- Sys.getenv("TEST_SUITE")
if(filter == "")filter <- NULL
dont.need.browser <- grepl("compiler", filter)
use.browser <- !dont.need.browser

if(use.browser)tests_init()
tests_run(filter=filter)
if(use.browser)tests_exit()


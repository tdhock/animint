library("testthat")
library("animint")
library("RSelenium") # RSelenium works with firefox <= 37.0.2

## TDH usually tests using 

## thocking@silene:~/R/animint-mine(fix-common-chunk)$ firefox --version
## Mozilla Firefox 11.0

tests_init()
tests_run()
tests_exit()

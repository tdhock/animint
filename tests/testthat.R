library("testthat")
library("animint")
library("RSelenium")

## RSelenium does not work with all versions of firefox, TDH usually
## tests using one of the following.

## thocking@silene:~/R/animint-mine(fix-common-chunk)$ firefox --version
## Mozilla Firefox 11.0
## thocking@silene:~/R/animint(animation-fix)$ java -jar ~/lib/R/library/RSelenium/bin/selenium-server-standalone.jar --version
## 10:10:58.942 INFO - Launching a standalone server
## 10:10:58.973 INFO - Java: Sun Microsystems Inc. 23.25-b01
## 10:10:58.973 INFO - OS: Linux 3.8.0-44-generic amd64
## 10:10:58.987 INFO - v2.44.0, with Core v2.44.0. Built from revision 76d78cf
## > packageVersion("RSelenium")
## [1] ‘1.3.6’

## > packageVersion("RSelenium")
## [1] ‘1.3.5’
## tdhock@recycled:~/R/animint(roc-bugfix)$ firefox --version
## Mozilla Firefox 41.0
## tdhock@recycled:~/R/animint(roc-bugfix*)$ java -jar ~/lib/R/library/RSelenium/bin/selenium-server-standalone.jar --version
## 08:13:17.803 INFO - Launching a standalone Selenium Server
## 08:13:17.877 INFO - Java: Oracle Corporation 24.79-b02
## 08:13:17.877 INFO - OS: Linux 3.13.0-65-generic i386
## 08:13:17.907 INFO - v2.47.0, with Core v2.47.0. Built from revision 0e4837e

filter <- Sys.getenv("TEST_SUITE")
dont.need.browser <- grepl("compiler", filter)
use.browser <- !dont.need.browser
if(filter == ""){
  filter <- NULL
}
if(interactive()){
  setwd("testthat")
  source("helper-functions.R")
  tests_init("firefox")
}

if(use.browser)tests_init()
tests_run(filter=filter)
if(use.browser)tests_exit()


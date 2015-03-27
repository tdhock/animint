library("testthat")
library("animint")
library("RSelenium")

pid <- tests_init()
tests_run()
tools::pskill(pid)
pJS$stop()

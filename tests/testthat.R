library("testthat")
library("animint")
library("RSelenium")

env <- tests_init()
tests_run(env)
tests_exit(env)

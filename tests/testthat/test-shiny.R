context("shiny")

Sys.setenv("R_TESTS" = "") # As suggested by hadley -- https://github.com/hadley/testthat/issues/144

#just in case, try to kill process that we may have started
killservr <- 'pkill -f "shiny::runApp\\(port=6012"'
system(killservr)
# serve the app using a seperate R session so we can execute subsequent commands
cmd <- "R -e \'shiny::runApp(port=6012, appDir=system.file(\"examples/shiny\", package = \"animint\"))\'"
system(cmd, wait = FALSE)


test_that("animint plot renders in a shiny app", {
  Sys.sleep(5) # give shiny a second to do it's thing
  remDr$navigate("http://localhost:6012/")
  Sys.sleep(10)
  html <- XML::htmlParse(remDr$getPageSource(), asText = TRUE)
  circles <- getNodeSet(html, "//div[@id='animint']//circle")
  expect_true(length(circles) >= 1)
})

# kill the shiny app
killservr <- 'pkill -f "shiny::runApp\\(port=6012"'
system(killservr)
#just in case, try to kill process that we may have started
killrmd <- 'pkill -f "rmarkdown::run\\(shiny_args=list\\(port=6014"'
system(killrmd)

# serve the "app" using a seperate R session so we can execute subsequent commands
rmd <- "R -e \'rmarkdown::run(shiny_args=list(port=6014), system.file(\"examples/rmarkdown/index.Rmd\", package = \"animint\"))\'"
system(rmd, wait = FALSE)

test_that("animint plot renders in an interactive document", {
  Sys.sleep(10) # give shiny a second to do it's thing
  remDr$navigate("http://localhost:6014/")
  Sys.sleep(10)
  html <- XML::htmlParse(remDr$getPageSource(), asText = TRUE)
  circles <- getNodeSet(html, "//svg//circle")
  expect_true(length(circles) >= 1)
})

killrmd <- 'pkill -f "rmarkdown::run\\(shiny_args=list\\(port=6014"'
system(killrmd)

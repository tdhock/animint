context("shiny")

test_that("animint plot renders in a shiny app", {
  # serve the app using a seperate R session so we can execute subsequent commands
  killservr <- 'pkill -f "shiny::runApp\\(port=6012"'
  system(killservr)
  cmd <- "R -e \'shiny::runApp(port=6012, appDir=system.file(\"examples\", \"shiny\", package = \"animint\"))\'"
  system(cmd, intern = FALSE, wait = FALSE)
  Sys.sleep(5) # give shiny a second to do it's thing
  remDr$navigate("http://localhost:6012/")
  Sys.sleep(10) # I suppose we need to wait until animint2dir has time to run
  html <- XML::htmlParse(remDr$getPageSource(), asText = TRUE)
  circles <- getNodeSet(html, "//div[@id='animint']//circle")
  expect_true(length(circles) >= 1)
  killservr <- 'pkill -f "shiny::runApp\\(port=6012"'
  system(killservr)
})

test_that("animint plot renders in an interactive document", {
  killrmd <- 'pkill -f "rmarkdown::run\\(shiny_args=list\\(port=6014"'
  system(killrmd)
  rmd <- "R -e \'rmarkdown::run(shiny_args=list(port=6014), system.file(\"examples/rmarkdown/index.Rmd\", package = \"animint\"))\'"
  system(rmd, intern = FALSE, wait = FALSE)
  Sys.sleep(5) # give shiny a second to do it's thing
  remDr$navigate("http://localhost:6014/")
  Sys.sleep(10) # I suppose we need to wait until animint2dir has time to run
  html <- XML::htmlParse(remDr$getPageSource(), asText = TRUE)
  circles <- getNodeSet(html, "//svg//circle")
  expect_true(length(circles) >= 1)
  killrmd <- 'pkill -f "rmarkdown::run\\(shiny_args=list\\(port=6014"'
  system(killrmd)
})
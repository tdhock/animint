context("shiny")

test_that("animint plot renders in a shiny app", {
  # serve the app using a seperate R session so we can execute subsequent commands
  cmd <- "R -e \'shiny::runApp(port=6012, appDir=system.file(\"examples\", \"shiny\", package = \"animint\"))\'"
  system(cmd, intern = FALSE, wait = FALSE)
  
  Sys.sleep(5) # give shiny a second to do it's thing
  remDr$navigate("http://127.0.0.1:6012/")
  Sys.sleep(20) # I suppose we need to wait until animint2dir has time to run
  remDr$refresh()
  html <- htmlParse(remDr$getPageSource(), asText = TRUE)
  circles <- getNodeSet(html, "//div[@id='animint']//circle")
  expect_true(length(circles) >= 1)
  #remDr$getSession()$browserName
  killservr <- 'pkill -f "shiny::runApp\\(port=6012"'
  system(killservr)
})

test_that("animint plot renders in an interactive document", {
  rmd <- "R -e \'rmarkdown::run(shiny_args=list(port=6014), system.file(\"examples/rmarkdown/index.Rmd\", package = \"animint\"))\'"
  system(rmd, intern = FALSE, wait = FALSE)
  
  Sys.sleep(5) # give shiny a second to do it's thing
  remDr$navigate("http://127.0.0.1:6014/")
  Sys.sleep(20) # I suppose we need to wait until animint2dir has time to run
  remDr$refresh()
  html <- htmlParse(remDr$getPageSource(), asText = TRUE)
  circles <- getNodeSet(html, "//div[@id='animint']//circle")
  expect_true(length(circles) >= 1)
  killrmd <- 'pkill -f "rmarkdown::run\\(shiny_args=list(port=6014"'
  system(killrmd)
})
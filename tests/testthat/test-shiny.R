context("shiny")

Sys.setenv("R_TESTS" = "") # As suggested by hadley -- https://github.com/hadley/testthat/issues/144

cmd <- "R -e \'cat(Sys.getpid(), file=\"pid-6012.txt\"); shiny::runApp(port=6012, appDir=system.file(\"examples/shiny\", package = \"animint\"))\'"
system(cmd, wait = FALSE)

test_that("animint plot renders in a shiny app", {
  Sys.sleep(5) # give shiny a second to do it's thing
  animintEnv$remDr$navigate("http://localhost:6012/")
  Sys.sleep(10)
  html <- XML::htmlParse(animintEnv$remDr$getPageSource(), asText = TRUE)
  tools::pskill(readLines("pid-6012.txt", warn = F))
  file.remove("pid-6012.txt")
  circles <- getNodeSet(html, "//div[@id='animint']//circle")
  expect_true(length(circles) >= 1)
})

rmd <- "R -e \'cat(Sys.getpid(), file=\"pid-6014.txt\"); rmarkdown::run(shiny_args=list(port=6014), system.file(\"examples/rmarkdown/index.Rmd\", package = \"animint\"))\'"
system(rmd, wait = FALSE)

test_that("animint plot renders in an interactive document", {
  Sys.sleep(10) # give shiny a second to do it's thing
  animintEnv$remDr$navigate("http://localhost:6014/")
  Sys.sleep(10)
  html <- XML::htmlParse(animintEnv$remDr$getPageSource(), asText = TRUE)
  tools::pskill(readLines("pid-6014.txt", warn = F))
  file.remove("pid-6014.txt")
  circles <- getNodeSet(html, "//svg//circle")
  expect_true(length(circles) >= 1)
})


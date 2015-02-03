context("shiny")

Sys.setenv("R_TESTS" = "") # As suggested by hadley -- https://github.com/hadley/testthat/issues/144

pid.file <- animint:::run_servr(port = 6012)

test_that("animint plot renders in a shiny app", {
  Sys.sleep(5) # give shiny a second to do it's thing
  animintEnv$remDr$navigate("http://localhost:6012/")
  Sys.sleep(10)
  html <- XML::htmlParse(animintEnv$remDr$getPageSource(), asText = TRUE)
  tools::pskill(readLines(pid.file, warn = F))
  file.remove(pid.file)
  circles <- getNodeSet(html, "//div[@id='animint']//circle")
  expect_true(length(circles) >= 1)
})

pid.file <- animint:::run_servr(port = 6014)

test_that("animint plot renders in an interactive document", {
  Sys.sleep(10) # give shiny a second to do it's thing
  animintEnv$remDr$navigate("http://localhost:6014/")
  Sys.sleep(10)
  html <- XML::htmlParse(animintEnv$remDr$getPageSource(), asText = TRUE)
  tools::pskill(readLines(pid.file, warn = F))
  file.remove(pid.file)
  circles <- getNodeSet(html, "//svg//circle")
  expect_true(length(circles) >= 1)
})


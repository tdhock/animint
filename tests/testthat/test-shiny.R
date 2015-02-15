context("shiny")

shiny_cmd <- "shiny::runApp(appDir=system.file(\"examples/shiny\", package = \"animint\"), port=%d, launch.browser=FALSE)"
res <- run_servr(command = shiny_cmd)
address <- sprintf("http://localhost:%s/", res$port)

test_that("animint plot renders in a shiny app", {
  Sys.sleep(5) # give shiny a second to do it's thing
  animintEnv$remDr$navigate(address)
  Sys.sleep(10)
  html <- XML::htmlParse(animintEnv$remDr$getPageSource(), asText = TRUE)
  tools::pskill(res$pid)
  circles <- getNodeSet(html, "//div[@id='animint']//circle")
  expect_true(length(circles) >= 1)
})

res <- run_servr(command = shiny_cmd)
address <- sprintf("http://localhost:%s/", res$port)

test_that("animint plot renders in an interactive document", {
  Sys.sleep(10) # give shiny a second to do it's thing
  animintEnv$remDr$navigate(address)
  Sys.sleep(10)
  html <- XML::htmlParse(animintEnv$remDr$getPageSource(), asText = TRUE)
  tools::pskill(res$pid)
  circles <- getNodeSet(html, "//svg//circle")
  expect_true(length(circles) >= 1)
})
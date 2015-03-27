context("shiny")

shiny_dir <- system.file("inst/examples/shiny", package = "animint")
shiny_cmd <- "shiny::runApp(appDir=\"%s\", port=%d, launch.browser=FALSE)"
res <- animint:::run_servr(port = 3147, directory = shiny_dir, command = shiny_cmd)
address <- sprintf("http://localhost:%s/", res$port)

test_that("animint plot renders in a shiny app", {
  Sys.sleep(5) # give shiny a second to do it's thing
  remDr$navigate(address)
  Sys.sleep(10)
  html <- XML::htmlParse(remDr$getPageSource(), asText = TRUE)
  tools::pskill(res$pid)
  circles <- getNodeSet(html, "//div[@id='animint']//circle")
  expect_true(length(circles) >= 1)
})

rmd_dir <- system.file("inst/examples/rmarkdown", package = "animint")
rmd_cmd <- "rmarkdown::run(dir = \"%s\", shiny_args = list(port=%d, launch.browser=FALSE))"
res <- animint:::run_servr(port = 3147, directory = rmd_dir, command = rmd_cmd)
address <- sprintf("http://localhost:%s/", res$port)

test_that("animint plot renders in an interactive document", {
  Sys.sleep(10) # give shiny a second to do it's thing
  remDr$navigate(address)
  Sys.sleep(10)
  html <- XML::htmlParse(remDr$getPageSource(), asText = TRUE)
  tools::pskill(res$pid)
  circles <- getNodeSet(html, "//svg//circle")
  expect_true(length(circles) >= 1)
})

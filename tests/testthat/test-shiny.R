acontext("shiny")

if (Sys.getenv("TRAVIS") == "true" | Sys.getenv("WERCKER") == "true") {
  message("shiny tests don't work on travis/wercker (but should someday)")
} else {
  # shiny tests require navigating to different ports, so remember where we are
  # and return when tests are done
  old_address <- remDr$getCurrentUrl()[[1]]
  remDr$setImplicitWaitTimeout(milliseconds = 30000)
  
  shiny_dir <- system.file("examples/shiny", package = "animint")
  shiny_cmd <- "shiny::runApp(appDir=\"%s\", port=%d, launch.browser=FALSE)"
  animint:::run_servr(port = 3147, directory = shiny_dir, code = shiny_cmd)
  address <- sprintf("http://localhost:3147")
  
  test_that("animint plot renders in a shiny app", {
    Sys.sleep(10) # give shiny a second to do it's thing
    remDr$navigate(address)
    Sys.sleep(10)
    # just check that svg is displayed
    html <- getHTML()
    circles <- getNodeSet(html, "//div[@id='animint']//circle")
    expect_true(length(circles) >= 1)
  })
  
  rmd_dir <- system.file("examples/rmarkdown", package = "animint")
  rmd_cmd <- "rmarkdown::run(dir = \"%s\", shiny_args = list(port=%d, launch.browser=FALSE))"
  animint:::run_servr(port = 3120, directory = rmd_dir, code = rmd_cmd)
  address <- sprintf("http://localhost:3120")
  
  test_that("animint plot renders in an interactive document", {
    Sys.sleep(10) # give shiny a second to do it's thing
    remDr$navigate(address)
    Sys.sleep(10)
    e <- remDr$findElement("class name", "shiny-frame")
    remDr$switchToFrame(e)
    html <- getHTML()
    circles <- getNodeSet(html, "//svg//circle")
    expect_true(length(circles) >= 1)
  })
  
  # go back to non-shiny tests
  remDr$navigate(old_address)
}

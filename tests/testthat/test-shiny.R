acontext("shiny")

## We do not need if(on wercker or travis){skip shiny test} as of 10
## Oct 2015, since we only run tests that match the TEST_SUITE env
## var, and test-shiny.R never matches.

## shiny tests require navigating to different ports, so remember
## where we are and return when tests are done
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
  ## just check that svg is displayed
  html <- getHTML()
  circles <- getNodeSet(html, "//div[@id='animint']//circle")
  expect_true(length(circles) >= 1)
})

shiny_dir <- system.file("examples/shiny-WorldBank", package = "animint")
shiny_cmd <- "shiny::runApp(appDir=\"%s\", port=%d, launch.browser=FALSE)"
animint:::run_servr(port = 3148, directory = shiny_dir, code = shiny_cmd)
address <- sprintf("http://localhost:3148")

test_that("WorldBank renders in a shiny app", {
  Sys.sleep(10) # give shiny a second to do it's thing
  remDr$navigate(address)
  Sys.sleep(10)
  ## just check that svg is displayed
  html <- getHTML()
  circles <- getNodeSet(html, "//div[@id='animint']//circle")
  expect_true(length(circles) >= 1)
})

getYear <- function(){
  node.set <- getNodeSet(getHTML(), '//g[@class="geom10_text_ts"]//text')
  expect_equal(length(node.set), 1)
  value <- xmlValue(node.set[[1]])
  sub("year = ", "", value)
}

test_that("animation updates", {
  old.year <- getYear()
  Sys.sleep(5) #wait for two animation frames.
  new.year <- getYear()
  expect_true(old.year != new.year)
})

getCountries <- function(){
  country.labels <- getNodeSet(getHTML(), '//g[@class="geom9_text_ts"]//text')
  sort(sapply(country.labels, xmlValue))
}

test_that("clicking selects country", {
  old.countries <- getCountries()
  expect_identical(old.countries, c("United States", "Vietnam"))
  clickID("Bahrain")
  new.countries <- getCountries()
  expect_identical(new.countries, c("Bahrain", "United States", "Vietnam"))
})

getFacets <- function(){
  facets <- getNodeSet(getHTML(), '//g[@class="topStrip"]//text')
  sapply(facets, xmlValue)
}

test_that("shiny changes axes", {
  old.facets <- getFacets()
  expect_identical(old.facets, c("fertility.rate", "Years"))
  e <- remDr$findElement("class name", "selectize-input")
  ## This click and sendKeys is just to make sure we have focus on the
  ## first selectize element.
  e$clickElement()
  e$sendKeysToElement(list(key="backspace"))
  e$clickElement() # hide menu
  e$clickElement() # show menu
  remDr$sendKeysToActiveElement(list(key="backspace"))
  remDr$sendKeysToActiveElement(list("lite"))
  remDr$sendKeysToActiveElement(list(key="enter"))
  Sys.sleep(10)
  new.facets <- getFacets()
  expect_identical(new.facets, c("literacy", "Years"))
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

## go back to non-shiny tests
remDr$navigate(old_address)


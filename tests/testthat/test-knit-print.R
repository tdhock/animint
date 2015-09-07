context("knitting multiple animint plots in a single Rmd")

knitr::knit_meta() #clear knitr 'metadata'
test_file <- system.file("examples", "test_knit_print.Rmd", 
                         package = "animint")
setwd("animint-htmltest")
knitr::knit2html(input = test_file, output = "index.html")
setwd("..")
remDr$refresh()
html <- getHTML()

test_that("knit_print.animint works as intended", {
  nodes <- getNodeSet(html, "//text[@id='xtitle']")
  xlabel1 <- xmlValue(nodes[[1]])
  expect_match(xlabel1, "Worthless label 1")
  xlabel2 <- xmlValue(nodes[[2]])
  expect_match(xlabel2, "Worthless label 2")
})

test_that("legend interactivity works in an Rmd document", {
  ## function to extract all circles from an HTML page
  get_circles <- function(id) {
    getNodeSet(getHTML(), "//circle[@class='geom']")
  }
  
  # 10 circles on the plot
  expect_equal(length(get_circles()), 10)
  
  # only 5 circles on the plot after clicking
  clickID("a178")
  expect_equal(length(get_circles()), 5)
  
  # clicking again goes back to 10 plot circles
  clickID("a178")
  expect_equal(length(get_circles()), 10)
  
})


context("knitting multiple animint plots in a single Rmd")

knitr::knit_meta() #clear knitr 'metadata'
test_file <- system.file("examples", "test_knit_print.Rmd", 
                         package = "animint")

setwd("animint-htmltest")
knitr::knit2html(input = test_file, output = "index.html")
setwd("..")
remDr$refresh()
html <- getHTML()

test_that("knit_print.animint renders three plots", {
  nodes <- getNodeSet(html, "//text[@id='xtitle']")
  value.vec <- sapply(nodes, xmlValue)
  expected.vec <-
    c("first plot with color legend",
      "second plot with color legend",
      "non-interactive plot")
  expect_identical(value.vec, expected.vec)
})

## function to extract all circles from an HTML page
get_circles <- function(id) {
  getNodeSet(getHTML(), "//circle[@class='geom']")
}

get_elements <- function(id){
  div <- remDr$findElement("id", id)
  list(a178=div$findChildElement("id", "a178"),
       b934=div$findChildElement("id", "b934"))
}

plot1top <- get_elements("plot1top")
plot1bottom <- get_elements("plot1bottom")

test_that("clicking top legend adds/remove points", {
  expect_equal(length(get_circles()), 20)
  plot1top$a178$clickElement()
  expect_equal(length(get_circles()), 15)
  plot1top$b934$clickElement()
  expect_equal(length(get_circles()), 10)
  plot1top$b934$clickElement()
  expect_equal(length(get_circles()), 15)
  plot1top$a178$clickElement()
  expect_equal(length(get_circles()), 20)
})

test_that("clicking bottom legend adds/remove points", {
  expect_equal(length(get_circles()), 20)
  plot1bottom$a178$clickElement()
  expect_equal(length(get_circles()), 15)
  plot1bottom$b934$clickElement()
  expect_equal(length(get_circles()), 10)
  plot1bottom$b934$clickElement()
  expect_equal(length(get_circles()), 15)
  plot1bottom$a178$clickElement()
  expect_equal(length(get_circles()), 20)
})


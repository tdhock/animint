context("knitting multiple animint plots in a single Rmd")

test_that("knit_print.animint works as intended", {
  knitr::knit_meta() #clear knitr 'metadata'
  test_file <- system.file("examples", "test_knit_print.Rmd", 
                          package = "animint")
  setwd("animint-htmltest")
  knitr::knit2html(input = test_file, output = "index.html")
  setwd("..")
  remDr$refresh()
  html <- getHTML()
  nodes <- getNodeSet(html, "//text[@id='xtitle']")
  xlabel1 <- xmlValue(nodes[[1]])
  expect_match(xlabel1, "Worthless label 1")
  xlabel2 <- xmlValue(nodes[[2]])
  expect_match(xlabel2, "Worthless label 2")
})


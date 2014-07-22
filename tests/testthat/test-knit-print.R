context("knitting multiple animint plots in a single Rmd")

test_that("knit_print.animint works as intended", {
  library(knitr)
  knit_meta() #clear knitr 'metadata'
  knit2html(input = system.file("examples", "test_knit_print.Rmd", package = "animint"),
            output = "index.html")
  remDr$navigate("http://localhost:4848/testthat/")
  html <- XML::htmlParse(remDr$getPageSource(), asText = TRUE)
  nodes <- getNodeSet(html, "//g[@id='xaxis']//text[@class='label']")
  xlabel1 <- xmlValue(nodes[[1]])
  expect_match(xlabel1, "Worthless label 1")
  xlabel2 <- xmlValue(nodes[[2]])
  expect_match(xlabel2, "Worthless label 2")
})


context("knitting multiple animint plots in a single Rmd")

test_that("knit_print.animint works as intended", {
  library(knitr)
  knit_meta() #clear knitr 'metadata'
  knit2html(input = system.file("examples", "test_knit_print.Rmd", package = "animint"),
            output = "index.html")
  unlink("test_knit_print.md")
  remDr$navigate("http://localhost:4848/index.html")
  html <- XML::htmlParse(remDr$getPageSource(), asText = TRUE)
  nodes <- getNodeSet(html, "//text[@id='xtitle']")
  xlabel1 <- xmlValue(nodes[[1]])
  expect_match(xlabel1, "Worthless label 1")
  xlabel2 <- xmlValue(nodes[[2]])
  expect_match(xlabel2, "Worthless label 2")
  unlink("index.html")
  unlink("plot1", recursive = TRUE)
  unlink("plot2", recursive = TRUE)
})


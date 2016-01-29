acontext("knitting multiple animint plots in a single Rmd")

knitr::knit_meta() #clear knitr 'metadata'
test_file <- system.file("examples", "test_knit_print.Rmd", 
                         package = "animint")

setwd("animint-htmltest")
rmarkdown::render(input = test_file, output_file = "index.html")
setwd("..")
remDr$refresh()
html <- getHTML()

test_that("knit_print.animint renders three plots", {
  nodes <- getNodeSet(html, "//text[@class='xtitle']")
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
  ##print("before div")
  div <- remDr$findElement("id", id)
  ## For debugging a NoSuchElement error I insert print statements.
  ##print("before css selector")
  tr.list <- div$findChildElements("css selector", "table.legend tr.col")
  a <- tr.list[[1]]
  b <- tr.list[[2]]
  ##print("before show_hide")
  show_hide <- div$findChildElement("class name", "show_hide_selector_widgets")
  ##print("before col_selector_widget")
  col.w <- div$findChildElement("class name", "col_selector_widget")
  list(a178=a,
       b934=b,
       show_hide=show_hide,
       col_widget=col.w)
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

plot1top$show_hide$clickElement()
s.div <- plot1top$col_widget$findChildElement("class name", "selectize-input")
s.div$clickElement()

test_that("clicking top legend adds/remove points", {
  expect_equal(length(get_circles()), 20)
  remDr$sendKeysToActiveElement(list(key="backspace"))
  expect_equal(length(get_circles()), 15)
  remDr$sendKeysToActiveElement(list(key="backspace"))
  expect_equal(length(get_circles()), 10)
  remDr$sendKeysToActiveElement(list("a", key="enter"))
  expect_equal(length(get_circles()), 15)
  remDr$sendKeysToActiveElement(list("b", key="enter"))
  expect_equal(length(get_circles()), 20)
})

plot1bottom$show_hide$clickElement()
s.div <-
  plot1bottom$col_widget$findChildElement("class name", "selectize-input")
s.div$clickElement()

test_that("clicking top legend adds/remove points", {
  expect_equal(length(get_circles()), 20)
  remDr$sendKeysToActiveElement(list(key="backspace"))
  expect_equal(length(get_circles()), 15)
  remDr$sendKeysToActiveElement(list(key="backspace"))
  expect_equal(length(get_circles()), 10)
  remDr$sendKeysToActiveElement(list("a", key="enter"))
  expect_equal(length(get_circles()), 15)
  remDr$sendKeysToActiveElement(list("b", key="enter"))
  expect_equal(length(get_circles()), 20)
})


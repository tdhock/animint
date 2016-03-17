acontext("knitting multiple animint plots in a single Rmd")

knitr::knit_meta() #clear knitr 'metadata'
Rmd.file <- "~/R/animint/inst/examples/test_knit_print.Rmd"
Rmd.file <- system.file("examples", "test_knit_print.Rmd", 
                         package = "animint")
index.file <- file.path("animint-htmltest", "index.Rmd")

file.copy(Rmd.file, index.file, overwrite=TRUE)
## https://github.com/rstudio/rmarkdown/issues/587#issuecomment-168437646
## @yihui says "Do not use the output_dir argument of render()"
rmarkdown::render(index.file)
remDr$refresh()
Sys.sleep(1)
html <- getHTML()

test_that("knit_print.animint renders five x axis titles", {
  nodes <- getNodeSet(html, "//text[@class='xtitle']")
  value.vec <- sapply(nodes, xmlValue)
  expected.vec <-
    c("first plot with color legend",
      "second plot with color legend",
      "non-interactive plot",
      "position",
      "segments")
  expect_identical(value.vec, expected.vec)
})

test_that("segments and breakpoints are rendered", {
  seg.list <- getNodeSet(html, "//g[@class='geom3_segment_signal']//line")
  expect_equal(length(seg.list), 6)
  break.list <- getNodeSet(html, "//g[@class='geom4_vline_signal']//line")
  expect_equal(length(break.list), 5)
})

test_that("svg id property is unique", {
  svg.list <- getNodeSet(html, "//svg")
  attr.mat <- sapply(svg.list, xmlAttrs)
  id.counts <- table(attr.mat["id",])
  expect_true(all(id.counts==1))
})

## function to extract all circles from an HTML page
get_circles <- function(html=getHTML()) {
  plot.names <- c("plot1top", "plot1bottom")
  count.vec <- c()
  for(i in seq_along(plot.names)){
    xpath <- sprintf("//div[@id='%s']//circle[@class='geom']", plot.names[[i]])
    circle.list <- getNodeSet(html, xpath)
    count.vec[[i]] <- length(circle.list)
  }
  count.vec
}

get_elements <- function(id){
  ##print("before div")
  div <- remDr$findElement("id", id)
  ## For debugging a NoSuchElement error I insert print statements.
  ##print("before css selector")
  tr.list <- div$findChildElements(
    "css selector", "table.legend tr.label_variable")
  a <- tr.list[[1]]
  b <- tr.list[[2]]
  ##print("before show_hide")
  show_hide <- div$findChildElement("class name", "show_hide_selector_widgets")
  ##print("before col_selector_widget")
  widget <- div$findChildElement("class name", "label_variable_selector_widget")
  list(a178=a,
       b934=b,
       show_hide=show_hide,
       widget=widget)
}

plot1top <- get_elements("plot1top")
plot1bottom <- get_elements("plot1bottom")

test_that("clicking top legend adds/remove points", {
  expect_equal(get_circles(), c(10, 10))
  plot1top$a178$clickElement()
  expect_equal(get_circles(), c(5, 10))
  plot1top$b934$clickElement()
  expect_equal(get_circles(), c(0, 10))
  plot1top$b934$clickElement()
  expect_equal(get_circles(), c(5, 10))
  plot1top$a178$clickElement()
  expect_equal(get_circles(), c(10, 10))
})

test_that("clicking bottom legend adds/remove points", {
  expect_equal(get_circles(), c(10, 10))
  plot1bottom$a178$clickElement()
  expect_equal(get_circles(), c(10, 5))
  plot1bottom$b934$clickElement()
  expect_equal(get_circles(), c(10, 0))
  plot1bottom$b934$clickElement()
  expect_equal(get_circles(), c(10, 5))
  plot1bottom$a178$clickElement()
  expect_equal(get_circles(), c(10, 10))
})

plot1top$show_hide$clickElement()
s.div <- plot1top$widget$findChildElement("class name", "selectize-input")
s.div$clickElement()

test_that("top widget adds/remove points", {
  expect_equal(get_circles(), c(10, 10))
  remDr$sendKeysToActiveElement(list(key="backspace"))
  expect_equal(get_circles(), c(5, 10))
  remDr$sendKeysToActiveElement(list(key="backspace"))
  expect_equal(get_circles(), c(0, 10))
  remDr$sendKeysToActiveElement(list("a", key="enter"))
  expect_equal(get_circles(), c(5, 10))
  remDr$sendKeysToActiveElement(list("b", key="enter"))
  expect_equal(get_circles(), c(10, 10))
})

plot1bottom$show_hide$clickElement()
s.div <-
  plot1bottom$widget$findChildElement("class name", "selectize-input")
s.div$clickElement()

test_that("bottom widget adds/remove points", {
  expect_equal(get_circles(), c(10, 10))
  remDr$sendKeysToActiveElement(list(key="backspace"))
  expect_equal(get_circles(), c(10, 5))
  remDr$sendKeysToActiveElement(list(key="backspace"))
  expect_equal(get_circles(), c(10, 0))
  remDr$sendKeysToActiveElement(list("a", key="enter"))
  expect_equal(get_circles(), c(10, 5))
  remDr$sendKeysToActiveElement(list("b", key="enter"))
  expect_equal(get_circles(), c(10, 10))
})

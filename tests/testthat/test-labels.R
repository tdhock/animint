context <- "labels"
context(context)

# Setup a directory specific to this context
# Note this should be running in tests/testthat
current.dir <- file.path(getwd(), context)
if (!file_test("-d", current.dir)) dir.create(current.dir)
# Remove the directory when the test is done
on.exit(unlink(current.dir, recursive = TRUE))

# Establish remote driver connection via RSelenium and close it when this context is over
#source(system.file('tests', 'connect.R', package = 'animint'), local = TRUE)
#on.exit(remDr$close())

# create some objects that will be reused
plot <- ggplot() + geom_point(aes(Petal.Width, Sepal.Width), data = iris)
label1 <- "My amazing plot!"
label2 <- "Sepal Width"
label3 <- "Petal Width"
label4 <- "Petal Length"
p1 <- plot + ggtitle(label1)
p2 <- plot + ylab(label2)
p3 <- plot + scale_x_continuous(label3)
p4 <- ggplot() + geom_point(aes(Petal.Length, Sepal.Length), data = iris) +
  scale_x_continuous(breaks = c(1.5, 6.5)) + xlab(label4)

source("functions.R")

# Used to test SVG elements
info1 <- gg2html(p1, context, "ggtitle")
info2 <- gg2html(p2, context, "ylab")
info3 <- gg2html(p3, context, "scale_name")
info4 <- gg2html(p4, context, "scale_breaks")  
# Navigate to and parse page source of all the plots
html1 <- parse_page(info1)
html2 <- parse_page(info2)
html3 <- parse_page(info3)
html4 <- parse_page(info4)

test_that("ggtitle converts to list", {
  expect_identical(info1$plots$scatter$title, label1)
})

test_that("ggtitle converts to SVG", {
  ptitle <- getNodeSet(html1, "//text[@id='plottitle']")
  expect_identical(xmlValue(ptitle[[1]]), label1)
})

test_that("ylab converts to list", {
  expect_identical(info2$plots$scatter$axis$yname, label2)
})

test_that("ylab converts to SVG", {
  ylabel <- getNodeSet(html2, "//g[@id='yaxis']/text")
  expect_identical(xmlValue(ylabel[[1]]), label2)
})

test_that("scale_x_continuous(name) converts to list", {
  expect_identical(info3$plots$scatter$axis$xname, label3)
})

test_that("scale_x_continuous(name) converts to SVG", {
  xlabel <- getNodeSet(html3, "//g[@id='xaxis']/text")
  expect_identical(xmlValue(xlabel[[1]]), label3)
})

test_that("scale_x_continuous(breaks)+xlab(name) converts to list", {
  expect_identical(info4$plots$scatter$axis$xname, label4)
  expect_identical(info4$plots$scatter$axis$xlab, c("1.5", "6.5"))
})

test_that("scale_x_continuous(breaks)+xlab(name) converts to SVG", {
  xlabel <- getNodeSet(html4, "//g[@id='xaxis']/text")
  expect_identical(xmlValue(xlabel[[1]]), label4)
  xticks <- getNodeSet(html4, "//g[@id='xaxis']/g[@class='tick major']")
  expect_identical(sapply(xticks, xmlValue), c("1.5", "6.5"))
})

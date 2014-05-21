context <- "labels"
context(context)

# Setup a directory specific to this context
# Note this should be running in tests/testthat
current.dir <- file.path(getwd(), context)
if (!file_test("-d", current.dir)) dir.create(current.dir)
# Remove the directory when this context is done
on.exit(unlink(current.dir, recursive = TRUE))

# create some objects that will be reused
plot <- ggplot() + geom_point(aes(Petal.Width, Sepal.Width), data = iris)
p1 <- plot + ggtitle("My amazing plot!")
p2 <- plot + ylab("Sepal Width")
p3 <- plot + scale_x_continuous("Petal Width")
p4 <- ggplot() + geom_point(aes(Petal.Length, Sepal.Length), data = iris) +
  scale_x_continuous(breaks = c(1.5, 6.5)) + xlab("Petal Length")

# Used to test SVG elements
info1 <- animint2HTML(list(scatter = p1), context, "ggtitle")
info2 <- animint2HTML(list(scatter = p2), context, "ylab")
info3 <- animint2HTML(list(scatter = p3), context, "scale_name")
info4 <- animint2HTML(list(scatter = p4), context, "scale_breaks")  
html1 <- parse_page(info1)
html2 <- parse_page(info2)
html3 <- parse_page(info3)
html4 <- parse_page(info4)

test_that("ggtitle converts to list", {
  expect_identical(info1$plots$scatter$title, "My amazing plot!")
})

test_that("ggtitle converts to SVG", {
  ptitle <- getNodeSet(html1, "//text[@id='plottitle']")
  expect_identical(xmlValue(ptitle[[1]]), "My amazing plot!")
})

test_that("ylab converts to list", {
  expect_identical(info2$plots$scatter$axis$yname, "Sepal Width")
})

test_that("ylab converts to SVG", {
  ylabel <- getNodeSet(html2, "//g[@id='yaxis']/text")
  expect_identical(xmlValue(ylabel[[1]]), "Sepal Width")
})

test_that("scale_x_continuous(name) converts to list", {
  expect_identical(info3$plots$scatter$axis$xname, "Petal Width")
})

test_that("scale_x_continuous(name) converts to SVG", {
  xlabel <- getNodeSet(html3, "//g[@id='xaxis']/text")
  expect_identical(xmlValue(xlabel[[1]]), "Petal Width")
})

test_that("scale_x_continuous(breaks)+xlab(name) converts to list", {
  expect_identical(info4$plots$scatter$axis$xname, "Petal Length")
  expect_identical(info4$plots$scatter$axis$xlab, c("1.5", "6.5"))
})

test_that("scale_x_continuous(breaks)+xlab(name) converts to SVG", {
  xlabel <- getNodeSet(html4, "//g[@id='xaxis']/text")
  expect_identical(xmlValue(xlabel[[1]]), "Petal Length")
  xticks <- getNodeSet(html4, "//g[@id='xaxis']/g[@class='tick major']")
  expect_identical(sapply(xticks, xmlValue), c("1.5", "6.5"))
})
context("Labels")

# create some objects that will be reused
ggpoint <- ggplot() + geom_point(aes(Petal.Width, Sepal.Width), data = iris)

test_that("ggtitle converts", {
  viz <- list(scatter=ggpoint + ggtitle("My amazing plot!"))
  info <- animint2HTML(viz)
  expect_identical(info$plots$scatter$title, "My amazing plot!")
  
  ptitle <- getNodeSet(info$html, "//text[@id='plottitle']")
  expect_identical(xmlValue(ptitle[[1]]), "My amazing plot!")
})

test_that("ylab converts", {
  viz <- list(scatter=ggpoint + ylab("Sepal Width"))
  info <- animint2HTML(viz)
  expect_identical(info$plots$scatter$ytitle, "Sepal Width")
  ylabel <- getNodeSet(info$html, "//text[@id='ytitle']")
  expect_identical(xmlValue(ylabel[[1]]), "Sepal Width")
})

test_that("scale_x_continuous(name) converts", {
  viz <- list(scatter=ggpoint + scale_x_continuous("Petal Width"))
  info <- animint2HTML(viz)
  expect_identical(info$plots$scatter$xtitle, "Petal Width")
  xlabel <- getNodeSet(info$html, "//text[@id='xtitle']")
  expect_identical(xmlValue(xlabel[[1]]), "Petal Width")
})

test_that("scale_x_continuous(breaks)+xlab(name) converts", {
  viz <-
    list(scatter=ggplot() +
           geom_point(aes(Petal.Length, Sepal.Length), data = iris) +
           scale_x_continuous(breaks = c(1.5, 6.5)) +
           xlab("Petal Length"))
  
  info <- animint2HTML(viz)  
  
  expect_identical(info$plots$scatter$xtitle, "Petal Length")
  expect_identical(info$plots$scatter$axis$xlab, c("1.5", "6.5"))
  
  xlabel <- getNodeSet(info$html, "//text[@id='xtitle']")
  expect_identical(xmlValue(xlabel[[1]]), "Petal Length")
  xticks <- getNodeSet(info$html, "//g[@id='xaxis']/g[@class='tick major']")
  expect_identical(sapply(xticks, xmlValue), c("1.5", "6.5"))
})

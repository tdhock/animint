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


stocks <- data.frame(
  time = as.Date('2009-01-01') + 0:89,
  vars = rep(c("A", "B", "C"), 30),
  value = rnorm(90)
)

series <- ggplot() + geom_line(aes(x = time, y = value, group = vars), data = stocks)

test_that("scale_x_time ticks/labels work", { 
  info <- animint2HTML(list(series = series))
  xticks <- getNodeSet(info$html, "//g[@id='xaxis']/g[@class='tick major']")
  expect_true(length(xticks) > 1)
})

getTickText <- function(html, id){
  xpath <- sprintf("//g[@id='%s']//g[@class='tick major']//text", id)
  nodes <- getNodeSet(html, xpath)
  sapply(nodes, xmlValue)
}

is.blank <- function(ticks){
  all(ticks == "")
}

test_that("plot renders with theme(axis.text.x=element_blank())", {
  viz <- list(series=series+theme(axis.text.x=element_blank()))
  info <- animint2HTML(viz)
  xticks <- getTickText(info$html, "xaxis")
  expect_true(is.blank(xticks))
  yticks <- getTickText(info$html, "yaxis")
  expect_true(!is.blank(yticks))
})

test_that("plot renders with theme(axis.text.y=element_blank())", {
  viz <- list(series=series+theme(axis.text.y=element_blank()))
  info <- animint2HTML(viz)
  xticks <- getTickText(info$html, "xaxis")
  expect_true(!is.blank(xticks))
  yticks <- getTickText(info$html, "yaxis")
  expect_true(is.blank(yticks))
})

test_that("plot renders with theme(axis.text=element_blank())", {
  viz <- list(series=series+theme(axis.text=element_blank()))
  info <- animint2HTML(viz)
  xticks <- getTickText(info$html, "xaxis")
  expect_true(is.blank(xticks))
  yticks <- getTickText(info$html, "yaxis")
  expect_true(is.blank(yticks))
})


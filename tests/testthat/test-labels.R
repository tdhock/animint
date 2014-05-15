context("labels")

test_that("ggtitle is translated correctly", {
  viz <- list(scatter=ggplot()+
    geom_point(aes(Petal.Width, Sepal.Width), data=iris)+
    ggtitle("My amazing plot!"))
  info <- gg2animint(viz, open.browser=FALSE)
  expect_identical(info$plots$scatter$title, "My amazing plot!")
})

test_that("ylab is translated correctly", {
  viz <- list(scatter=ggplot()+
    geom_point(aes(Petal.Width, Sepal.Width), data=iris)+
    ylab("sepal width"))
  info <- gg2animint(viz, open.browser=FALSE)
  expect_identical(info$plots$scatter$axis$yname, "sepal width")
})

test_that("scale_x_continuous(name) is translated correctly", {
  viz <- list(scatter=ggplot()+
    geom_point(aes(Petal.Width, Sepal.Width), data=iris)+
    scale_x_continuous("petal width"))
  info <- gg2animint(viz, open.browser=FALSE)
  expect_identical(info$plots$scatter$axis$xname, "petal width")
})

test_that("scale_x_continuous(breaks)+xlab(name) is translated correctly", {
  viz <- list(scatter=ggplot()+
    geom_point(aes(Petal.Length, Sepal.Length), data=iris)+
    scale_x_continuous(breaks=c(1.5, 6.5))+
    xlab("petal length"))
  info <- gg2animint(viz, open.browser=FALSE)
  expect_identical(info$plots$scatter$axis$xname, "petal length")
  expect_identical(info$plots$scatter$axis$xlab, c("1.5", "6.5"))
})

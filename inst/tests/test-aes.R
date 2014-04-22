context("aesthetics")

test_that("we stop when data does not contain interactive variables",{
  foo <- 1
  gg <- ggplot()+
    geom_point(aes(Sepal.Length, Petal.Length, showSelected=foo),
               data=iris)
  viz <- list(scatter=gg)
  expect_that({
    info <- gg2animint(viz, open.browser=FALSE)
  }, throws_error("data does not have interactive variables"))
})

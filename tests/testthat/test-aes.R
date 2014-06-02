context("aesthetics")

test_that("we stop when data does not contain interactive variables",{
  ## In interactive testing, foo will be found and copied to
  ## ggplot_build(gg)$data, but on R CMD check, animint2dir only has
  ## access to its environment, so it will not have access to foo
  ## defined in the global environment, and so calling ggplot_build on
  ## this plot from inside animint2dir will result in a "foo not found"
  ## error. However, animint should check for the validity of its
  ## interactive variables BEFORE calling ggplot_build, so below we
  ## should generate an animint error, not a ggplot_build error.
  foo <- 1 
  gg <- ggplot()+
    geom_point(aes(Sepal.Length, Petal.Length, showSelected=foo),
               data=iris)
  viz <- list(scatter=gg)
  expect_that({
    info <- animint2dir(viz, open.browser=FALSE)
  }, throws_error("data does not have interactive variables"))
})

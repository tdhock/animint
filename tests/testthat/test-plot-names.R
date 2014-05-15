context("plot names")

gg <- qplot(Petal.Width, Sepal.Width, data=iris)

test_that("error if we refer to non-existent plot names", {
  viz <- list(irisPlot=gg, width=list(foo=1000))
  expect_error({
    gg2animint(viz)
  }, "no ggplot named foo")
  viz <- list(OK=gg, height=list(bar=1000))
  expect_error({
    gg2animint(viz)
  }, "no ggplot named bar")
})
  
test_that("non-alphanumeric plot names are not allowed", {
  viz <- list(fooo.bar=gg)
  expect_error({
    gg2animint(viz)
  }, "ggplot names must match ^[a-zA-Z][a-zA-Z0-9]*$", fixed=TRUE)
})

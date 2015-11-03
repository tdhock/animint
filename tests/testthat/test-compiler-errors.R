acontext("compiler errors")

test_that("aes(showSelected=var1, showSelected=var2) is an error", {
  viz <- list(
    petals=ggplot()+
      geom_point(aes(Petal.Width, Petal.Length,
                     showSelected=Species,
                     showSelected=Sepal.Width),
                 data=iris)
    )
  expect_error({
    animint2dir(viz)
  }, "aes names must be unique, problems: showSelected")
})

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

test_that("informative error for time option with no showSelected", {
  viz <- list(
    petals=ggplot()+
      geom_point(aes(Petal.Width, Petal.Length),
                 data=iris),
    time=list(variable="Species", ms=3000)
  )
  expect_error({
    animint2dir(viz)
  }, "no interactive aes for time variable Species")
})

test_that("no error for time option with clickSelects", {
  viz <- list(
    petals=ggplot()+
      geom_point(aes(Petal.Width, Petal.Length, clickSelects=Species),
                 data=iris),
    time=list(variable="Species", ms=3000)
  )
  info <- animint2dir(viz)
})

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
    animint2dir(viz, open.browser=FALSE)
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
    animint2dir(viz, open.browser=FALSE)
  }, "no interactive aes for time variable Species")
})

test_that("no error for time option with clickSelects", {
  viz <- list(
    petals=ggplot()+
      geom_point(aes(Petal.Width, Petal.Length, clickSelects=Species),
                 data=iris),
    time=list(variable="Species", ms=3000)
  )
  info <- animint2dir(viz, open.browser=FALSE)
})

test_that("no error for time option with showSelected", {
  viz <- list(
    petals=ggplot()+
      geom_point(aes(Petal.Width, Petal.Length, showSelected=Species),
                 data=iris),
    time=list(variable="Species", ms=3000)
  )
  info <- animint2dir(viz, open.browser=FALSE)
})

test_that("no error for time option with color", {
  viz <- list(
    petals=ggplot()+
      geom_point(aes(Petal.Width, Petal.Length, color=Species),
                 data=iris),
    time=list(variable="Species", ms=3000)
  )
  info <- animint2dir(viz, open.browser=FALSE)
})

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

data("WorldBank", package="animint")
viz.no.duration <- list(
  scatter=ggplot()+
    geom_point(aes(x=life.expectancy, y=fertility.rate, color=region,
                   key=country,
                   showSelected=year,
                   clickSelects=country),
               data=WorldBank)+
    geom_text(aes(x=life.expectancy, y=fertility.rate, label=country,
                  showSelected=year,
                  showSelected2=country,
                  showSelected3=region,
                  clickSelects=country),
              data=WorldBank),
  first=list(
    year=1970,
    country=c("Canada", "India", "Pakistan", "Japan"),
    region=c("North America", "South Asia")),
  selector.types=list(country="multiple")
)

test_that("no warning for no duration vars", {
  expect_no_warning({
    info <- animint2dir(viz.no.duration, open.browser=FALSE)
  })
})

test_that("warn no key for geom_text with showSelected=duration var", {
  viz.duration <- viz.no.duration
  viz.duration$duration <- list(year=2000)
  expect_warning({
    info <- animint2dir(viz.duration, open.browser=FALSE)
  }, "to ensure that smooth transitions are interpretable, aes(key) should be specifed for geoms with aes(showSelected=year)")
})

viz.key.duration <- list(
  scatter=ggplot()+
    geom_point(aes(x=life.expectancy, y=fertility.rate, color=region,
                   key=country,
                   showSelected=year,
                   clickSelects=country),
               data=WorldBank)+
    geom_text(aes(x=life.expectancy, y=fertility.rate, label=country,
                  showSelected=year,
                  showSelected2=country,
                  showSelected3=region,
                  key=country,
                  clickSelects=country),
              data=WorldBank),
  first=list(
    year=1970,
    country=c("Canada", "India", "Pakistan", "Japan"),
    region=c("North America", "South Asia")),
  selector.types=list(country="multiple"),
  duration=list(year=2000)
)
test_that("no warning when key specified", {
  expect_no_warning({
    info <- animint2dir(viz.key.duration, open.browser=FALSE)
  })
})

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
  }, "to ensure that smooth transitions are interpretable, aes(key) should be specifed for geoms with aes(showSelected=year), problem: geom2_text_scatter", fixed=TRUE)
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

test_that("warning for position=stack and showSelected", {
  set.seed(1)
  df <- data.frame(
    letter = c(replicate(4, LETTERS[1:5])),
    count = c(replicate(4, rbinom(5, 50, 0.5))),
    stack = rep(rep(1:2, each = 5), 2),
    facet = rep(1:2, each = 10)
  )
  gg <- ggplot() +
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    geom_bar(
      aes(letter, count, fill = stack, showSelected=facet,
          key=paste(stack, letter)),
      data = df,
      stat = "identity",
      position="stack"
    )
  complicated <- list(
    plot = gg,
    time = list(variable = "facet", ms = 1000),
    duration = list(facet = 1000)
  )
  expect_warning({
    animint2dir(complicated, open.browser=FALSE)
  }, "showSelected only works with position=identity, problem: geom1_bar_plot")
})

test_that("no warning for position=stack without showSelected", {
  set.seed(1)
  df <- data.frame(
    letter = c(replicate(4, LETTERS[1:5])),
    count = c(replicate(4, rbinom(5, 50, 0.5))),
    stack = rep(rep(1:2, each = 5), 2),
    facet = rep(1:2, each = 10)
  )
  gg <- ggplot() +
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    geom_bar(
      aes(letter, count, fill = stack),
      data = df,
      stat = "identity",
      position="stack"
    )
  no.show <- list(
    plot = gg
  )
  expect_no_warning({
    animint2dir(no.show, open.browser=FALSE)
  })
})


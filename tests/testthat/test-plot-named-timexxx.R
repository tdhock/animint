context("plot named timexxx")

data(WorldBank)
not.na <- subset(WorldBank, !(is.na(life.expectancy) | is.na(fertility.rate)))
subset(not.na, is.na(not.na$population))
not.na[not.na$country=="Kuwait", "population"] <- 1700000
viz <-
  list(scatter=ggplot()+
       geom_point(aes(life.expectancy, fertility.rate, clickSelects=country,
                      showSelected=year, colour=region, size=population),
                  data=not.na)+
       geom_text(aes(life.expectancy, fertility.rate, label=country,
                     showSelected=country, showSelected2=year),
                 data=not.na)+
       scale_size_animint(breaks=10^(5:9))+
       make_text(WorldBank, 55, 9, "year"),

       timeSeries=ggplot()+
       make_tallrect(WorldBank, "year")+
       geom_line(aes(year, life.expectancy, group=country, colour=region,
                     clickSelects=country),
                 data=WorldBank, size=4, alpha=3/5),

       duration=list(year=1000))

test_that("plot named timeSeries is OK without time option list", {
  animint2dir(viz, open.browser = FALSE)
})

viz.time <- viz
viz.time$time <- list(ms=2000, variable="year")

test_that("plot named timeSeries is OK with time option list", {
  animint2dir(viz.time, open.browser = FALSE)
})

bad <-
  list(scatter=ggplot()+
       geom_point(aes(life.expectancy, fertility.rate, clickSelects=country,
                      showSelected=year, colour=region, size=population),
                  data=not.na)+
       geom_text(aes(life.expectancy, fertility.rate, label=country,
                     showSelected=country, showSelected2=year),
                 data=not.na)+
       scale_size_animint(breaks=10^(5:9))+
       make_text(WorldBank, 55, 9, "year"),

       time=ggplot()+
       make_tallrect(WorldBank, "year")+
       geom_line(aes(year, life.expectancy, group=country, colour=region,
                     clickSelects=country),
                 data=WorldBank, size=4, alpha=3/5),

       duration=list(year=1000))

test_that("plot named time is NOT OK", {
  expect_error({
    animint2dir(bad, open.browser = FALSE)
  }, "time option list needs ms, variable")
})


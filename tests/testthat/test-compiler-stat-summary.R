acontext("stat_summary")

test_that("stat_summary does not infinitely recurse", {
  data(UStornadoes, package = "animint")
  gg <- ggplot()+
    stat_summary(aes(year, year, clickSelects=year),
                 data=UStornadoes, fun.y=length, geom="bar")
  L <- list(bar=gg)
  info <- animint2dir(L, open.browser=FALSE)
  expect_identical(length(info$geoms), 1L)
})

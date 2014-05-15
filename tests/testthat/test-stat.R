context("stat")

test_that("stat_summary does not infinitely recurse", {
  data(UStornadoes)
  gg <- ggplot()+
    stat_summary(aes(year, year, clickSelects=year),
                 data=UStornadoes, fun.y=length, geom="bar")
  L <- list(bar=gg)
  info <- gg2animint(L, open.browser=FALSE)
  expect_identical(length(info$geoms), 1L)
})

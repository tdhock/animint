context("stat")

test_that("stat_summary does not infinitely recurse", {
  gg <- ggplot()+
    stat_summary(aes(year, year, clickSelects=year),
                 data=UStornadoes, fun.y=length, geom="bar")
  L <- list(bar=gg)
  info <- gg2animint(L)
  expect_identical(length(info$geoms), 1L)
})

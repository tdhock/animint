acontext("selectors")

test_that("first options are copied to selectors", {
  data(WorldBank, package = "animint")
  gapminder <-
    list(ts=ggplot()+
         make_tallrect(WorldBank, "year")+
         geom_line(aes(year, life.expectancy, group=country, color=region,
                       clickSelects=country),
                   data=WorldBank, size=4, alpha=3/5),
         scatter=ggplot()+
         geom_point(aes(fertility.rate, life.expectancy, clickSelects=country,
                        showSelected=year, colour=region, size=population),
                    data=WorldBank)+
         geom_text(aes(fertility.rate, life.expectancy, label=country,
                       showSelected=country, showSelected2=year),
                   data=WorldBank)+
         make_text(WorldBank, 5, 80, "year")+
         scale_size_animint(pixel.range=c(2,20), breaks=10^(4:9)),
         first=list(country="United States", year=1984))
  info <- animint2dir(gapminder, open.browser=FALSE)
  expect_identical(info$selectors$country$selected, "United States")
  expect_identical(info$selectors$year$selected, "1984")
})

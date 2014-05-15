context("text")

data(WorldBank)
wb  <- WorldBank[WorldBank$year == 2010,]
viz <- list(scatter=ggplot()+
  geom_text(aes(y=fertility.rate, x=life.expectancy,
                label=country, size=population, colour=population),
            data=wb)+
  scale_size_continuous(range=c(10,20)))

test_that("text size range is translated to <text font-size>", {
  html <- animint2HTML(viz)
  expect_match(html, 'font-size="10"')
  expect_match(html, 'font-size="20"')
})
              
test_that("text may contain commas and parentheses", {
  html <- animint2HTML(viz)
  expect_match(html, "Bahamas, The")
  expect_match(html, "Yemen, Rep.")
  expect_match(html, "Virgin Islands (U.S.)")
})

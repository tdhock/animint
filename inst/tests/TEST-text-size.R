context("text size")
data(WorldBank)
wb  <- WorldBank[WorldBank$year == 2010,]
viz <- list(scatter=ggplot()+
  geom_text(aes(y=fertility.rate, x=life.expectancy,
                label=iso2c, size=population, colour=population),
            data=wb)+
  scale_size_continuous(range=c(10,20)))

test_that("text size range is translated to <text font-size>", {
  html <- animint2HTML(viz)
  expect_true(grepl('font-size="10"', html))
  expect_true(grepl('font-size="20"', html))
})
              


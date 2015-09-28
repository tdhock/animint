acontext("Text")

data(WorldBank)
wb  <- WorldBank[WorldBank$year == 2010,]
viz <- list(scatter=ggplot()+
  geom_text(aes(y=fertility.rate, x=life.expectancy,
                label=country, size=population, colour=population),
            data=wb)+
  scale_size_continuous(range=c(10,20)))

test_that("text size range translates to <text font-size>", {
  info <- animint2HTML(viz)
  expect_attrs(info$html, 'text[@class="geom"]', "font-size", c("10", "20"))
})
              
test_that("text may contain commas and parentheses", {
  info <- animint2HTML(viz)
  geom <- getNodeSet(info$html, '//text[@class="geom"]')
  txt <- sapply(geom, xmlValue)
  expect_that(any(grepl("\\.", txt)), is_true())
  expect_that(any(grepl("\\(", txt)), is_true())
  expect_that(any(grepl(",", txt)), is_true())
})

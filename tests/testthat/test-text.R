context <- "text"
context(context)

# Setup a directory specific to this context
# Note this should be running in tests/testthat
current.dir <- file.path(getwd(), context)
if (!file_test("-d", current.dir)) dir.create(current.dir)
# Remove the directory when this context is done
on.exit(unlink(current.dir, recursive = TRUE))

data(WorldBank)
wb  <- WorldBank[WorldBank$year == 2010,]
viz <- list(scatter=ggplot()+
  geom_text(aes(y=fertility.rate, x=life.expectancy,
                label=country, size=population, colour=population),
            data=wb)+
  scale_size_continuous(range=c(10,20)))

test_that("text size range translates to <text font-size>", {
  info <- animint2HTML(viz, context, "text-size")
  html <- parse_page(info)
  geom <- getNodeSet(html, '//text[@class="geom"]')
  sizes <- as.numeric(sapply(geom, function(x) xmlAttrs(x)["font-size"]))
  expect_that(min(sizes) == 10, is_true())
  expect_that(max(sizes) == 20, is_true())
})
              
test_that("text may contain commas and parentheses", {
  info <- animint2HTML(viz, context, "text-commas")
  html <- parse_page(info)
  geom <- getNodeSet(html, '//text[@class="geom"]')
  txt <- sapply(geom, xmlValue)
  expect_that(any(grepl("\\.", txt)), is_true())
  expect_that(any(grepl("\\(", txt)), is_true())
  expect_that(any(grepl(",", txt)), is_true())
})

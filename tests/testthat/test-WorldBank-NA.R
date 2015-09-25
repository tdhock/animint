context("WorldBank-NA")

data(WorldBank)

## This example is good because it uses constancy
## http://bost.ocks.org/mike/constancy/
no.time <-
  list(scatter=ggplot()+
       geom_point(aes(life.expectancy, fertility.rate, clickSelects=country,
                      showSelected=year, colour=region, size=population,
                      tooltip=paste(country, "population", population),
                      key=country), # key aesthetic for animated transitions!
                  data=WorldBank)+
       geom_text(aes(life.expectancy, fertility.rate, label=country,
                     showSelected=country, showSelected2=year,
                     key=country), # also use key here!
                 data=WorldBank)+
       scale_size_animint(breaks=10^(5:9))+
       make_text(WorldBank, 55, 9, "year"),
       
       ts=ggplot()+
       make_tallrect(WorldBank, "year")+
       geom_line(aes(year, life.expectancy, group=country, colour=region,
                     clickSelects=country),
                 data=WorldBank, size=4, alpha=3/5),

       bar=ggplot()+
       theme_animint(height=2400)+
       geom_bar(aes(country, life.expectancy, fill=region,
                    key=country,
                    showSelected=year, clickSelects=country),
                data=WorldBank, stat="identity", position="identity")+
       coord_flip(),
       
       duration=list(year=1000),
       
       first=list(year=1975, country="United States"),
       
       title="World Bank data (single selection)")

bar.attributes <- function(html){
  node.set <-
    getNodeSet(info$html, '//g[@class="geom6_bar_bar"]//rect')
  sapply(node.set, xmlAttrs)
}

info <- animint2HTML(no.time)

chunk1.tsv <- file.path("animint-htmltest", "geom6_bar_bar_chunk1.tsv")
chunk1 <- read.table(chunk1.tsv, sep="\t", header=TRUE,
                     comment.char="", quote="")

test_that("chunk1 contains expected columns", {
  expect_identical(names(chunk1), c("xmax", "group"))
})

test_that("chunk1 does not contain NA", {
  not.missing <- !is.na(chunk1)
  expect_true(all(not.missing))
})

common.tsv <- file.path("animint-htmltest", "geom6_bar_bar_chunk_common.tsv")
common <- read.table(common.tsv, sep="\t", header=TRUE,
                     comment.char="", quote="")

test_that("common chunk contains expected columns", {
  expected.cols <-
    c("ymin", "ymax", "xmin", "fill", "key",
      "clickSelects", "showSelectedlegendfill",
      "group")
  expect_identical(sort(names(common)), sort(expected.cols))
})

test_that("common chunk does not contain NA", {
  not.missing <- !is.na(common)
  expect_true(all(not.missing))
})

test_that("bars render without time", {
  at.mat <- bar.attributes(info$html)
  num.vec <- as.numeric(at.mat[c("x", "width", "y", "height"), ])
  expect_true(0 < ncol(at.mat))
  expect_true(all(is.finite(num.vec)))
})

with.time <- no.time
with.time$time <- list(variable="year", ms=3000)
info <- animint2HTML(with.time)

test_that("bars render with time", {
  at.mat <- bar.attributes(info$html)
  num.vec <- as.numeric(at.mat[c("x", "width", "y", "height"), ])
  expect_true(0 < ncol(at.mat))
  expect_true(all(is.finite(num.vec)))
})

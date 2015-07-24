context("WorldBank-good")

data(WorldBank)

not.na <- subset(WorldBank, !(is.na(life.expectancy) | is.na(fertility.rate)))
subset(not.na, is.na(not.na$population))
not.na[not.na$country=="Kuwait", "population"] <- 1700000

## This example is good because it uses constancy
## http://bost.ocks.org/mike/constancy/
no.time <-
  list(scatter=ggplot()+
       geom_point(aes(life.expectancy, fertility.rate, clickSelects=country,
                      showSelected=year, colour=region, size=population,
                      tooltip=paste(country, "population", population),
                      key=country), # key aesthetic for animated transitions!
                  data=not.na)+
       geom_text(aes(life.expectancy, fertility.rate, label=country,
                     showSelected=country, showSelected2=year,
                     key=country), #also use key here!
                 data=not.na)+
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


acontext("aes(tooltip)")

data(WorldBank, package = "animint")
not.na <- subset(WorldBank, !(is.na(life.expectancy) | is.na(fertility.rate)))
country.counts <- table(not.na$year)
years <- data.frame(year=as.numeric(names(country.counts)),
                    countries=as.numeric(country.counts))
viz <-
  list(scatter=ggplot()+
       geom_point(aes(life.expectancy, fertility.rate, clickSelects=country,
                      showSelected=year, colour=region, size=population,
                      tooltip=paste(country, "population", population),
                      key=country), # key aesthetic for animated transitions!
                  data=WorldBank)+
       geom_text(aes(life.expectancy, fertility.rate, label=country,
                     showSelected=country, showSelected2=year,
                     key=country), #also use key here!
                 data=WorldBank)+
       scale_size_animint(breaks=10^(5:9))+
       geom_rect(aes(xmin=45, xmax=70,
                     ymin=8, ymax=10,
                     showSelected=year,
                     tooltip=paste(countries, "not NA in", year)),
                 data=years, color="yellow")+
       geom_rect(aes(xmin=35, xmax=40,
                     ymin=2, ymax=2.5,
                     showSelected=year),
                 data=years, color="orange")+
       geom_text(aes(55, 9, label=paste("year =", year),
                     showSelected=year),
                 data=years),

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

subset(WorldBank, country=="United States" & year == 1975)$population
subset(years, year==1975)

info <- animint2HTML(viz)

test_that("aes(tooltip, clickSelects) means show tooltip", {
  nodes <-
    getNodeSet(info$html, '//g[@class="geom1_point_scatter"]//circle//title')
  tooltips <- sapply(nodes, xmlValue)
  expect_match(tooltips, "population")
})

test_that("aes(clickSelects) means show 'variable value'", {
  nodes <-
    getNodeSet(info$html, '//g[@class="geom7_line_ts"]//path//title')
  tooltips <- sapply(nodes, xmlValue)
  expect_match(tooltips, "country")
})

test_that("aes(tooltip) means show tooltip", {
  nodes <-
    getNodeSet(info$html, '//g[@class="geom3_rect_scatter"]//rect//title')
  tooltips <- sapply(nodes, xmlValue)
  expect_match(tooltips, "not NA")
})

test_that("aes() means show no tooltip", {
  rect.xpath <- '//g[@class="geom4_rect_scatter"]//rect'
  rect.nodes <- getNodeSet(info$html, rect.xpath)
  expect_equal(length(rect.nodes), 1)
  
  title.xpath <- paste0(rect.xpath, '//title')
  title.nodes <- getNodeSet(info$html, title.xpath)
  expect_equal(length(title.nodes), 0)
})

set.seed(1)
viz <- list(
  linetip=ggplot()+
    geom_line(aes(x, y, tooltip=paste("group", g), group=g),
              size=5,
              data=data.frame(x=c(1,2,1,2), y=rnorm(4), g=c(1,1,2,2))))

test_that("line tooltip renders as title", {
  info <- animint2HTML(viz)
  title.nodes <- getNodeSet(info$html, '//g[@class="geom1_line_linetip"]//title')
  value.vec <- sapply(title.nodes, xmlValue)
  expect_identical(value.vec, c("group 1", "group 2"))
})

WorldBank1975 <- WorldBank[WorldBank$year == 1975, ]
ex_plot <- ggplot() +
  geom_point(aes(fertility.rate, life.expectancy, color = region,
                 tooltip = country, href = "https://github.com"),
             data = WorldBank1975)

viz <- list(ex = ex_plot)
info <- animint2HTML(viz)

test_that("tooltip works with href",{
  # Test for bug when points are not rendered with both href + tooltip
  
  # See that points are rendered for every country. This test may need to be
  # changed soon!!! See the viz on the link below:
  # http://bl.ocks.org/faizan-khan-iit/449b84b2e6b55ca5e39ea9aaad97e27e
  # You may notice a point on the very top-left of the viz, outside the plot.
  # These are the points that have NaN as their value in the DOM, arriving from
  # NA values in the tsv. Should be an easy fix. But leaving a comment here
  point_nodes <-
    getNodeSet(info$html, '//g[@class="geom1_point_ex"]//a//circle')
  
  expect_equal(length(point_nodes), length(WorldBank1975$country))
  
  # See that every <a> element has a title (the country name) initially
  title_nodes <-
    getNodeSet(info$html, '//g[@class="geom1_point_ex"]//a//title')
  rendered_titles <- sapply(title_nodes, xmlValue)
  expected_titles <- unique(WorldBank1975$country)
  expect_identical(sort(rendered_titles), sort(expected_titles))
})

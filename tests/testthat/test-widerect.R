context("geom_widerect")

data(WorldBank)
not.na <- subset(WorldBank, !(is.na(life.expectancy) | is.na(fertility.rate)))
BOTH <- function(df, top, side){
  data.frame(df,
             top=factor(top, c("Fertility rate", "Years")),
             side=factor(side, c("Years", "Life expectancy")))
}
TS <- function(df)BOTH(df, "Years", "Life expectancy")
SCATTER <- function(df)BOTH(df, "Fertility rate", "Life expectancy")
TS2 <- function(df)BOTH(df, "Fertility rate", "Years")
years <- unique(not.na[, "year", drop=FALSE])
years$status <- ifelse(years$year %% 2, "odd", "even")
wb.facets <-
  list(ts=ggplot()+
       xlab("")+
       geom_tallrect(aes(xmin=year-1/2, xmax=year+1/2,
                         linetype=status,
                         clickSelects=year),
                     data=TS(years), alpha=1/2)+
       theme_animint(width=1000, height=800)+
       geom_line(aes(year, life.expectancy, group=country, colour=region,
                     clickSelects=country),
                 data=TS(not.na), size=4, alpha=3/5)+
       geom_point(aes(year, life.expectancy, color=region, size=population,
                      showSelected=country, clickSelects=country),
                  data=TS(not.na))+

       geom_line(aes(fertility.rate, year, group=country, colour=region,
                     clickSelects=country),
                 data=TS2(not.na), size=4, alpha=3/5)+
       geom_point(aes(fertility.rate, year, color=region, size=population,
                      showSelected=country, clickSelects=country),
                  data=TS2(not.na))+
       geom_widerect(aes(ymin=year-1/2, ymax=year+1/2,
                         clickSelects=year,
                         linetype=status,
                         id=paste0("year", year)),
                     data=TS2(years), alpha=1/2)+

       geom_point(aes(fertility.rate, life.expectancy, clickSelects=country,
                      showSelected=year, colour=region, size=population,
                      key=country), # key aesthetic for animated transitions!
                  data=SCATTER(not.na))+
       geom_text(aes(fertility.rate, life.expectancy, label=country,
                     showSelected=country, showSelected2=year,
                     clickSelects=country,
                     key=country), #also use key here!
                 data=SCATTER(not.na))+
       scale_size_animint(breaks=10^(5:9))+
       facet_grid(side ~ top, scales="free")+
       geom_text(aes(5, 85, label=paste0("year = ", year),
                     showSelected=year),
                 data=SCATTER(years)),

       bar=ggplot()+
       theme_animint(height=2400)+
       geom_bar(aes(country, life.expectancy, fill=region,
                    showSelected=year, clickSelects=country,
                    key=country, id=country),
                data=not.na, stat="identity", position="identity")+
       coord_flip(),

       time=list(variable="year", ms=1000),
       duration=list(year=1000),
       first=list(year=1975, country=c("United States", "Vietnam")),
       selector.types=list(country="multiple"),
       title="World Bank data (multiple selection, facets)")

info <- animint2HTML(wb.facets)

dasharrayPattern <-
  paste0("stroke-dasharray:",
         "(?<value>.*?)",
         ";")

rect.xpaths <-
  c('//g[@class="geom6_widerect_ts"]//g[@class="PANEL1"]//rect',
    '//g[@class="geom1_tallrect_ts"]//g[@class="PANEL4"]//rect')

test_that("wide/tallrect renders a <rect> for every year", {
  for(rect.xpath in rect.xpaths){
    node.set <- getNodeSet(info$html, rect.xpath)
    expect_equal(length(node.set), nrow(years))
    style.list <- list()
    for(node.i in seq_along(node.set)){
      node <- node.set[[node.i]]
      a.vec <- xmlAttrs(node)
      style.list[[node.i]] <- a.vec[["style"]]
      sizes <- as.numeric(a.vec[c("height", "width")])
      expect_true(all(sizes > 0))
    }
    style.vec <- do.call(c, style.list)
    dash.mat <- str_match_perl(style.vec, dasharrayPattern)
    dash.table <- table(dash.mat[, "value"])
    ## There should be 2 unique values of stoke-dasharray.
    expect_equal(length(dash.table), 2)
  }
})

getYear <- function(html){
  node.set <- getNodeSet(html, '//g[@class="geom9_text_ts"]//text')
  expect_equal(length(node.set), 1)
  xmlValue(node.set[[1]])
}

test_that("animation updates", {
  old.year <- getYear(info$html)
  Sys.sleep(1) #wait for one animation frame.
  new.html <- XML::htmlParse(remDr$getPageSource(), asText = TRUE)
  new.year <- getYear(new.html)
  expect_true(old.year != new.year)
})

clickID("show_hide_animation_controls")

test_that("pause stops animation", {
  clickID("play_pause")
  old.html <- getHTML()
  old.year <- getYear(old.html)
  Sys.sleep(1)
  new.html <- getHTML()
  new.year <- getYear(new.html)
  expect_true(old.year == new.year)
})

test_that("play restarts animation", {
  old.html <- getHTML()
  old.year <- getYear(old.html)
  clickID("play_pause")
  Sys.sleep(2)
  new.html <- getHTML()
  new.year <- getYear(new.html)
  expect_true(old.year != new.year)
})

e <- remDr$findElement("id", "updates_ms")
e$clickElement()
e$clearElement()
e$sendKeysToElement(list("3000", key="enter"))

test_that("pause stops animation (second time)", {
  clickID("play_pause")
  old.html <- getHTML()
  old.year <- getYear(old.html)
  Sys.sleep(1)
  new.html <- getHTML()
  new.year <- getYear(new.html)
  expect_true(old.year == new.year)
})

test_that("play restarts animation (slower)", {
  old.html <- getHTML()
  old.year <- getYear(old.html)
  clickID("play_pause")
  Sys.sleep(1)
  new.html <- getHTML()
  new.year <- getYear(new.html)
  expect_true(old.year == new.year)
  Sys.sleep(3)
  newer.html <- getHTML()
  newer.year <- getYear(newer.html)
  expect_true(old.year != newer.year)
})

test_that("pause stops animation (third time)", {
  clickID("play_pause")
  old.html <- getHTML()
  old.year <- getYear(old.html)
  Sys.sleep(4)
  new.html <- getHTML()
  new.year <- getYear(new.html)
  expect_true(old.year == new.year)
})

getWidth <- function(html){
  node.set <-
    getNodeSet(html, '//g[@class="geom10_bar_bar"]//rect[@id="Vietnam"]')
  expect_equal(length(node.set), 1)
  alist <- xmlAttrs(node.set[[1]])
  alist[["width"]]
}

test_that("middle of transition != after when duration=1000", {
  clickID("year1960")
  Sys.sleep(1.5)
  before.html <- getHTML()
  before.width <- getWidth(before.html)
  clickID("year2010")
  during.html <- getHTML()
  during.width <- getWidth(during.html)
  Sys.sleep(1.5)
  after.html <- getHTML()
  after.width <- getWidth(after.html)
  rbind(before.width, during.width, after.width)
  expect_true(during.width != after.width)
})

e <- remDr$findElement("id", "duration_ms_year")
e$clickElement()
e$clearElement()
e$sendKeysToElement(list("0", key="enter"))

test_that("middle of transition == after when duration=0", {
  clickID("year1960")
  Sys.sleep(1.5)
  before.html <- getHTML()
  before.width <- getWidth(before.html)
  clickID("year2010")
  during.html <- getHTML()
  during.width <- getWidth(during.html)
  Sys.sleep(1.5)
  after.html <- getHTML()
  after.width <- getWidth(after.html)
  rbind(before.width, during.width, after.width)
  expect_true(during.width == after.width)
})


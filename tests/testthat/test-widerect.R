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
       theme_bw()+
       theme_animint(width=1000, height=800)+
       theme(panel.margin=grid::unit(0, "lines"))+
       geom_line(aes(year, life.expectancy, group=country, colour=region,
                     clickSelects=country, id = country),
                 data=TS(not.na), size=4, alpha=3/5)+
       geom_point(aes(year, life.expectancy, color=region, size=population,
                      showSelected=country, clickSelects=country),
                  data=TS(not.na))+

       geom_path(aes(fertility.rate, year, group=country, colour=region,
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

rect.list <-
  getNodeSet(info$html, '//svg[@id="ts"]//rect[@class="border_rect"]')
expect_equal(length(rect.list), 4)
at.mat <- sapply(rect.list, xmlAttrs)

test_that("three unique border_rect x values (no horiz space)", {
  left.vec <- as.numeric(at.mat["x", ])
  width.vec <- as.numeric(at.mat["width", ])
  right.vec <- left.vec + width.vec
  x.values <- unique(c(left.vec, right.vec))
  expect_equal(length(x.values), 3)
})

test_that("three unique border_rect y values (no vert space)", {
  top.vec <- as.numeric(at.mat["y", ])
  height.vec <- as.numeric(at.mat["height", ])
  bottom.vec <- top.vec + height.vec
  y.values <- unique(c(top.vec, bottom.vec))
  expect_equal(length(y.values), 3)
})

line.xpath <- '//g[@class="geom2_line_ts"]//g[@class="PANEL4"]//path'
opacityPattern <-
  paste0("opacity:",
         "(?<value>.*?)",
         ";")

test_that("line opacity initially 0.1 or 0.6", {

  node.set <- getNodeSet(info$html, line.xpath)
  opacity.list <- list()
  for(node.i in seq_along(node.set)){
    node <- node.set[[node.i]]
    a.vec <- xmlAttrs(node)
    style.str <- a.vec[["style"]]
    opacity.mat <- str_match_perl(style.str, opacityPattern)
    node.id <- a.vec[["id"]]
    opacity.list[[node.id]] <- as.numeric(opacity.mat[, "value"])
  }
  opacity.vec <- do.call(c, opacity.list)

  selected.computed <- as.numeric(opacity.vec[wb.facets$first$country])
  selected.expected <- rep(0.6, length(selected.computed))
  expect_equal(selected.computed, selected.expected)

  unselected.computed <-
    as.numeric(opacity.vec[!names(opacity.vec) %in% wb.facets$first$country])
  unselected.expected <- rep(0.1, length(unselected.computed))
  expect_equal(unselected.computed, unselected.expected)

})

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
    ## Use paste() to treat NA as a value instead of ignoring it.
    dash.table <- table(paste(dash.mat[, "value"]))
    ## There should be 2 unique values of stoke-dasharray.
    expect_equal(length(dash.table), 2)
  }
})

getYear <- function(){
  node.set <- getNodeSet(getHTML(), '//g[@class="geom9_text_ts"]//text')
  expect_equal(length(node.set), 1)
  value <- xmlValue(node.set[[1]])
  sub("year = ", "", value)
}

test_that("animation updates", {
  old.year <- getYear()
  Sys.sleep(1) #wait for one animation frame.
  new.year <- getYear()
  expect_true(old.year != new.year)
})

clickID("show_hide_animation_controls")

test_that("pause stops animation", {
  clickID("play_pause")
  old.year <- getYear()
  Sys.sleep(1)
  new.year <- getYear()
  expect_true(old.year == new.year)
})

test_that("play restarts animation", {
  old.year <- getYear()
  clickID("play_pause")
  Sys.sleep(2)
  new.year <- getYear()
  expect_true(old.year != new.year)
})

test_that("pause stops animation (second time)", {
  clickID("play_pause")
  old.year <- getYear()
  Sys.sleep(1)
  new.year <- getYear()
  expect_true(old.year == new.year)
})

clickID("even")
clickID("odd")
html.no.rects <- getHTML()

test_that("clicking status legend hides tallrects", {
  for(rect.xpath in rect.xpaths){
    node.set <- getNodeSet(html.no.rects, rect.xpath)
    expect_equal(length(node.set), 0)
  }
})

test_that("clicking status legend does not hide text", {
  node.set <-
    getNodeSet(html.no.rects,
               '//g[@class="geom9_text_ts"]//text[@class="geom"]')
  expect_equal(length(node.set), 1)
})

clickID("even")
clickID("odd")
html.with.rects <- getHTML()

test_that("clicking status legend brings back tallrects", {
  for(rect.xpath in rect.xpaths){
    node.set <- getNodeSet(html.with.rects, rect.xpath)
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
    ## Use paste() to treat NA as a value instead of ignoring it.
    dash.table <- table(paste(dash.mat[, "value"]))
    ## There should be 2 unique values of stoke-dasharray.
    expect_equal(length(dash.table), 2)
  }
})

test_that("play restarts animation (second time)", {
  old.year <- getYear()
  clickID("play_pause")
  Sys.sleep(2)
  new.year <- getYear()
  expect_true(old.year != new.year)
})

legend.td.xpath <- '//td[@id="North America" and @class="legend_entry_label"]'
rects_and_legends <- function(){
  html <- getHTML()
  list(rects=getNodeSet(html, '//rect[@id="United States"]'),
       legends=getNodeSet(html, legend.td.xpath))
}

opacityPattern <-
  paste0("opacity:",
         "(?<value>.*?)",
         ";")

expect_opacity <- function(node.list, expected.opacity){
  style.strs <- sapply(node.list, function(x) xmlAttrs(x)["style"])
  match.mat <- str_match_perl(style.strs, opacityPattern)
  opacity.chr <- match.mat[, "value"]
  opacity.num <- as.numeric(opacity.chr)
  opacity.num[is.na(opacity.num)] <- 1 ## no transparency if not specified.
  expected.vec <- rep(expected.opacity, l=length(opacity.num))
  expect_equal(opacity.num, expected.vec)
}

test_that("clicking legend removes/adds countries", {
  before <- rects_and_legends()
  expect_equal(length(before$rects), 1)
  expect_opacity(before$legends, 1)
  
  clickID("North America")
  oneclick <- rects_and_legends()
  expect_equal(length(oneclick$rects), 0)
  expect_opacity(oneclick$legends, 0.5)

  clickID("North America")
  twoclicks <- rects_and_legends()
  expect_equal(length(twoclicks$rects), 1)
  expect_opacity(twoclicks$legends, 1)
})

# skip these tests if the browser is phantomjs 
# (version 1.9.8 seems to stall on some machines)
if (Sys.getenv("ANIMINT_BROWSER") != "phantomjs") {
  
  e <- remDr$findElement("id", "updates_ms")
  e$clickElement()
  e$clearElement()
  e$sendKeysToElement(list("3000", key="enter"))
  
  test_that("pause stops animation (third time)", {
    clickID("play_pause")
    old.year <- getYear()
    Sys.sleep(4)
    new.year <- getYear()
    expect_true(old.year == new.year)
  })

  clickID("show_hide_selector_widgets")

  s.tr <- remDr$findElement("id", "year_selector_widget")
  s.div <- s.tr$findChildElement("class name", "selectize-input")
  s.div$clickElement()
  remDr$sendKeysToActiveElement(list(key="backspace"))
  remDr$sendKeysToActiveElement(list("1962", key="enter"))

  test_that("typing into selectize widget changes year to 1962", {
    current.year <- getYear()
    expect_identical(current.year, "1962")
  })
  
  s.div$clickElement()
  remDr$sendKeysToActiveElement(list(key="down_arrow"))
  remDr$sendKeysToActiveElement(list(key="enter"))
  
  test_that("down arrow key changes year to 1963", {
    current.year <- getYear()
    expect_identical(current.year, "1963")
  })

  getCountries <- function(){
    country.labels <- getNodeSet(getHTML(), '//g[@class="geom8_text_ts"]//text')
    sapply(country.labels, xmlValue)
  }

  test_that("initial countries same as first", {
    country.vec <- getCountries()
    expect_identical(sort(country.vec), sort(wb.facets$first$country))
  })
  
  s.tr <- remDr$findElement("id", "country_selector_widget")
  s.div <- s.tr$findChildElement("class name", "selectize-input")
  s.div$clickElement()
  remDr$sendKeysToActiveElement(list("Afg"))
  remDr$sendKeysToActiveElement(list(key="enter"))

  test_that("Afg autocompletes to Afghanistan", {
    country.vec <- getCountries()
    expected.countries <- c("United States", "Vietnam", "Afghanistan")
    expect_identical(sort(country.vec), sort(expected.countries))
  })
  
  div.list <- s.tr$findChildElements("class name", "item")
  names(div.list) <- sapply(div.list, function(e)e$getElementText()[[1]])
  us.div <- div.list[["United States"]]
  us.div$clickElement()
  remDr$sendKeysToActiveElement(list(key="backspace"))
  
  test_that("backspace removes US from selected countries", {
    country.vec <- getCountries()
    expected.countries <- c("Vietnam", "Afghanistan")
    expect_identical(sort(country.vec), sort(expected.countries))
  })
  
  getWidth <- function(){
    node.set <-
      getNodeSet(getHTML(), '//g[@class="geom10_bar_bar"]//rect[@id="Vietnam"]')
    expect_equal(length(node.set), 1)
    alist <- xmlAttrs(node.set[[1]])
    alist[["width"]]
  }
  
  test_that("middle of transition != after when duration=1000", {
    clickID("year2010")
    during.width <- getWidth()
    Sys.sleep(1.5)
    after.width <- getWidth()
    expect_true(during.width != after.width)
  })
  
  # doesn't quite work
  # remDr$executeScript('return document.getElementById("duration_ms_year").value = 0')
  e <- remDr$findElement("id", "duration_ms_year")
  e$clickElement()
  e$clearElement()
  e$sendKeysToElement(list("0", key="enter"))
  
  test_that("middle of transition == after when duration=0", {
    clickID("year1960")
    Sys.sleep(1.5)
    before.width <- getWidth()
    clickID("year2010")
    during.width <- getWidth()
    Sys.sleep(1.5)
    after.width <- getWidth()
    expect_true(before.width != after.width)
    expect_true(during.width == after.width)
  })
}

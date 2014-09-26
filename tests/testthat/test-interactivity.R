context("interactivity")

## Example: 2 plots, 2 selectors, but only interacting with 1 plot.
data(breakpoints)
only.error <- subset(breakpoints$error,type=="E")
only.segments <- subset(only.error, samples==samples[1])
signal.colors <- c(estimate="#0adb0a",
                   latent="#0098ef")
breakpointError <- 
  list(signal=ggplot()+
         geom_point(aes(position, signal, showSelected=samples),
                    data=breakpoints$signals)+
         geom_line(aes(position, signal), colour=signal.colors[["latent"]],
                   data=breakpoints$imprecision)+
         geom_segment(aes(first.base, mean, xend=last.base, yend=mean,
                          showSelected=segments,
                          showSelected2=samples),
                      colour=signal.colors[["estimate"]],
                      data=breakpoints$segments)+
         geom_vline(aes(xintercept=base,
                        showSelected=segments,
                        showSelected2=samples),
                    colour=signal.colors[["estimate"]],
                    linetype="dashed",
                    data=breakpoints$breaks),
       error=ggplot()+
         geom_vline(aes(xintercept=segments, clickSelects=segments,
                        key=segments),
                    data=only.segments, lwd=17, alpha=1/2)+
         geom_line(aes(segments, error, group=samples,
                       key=samples,
                       clickSelects=samples),
                   data=only.error, lwd=4),
       first=list(samples=150, segments=4),
       title="breakpointError (select one model size)")

info <- animint2HTML(breakpointError)

test_that("default is single selection", {
  selector.types <- lapply(info$selectors, "[[", "type")
  expect_match(selector.types$samples, "single")
  expect_match(selector.types$segments, "single")
})

test_that("aes(key) geoms have ids", {
  nodes <- getNodeSet(info$html, '//*[@id][@class="geom"]')
  expect_equal(length(nodes), 24)
})

test_that("default is 150 <circle> elements", {
  nodes <- getNodeSet(info$html, '//g[@class="geom1_point_signal"]//circle')
  expect_equal(length(nodes), 150)
})

test_that("default is 4 <line> segments", {
  nodes <- getNodeSet(info$html, '//g[@class="geom3_segment_signal"]//line')
  expect_equal(length(nodes), 4)
})

test_that("clickSelects 300 makes 300 <circle> elements", {
  e <- remDr$findElement("id", "300")
  remDr$mouseMoveToLocation(webElement=e)
  remDr$click()
  nodes <- getNodeSet(info$html, '//g[@class="geom1_point_signal"]//circle')
  expect_equal(length(nodes), 300)
})

test_that("clickSelects 1 changes to 1 <line> element", {
  e <- remDr$findElement("id", "1")
  remDr$mouseMoveToLocation(webElement=e)
  remDr$click()
  nodes <- getNodeSet(info$html, '//g[@class="geom3_segment_signal"]//line')
  expect_equal(length(nodes), 1)
})

breakpointError$selector.types <-
  list(segments="multiple",
       samples="single")
breakpointError$title <-
  "breakpointError (select several model sizes)"
info <- animint2HTML(breakpointError)

test_that("selector.types are converted to JSON", {
  selector.types <- lapply(info$selectors, "[[", "type")
  expect_match(selector.types$samples, "single")
  expect_match(selector.types$segments, "multiple")
})

test_that("default is 150 <circle> elements", {
  nodes <- getNodeSet(info$html, '//g[@class="geom1_point_signal"]//circle')
  expect_equal(length(nodes), 150)
})

test_that("clickSelects 300 makes 300 <circle> elements", {
  e <- remDr$findElement("id", "300")
  remDr$mouseMoveToLocation(webElement=e)
  remDr$click()
  nodes <- getNodeSet(info$html, '//g[@class="geom1_point_signal"]//circle')
  expect_equal(length(nodes), 300)
})

test_that("clickSelects 1 adds 1 <line> element", {
  e <- remDr$findElement("id", "1")
  remDr$mouseMoveToLocation(webElement=e)
  remDr$click()
  nodes <- getNodeSet(info$html, '//g[@class="geom3_segment_signal"]//line')
  expect_equal(length(nodes), 5)
})

test_that("clickSelects 4 removes 4 <line> elements", {
  e <- remDr$findElement("id", "4")
  remDr$mouseMoveToLocation(webElement=e)
  remDr$click()
  nodes <- getNodeSet(info$html, '//g[@class="geom3_segment_signal"]//line')
  expect_equal(length(nodes), 1)
})

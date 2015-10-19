acontext("showSelected=var, showSelected2=var")

viz <- list(
  points=ggplot()+
    geom_point(aes(Sepal.Length, Petal.Length,
                        showSelected=Species,
                        showSelected2=Species),
                    data=iris)
  )
info <- animint2HTML(viz)
ss.list <- with(info$geoms$geom1_point_points, c(chunk_order, subset_order))
ss.vec <- unlist(ss.list)

test_that("redundant showSelected are optimized out", {
  expect_equal(length(ss.vec), 1)
})

test_that("50 <circle> rendered at first", {
  circle.list <- getNodeSet(
    info$html, '//g[@class="geom1_point_points"]//circle')
  expect_equal(length(circle.list), 50)
})

test_that("redundant aes not saved to tsv", {
  geom.tsv <- Sys.glob(
    file.path("animint-htmltest", "geom1_point_points_*.tsv"))
  expect_equal(length(geom.tsv), 1)
  expected.names <- sort(c("x", "y", ss.vec))
  tsv.data <- read.table(geom.tsv, header=TRUE)
  computed.names <- sort(names(tsv.data))
  expect_identical(computed.names, expected.names)
})

test_that("selector widgets table initially visiable", {
  display <- getStyleValue(
    info$html, '//table[@class="table_selector_widgets"]',
    "display")
  ## If display is NA then display style is undefined and the table is
  ## rendered.
  expect_true(is.na(display))
})

viz <- list(
  points=ggplot()+
    geom_point(aes(Sepal.Length, Petal.Length,
                        showSelected=Species,
                        color=Species),
                    data=iris)
  )
info <- animint2HTML(viz)

test_that("150 <circle> rendered at first", {
  circle.list <- getNodeSet(
    info$html, '//g[@class="geom1_point_points"]//circle')
  expect_equal(length(circle.list), 150)
})

test_that("redundant showSelected and color optimized", {
  var.list <- with(info$geoms$geom1_point_points, c(chunk_order, subset_order))
  expect_equal(length(var.list), 1)
})

test_that("selector widgets table initially invisible", {
  display <- getStyleValue(
    info$html, '//table[@class="table_selector_widgets"]',
    "display")
  expect_match(display, "none")
})

## This test responds to @kferris10 who asked the question "What will
## happen if there is only one level? Will it render as an array or an
## object?" https://github.com/tdhock/animint/pull/115 We were talking
## about what to do with the selector widget when there is a
## showSelected variable which has only 1 level. However that is a
## trivial case for a showSelected variable, so it should just be
## optimized out with a warning.
iris$kingdom <- "plantae"
viz <- list(
  points=ggplot()+
    geom_point(aes(Petal.Length, Sepal.Length,
                   color=Species,
                   showSelected=kingdom),
               data=iris)
  )

test_that("compiler warns for showSelected with 1 level", {
  expect_warning({
    info <- animint2HTML(viz)
  }, "showSelected variables with only 1 level")
  circle.list <- getNodeSet(
    info$html, '//g[@class="geom1_point_points"]//circle')
  expect_equal(length(circle.list), 150)
})


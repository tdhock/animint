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

test_that("redundant showSelected and color optimized", {
  var.list <- with(info$geoms$geom1_point_points, c(chunk_order, subset_order))
  expect_equal(length(var.list), 1)
})

test_that("selector widgets table initially invisiable", {
  display <- getStyleValue(
    info$html, '//table[@class="table_selector_widgets"]',
    "display")
  expect_match(display, "none")
})


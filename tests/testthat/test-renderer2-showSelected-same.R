acontext("showSelected=var, showSelected2=var")

viz <- list(
  points=ggplot()+
    geom_point(aes(Sepal.Length, Petal.Length,
                        showSelected=Species,
                        showSelected2=Species),
                    data=iris)
  )
info <- animint2HTML(viz)

test_that("redundant showSelected are optimized out", {
  var.list <- with(info$geoms$geom1_point_points, c(chunk_order, subset_order))
  expect_equal(length(var.list), 1)
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


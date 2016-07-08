acontext("geom_point_stroke")

p1 <- ggplot(mtcars, aes(wt, mpg)) +
  geom_point(shape = 21, colour = "black", fill = "white",
             size = 5, stroke = 5)

p2 <- ggplot(mtcars, aes(wt, mpg, stroke=cyl)) +
  geom_point(shape = 21, colour = "black", fill = "white", size = 5)

viz <- list(p1=p1, p2=p2)
info <- animint2HTML(viz)

test_that("points are rendered with stroke-width", {
  point_nodes <-
    getNodeSet(info$html, '//g[@class="geom1_point_p1"]//circle')
  # Get style values and check that all rendered points have a single
  # stroke-width value
  points_style <- sapply(point_nodes, xmlAttrs)[2, ]
  
  expect_equal(length(points_style), length(mtcars$wt))
  
  points_style_unique <- unique(points_style)
  
  expect_equal(length(points_style_unique), 1)
  points_with_style <-
    unique(sapply(points_style_unique, strsplit, split="; "))
  
  expect_true(any(sapply(points_with_style, grepl,
                         pattern="stroke-width")))
})

test_that("aes(stroke) works", {
  point_nodes_2 <-
    getNodeSet(info$html, '//g[@class="geom2_point_p2"]//circle')
  points_style_2 <- sapply(point_nodes_2, xmlAttrs)[2, ]
  
  # Check that stroke values differ according to cyl values in mtcars
  expect_equal(length(points_style_2), length(mtcars$wt))
  
  points_style_unique_2 <- unique(points_style_2)
  
  expect_equal(length(points_style_unique_2), length(unique(mtcars$cyl)))
  points_with_style_2 <-
    unique(sapply(points_style_unique_2, strsplit, split="; "))
  
  # Below we are just checking that stroke-width is present amongst all
  # the points
  expect_true(all(apply((sapply(points_with_style_2, grepl,
                                pattern="stroke-width")), 2, any)))
  
  # Check that the values of the stroke are taken from mtcars$cyl
  stroke_width_vals <- sapply(points_with_style_2, grep,
                              pattern="^stroke-width", value=T)
  pixel_vals <- as.numeric(sapply(stroke_width_vals, gsub,
                                  pattern="[^0-9]", replacement=""))
  expect_identical(sort(pixel_vals), sort(unique(mtcars$cyl)))
})

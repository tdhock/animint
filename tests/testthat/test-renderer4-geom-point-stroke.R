acontext("geom_point_stroke")

stroke_in_R <- 5
p1 <- ggplot(mtcars, aes(wt, mpg)) +
  geom_point(shape = 21, colour = "black", fill = "white",
             size = 5, stroke = stroke_in_R)

p2 <- ggplot(mtcars, aes(wt, mpg, stroke=cyl)) +
  geom_point(shape = 21, colour = "black", fill = "white", size = 5)

viz <- list(p1=p1, p2=p2)
info <- animint2HTML(viz)

test_that("points are rendered with stroke-width", {
  stroke_vals <-
    getStyleValue(info$html, '//g[@class="geom1_point_p1"]//circle', 
                  "stroke-width")
  # stroke-width is rendered for every point
  expect_equal(length(stroke_vals), length(mtcars$wt))
  
  stroke_vals_unique <- unique(stroke_vals)
  expect_equal(length(stroke_vals_unique), 1)
  
  stroke_width_val <- as.numeric(gsub("[^0-9]", "", stroke_vals_unique))
  expect_equal(stroke_width_val, stroke_in_R)
})

test_that("aes(stroke) works", {
  stroke_vals_2 <-
    getStyleValue(info$html, '//g[@class="geom2_point_p2"]//circle',
                  "stroke-width")
  
  expect_equal(length(stroke_vals_2), length(mtcars$wt))
  
  stroke_vals_unique_2 <- unique(stroke_vals_2)
  expect_equal(length(stroke_vals_unique_2), length(unique(mtcars$cyl)))
  
  # Check that the values of the stroke are taken from mtcars$cyl
  stroke_width_vals <- as.numeric(gsub("[^0-9]", "", stroke_vals_unique_2))
  expect_identical(sort(stroke_width_vals), sort(unique(mtcars$cyl)))
})

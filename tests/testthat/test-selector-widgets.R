context("Selector Widgets")

p1 <- ggplot() +
  geom_point(aes(Sepal.Length, Sepal.Width, clickSelects = Species),
             data = iris) +
  ggtitle("Sepal Data")
p2 <- ggplot() +
  geom_point(aes(Petal.Length, Petal.Width, colour = Species, 
                 showSelected = Species), data = iris) +
  ggtitle("Petal Data")

info <- animint2HTML(list(p1 = p1, p2 = p2))

# different patterns to access
fillPattern <-
  paste0("fill: ",
         "(?<value>.*?)",
         ";")
# function to test how many points have the expected color pattern
test_fills <- function(fills, exp_color, exp_length) {
  exp_string <- if(grepl("rgb", fills[1])) {
    paste0("rgb(", paste(col2rgb(exp_color), collapse = ", "), ")")
  } else {
    exp_color
  }
  # how many points have the expected fill
  matching_fills <- sum(fills == exp_string)
  expect_equal(matching_fills, exp_length)
}

test_that("Show/Hide Species widget renders", {
  sp_widget <- getNodeSet(info$html, '//button[@id="Species"]')
  expect_equal(length(sp_widget), 1)
})

test_that("Clicking displays button for each species", {
  clickID("Species")
  buttons <- 
    getNodeSet(getHTML(), '//table[@class="selector_table"]//button')
  expect_equal(length(buttons), 3)
})

test_that("Clicking versicolor widget shows/hides points", {
  clickID("versicolor")
  points <- 
    getNodeSet(getHTML(), '//svg[@id="p2"]//circle')
  point_attr <- sapply(points, xmlAttrs)
  match_points <- str_match_perl(point_attr["style",], fillPattern)
  fill_values <- match_points[, "value"]

  # test versicolor points are selected
  test_fills(fill_values, "#00BA38", 50)
  # test setosa points are not selected
  test_fills(fill_values, "#F8766D", 0)
  
})
context("Selector Widgets")

mtcars$cyl <- factor(mtcars$cyl)
mtcars$vs <- factor(mtcars$vs)
p1 <- ggplot() + 
  geom_point(aes(wt, mpg, colour = cyl, size = vs), 
             data = mtcars)
p2  <- ggplot() + 
  geom_point(aes(mpg, wt, colour = cyl, size = vs), data = mtcars)


info <- animint2HTML(
  list(p1 = p1, p2 = p2, 
       selector.types = list(vs = "single", cyl = "multiple"), 
       first = list(vs = 0, cyl = c(4,6)))
)

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

test_that("Widgets render", {
  # testing that the "toggle selectors" button is present
  toggle_widget <- getNodeSet(info$html, '//button[@id="show_hide_selector_widgets"]')
  expect_equal(length(toggle_widget), 1)
  
  clickID("show_hide_selector_widgets")
  # testing that the vs widget is present
  vs_widget <- getNodeSet(info$html, '//select[@id="vs_input"]')
  expect_equal(length(vs_widget), 1)
  # testing that the cyl widget is present
  cyl_widget <- getNodeSet(info$html, '//select[@id="cyl_input"]')
  expect_equal(length(cyl_widget), 1)
})

test_that("Updating cyl widget shows/hides points", {
  e <- remDr$findElement("id", "cyl_input")
  e$clickElement()
  e$clearElement()
  browser()
  e$sendKeysToElement(list("3000", key="enter"))
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
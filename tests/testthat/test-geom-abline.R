context("geom_abline")

p <- qplot(wt, mpg, data = mtcars) + 
  geom_abline(intercept = c(20, 5), slope = c(1,4)) + 
  facet_wrap(~cyl)
info <- animint2HTML(list(p = p))
ablines <- getNodeSet(info$html, '//svg//g[@class="geom2_abline_p"]//line')
attr_ablines <- sapply(ablines, xmlAttrs)
start_ends <- attr_ablines[c("x1", "x2", "y1", "y2"), ]

test_that("All six ablines render", {
  expect_equal(length(ablines), 6)
})

test_that("Start and end of ablines are not NA", {
  expect_true(all(start_ends != "NaN"))
})

test_that("lines do not exceed ranges of plot", {
  expect_true(all(as.numeric(start_ends) >= 0))
})

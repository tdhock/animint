context("geom_abline")

p <- qplot(wt, mpg, data = mtcars) + 
  geom_abline(intercept = 20) + facet_wrap(~cyl)
info <- animint2HTML(list(p = p))
ablines <- getNodeSet(info$html, '//svg//g[@class="geom2_abline_p"]//line')
attr_ablines <- sapply(ablines, xmlAttrs)

test_that("All three ablines render", {
  expect_equal(length(ablines), 3)
})

test_that("Start and end of ablines are not NA", {
  start_ends <- attr_ablines[3:6, ]
  expect_true(!any(start_ends == "NaN"))
})

acontext("facet-coord")

p <- ggplot(mtcars, aes(mpg, wt)) + 
  geom_point(colour='grey50', size = 4) + 
  geom_point(aes(colour = cyl)) 

test_that("coord_fixed() + facet_wrap(nrow=1) with shrunk y-axis", {
  ratio2 <- 2
  wrapViz <-
    list(wrapPlot=p +
         facet_wrap(~am) + coord_fixed(ratio2))
  info <- animint2HTML(wrapViz)
  x.axes <- getNodeSet(info$html, "//g[contains(@class, 'xaxis')]")
  y.axes <- getNodeSet(info$html, "//g[contains(@class, 'yaxis')]")
  xdiff1 <- getTickDiff(x.axes[[1]])
  ydiff <- getTickDiff(y.axes[[1]], axis = "y")
  diffs <- normDiffs(xdiff1, ydiff, ratio2)
  expect_equal(diffs[1], diffs[2], tolerance = 1, scale = 1)
  xdiff2 <- getTickDiff(x.axes[[2]])
  diffs <- normDiffs(xdiff2, ydiff, ratio2)
  expect_equal(diffs[1], diffs[2], tolerance = 1, scale = 1)
})

test_that("coord_fixed() + facet_wrap(nrow=1) with shrunk x-axis", {
  ratio10 <- 10
  wrapViz <-
    list(wrapPlot=p +
           facet_wrap(~am) + coord_fixed(ratio10))
  info <- animint2HTML(wrapViz)
  x.axes <- getNodeSet(info$html, "//g[contains(@class, 'xaxis')]")
  y.axes <- getNodeSet(info$html, "//g[contains(@class, 'yaxis')]")
  xdiff1 <- getTickDiff(x.axes[[1]])
  ydiff <- getTickDiff(y.axes[[1]], axis = "y")
  diffs <- normDiffs(xdiff1, ydiff, ratio10)
  expect_equal(diffs[1], diffs[2], tolerance = 1, scale = 1)
  xdiff2 <- getTickDiff(x.axes[[2]])
  diffs <- normDiffs(xdiff2, ydiff, ratio10)
  expect_equal(diffs[1], diffs[2], tolerance = 1, scale = 1)
})

# also test multiple row and/or facet_grid?

context("coord")

test_that("coord_flip works", {
  data(worldPop, package="animint")
  bars <- ggplot()+
    geom_bar(aes(x=subcontinent, y=population, showSelected=year),
             data=worldPop, stat="identity", position="identity")
  ## First test without flip.
  no.flip <- animint2dir(list(bars=bars), open.browser=FALSE)
  ax <- no.flip$plots$bars
  expect_identical(ax$xtitle, "subcontinent")
  expect_identical(ax$ytitle, "population")
  ## Then test with flip.
  flip <- animint2dir(list(bars=bars+coord_flip()), open.browser=FALSE)
  ax <- flip$plots$bars
  expect_identical(ax$ytitle, "subcontinent")
  expect_identical(ax$xtitle, "population")
})

p <- ggplot(mtcars, aes(mpg, wt)) + 
  geom_point(colour='grey50', size = 4) + 
  geom_point(aes(colour = cyl))

test_that("coord_fixed with shrinking y-axis", {
  ratio5 <- 5
  viz1 <- p + coord_fixed(ratio5)
  info <- animint2HTML(list(plot = viz1))
  x.axes <- getNodeSet(info$html, "//g[@id='xaxis']")
  y.axes <- getNodeSet(info$html, "//g[@id='yaxis']")
  xdiff <- getTickDiff(x.axes[[1]])
  ydiff <- getTickDiff(y.axes[[1]], axis = "y")
  diffs <- normDiffs(xdiff, ydiff, ratio5)
  expect_equal(diffs[1], diffs[2], tolerance = 1, scale = 1)
})

test_that("coord_fixed with shrinking x-axis", {
  ratio10 <- 10
  viz2 <- p + coord_fixed(ratio10)
  info <- animint2HTML(list(plot = viz2))
  x.axes <- getNodeSet(info$html, "//g[@id='xaxis']")
  y.axes <- getNodeSet(info$html, "//g[@id='yaxis']")
  xdiff <- getTickDiff(x.axes[[1]])
  ydiff <- getTickDiff(y.axes[[1]], axis = "y")
  diffs <- normDiffs(xdiff, ydiff, ratio10)
  expect_equal(diffs[1], diffs[2], tolerance = 1, scale = 1)
})

context("facet-space")

no.panels <- ggplot(mtcars, aes(mpg, wt)) + 
  geom_point(colour='grey50', size = 4) + 
  geom_point(aes(colour = cyl)) 

test_that("space = 'free' argument in facet_grid works", {
  panels <- no.panels + facet_grid(.~am, space = "free", scales = "free")
  viz <- list(gridPlot = panels)
  info <- animint2HTML(viz)
  ## Check that there are two x axes and 1 y axis.
  x.axes <- getNodeSet(info$html, "//g[@id='xaxis']")
  expect_equal(length(x.axes), 2)
  y.axes <- getNodeSet(info$html, "//g[@id='xaxis']")
  expect_equal(length(y.axes), 1)
})

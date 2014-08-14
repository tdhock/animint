context("facet-space")

p <- ggplot(mtcars, aes(mpg, wt)) + 
  geom_point(colour='grey50', size = 4) + 
  geom_point(aes(colour = cyl)) 

test_that("space = 'free' argument in facet_grid works", {
  viz <- p + facet_grid(.~am, space = "free", scales = "free")
  info <- animint2HTML(list(plot = viz))
  nodes <- getNodeSet(info$html, "//g[@id='yaxis']")
  translate1 <- xmlAttrs(nodes[[1]])["transform"]
  translate2 <- xmlAttrs(nodes[[2]])["transform"]
  expect_transform(translate1, c(28, 0))
  expect_transform(translate2, c(198, 0))
})

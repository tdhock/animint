context("facet-coord")

p <- ggplot(mtcars, aes(mpg, wt)) + 
  geom_point(colour='grey50', size = 4) + 
  geom_point(aes(colour = cyl)) 

test_that("coord_fixed() works single row facet_wrap()", {
  viz <- p + facet_wrap(~am) + coord_fixed(ratio = 2)
  info <- animint2HTML(list(plot = viz))
  nodes <- getNodeSet(info$html, "//g[@id='xaxis']")
  translate1 <- xmlAttrs(nodes[[1]])["transform"]
  translate2 <- xmlAttrs(nodes[[2]])["transform"]
  expect_transform(translate1, c(0, 231))
  expect_transform(translate2, c(0, 231))
})

# also test multiple row and/or facet_grid?
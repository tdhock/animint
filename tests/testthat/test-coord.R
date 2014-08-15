context("coord")

test_that("coord_flip works", {
  data(worldPop, package="animint")
  bars <- ggplot()+
    geom_bar(aes(x=subcontinent, y=population, showSelected=year),
             data=worldPop, stat="identity", position="identity")
  ## First test without flip.
  no.flip <- animint2dir(list(bars=bars), open.browser=FALSE)
  ax <- no.flip$plots$bars$axis
  expect_identical(ax$xname, "subcontinent")
  expect_identical(ax$yname, "population")
  ## Then test with flip.
  flip <- animint2dir(list(bars=bars+coord_flip()), open.browser=FALSE)
  ax <- flip$plots$bars$axis
  expect_identical(ax$yname, "subcontinent")
  expect_identical(ax$xname, "population")
})

test_that("coord_fixed works", { 
  p <- ggplot(mtcars, aes(mpg, wt)) + 
    geom_point(colour='grey50', size = 4) + 
    geom_point(aes(colour = cyl))
  # CASE 1: y-axis is 'shrunk'
  viz1 <- p + coord_fixed(ratio = 3)
  info <- animint2HTML(list(plot = viz1))
  nodes <- getNodeSet(info$html, "//g[@id='xaxis']")
  # grab transform attribute value for the xaxis
  translate <- xmlAttrs(nodes[[1]])["transform"]
  expect_transform(translate, c(0, 266))
  # CASE 2: x-axis is 'shrunk'
  viz2 <- p + coord_fixed(ratio = 10)
  info <- animint2HTML(list(plot = viz2))
  nodes <- getNodeSet(info$html, "//g[@id='yaxis']")
  translate <- xmlAttrs(nodes[[1]])["transform"]
  expect_transform(translate, c(108, 0))
})

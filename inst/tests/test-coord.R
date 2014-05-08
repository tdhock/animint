context("coord")

test_that("coord_flip works",{
  data(worldPop, package="animint")
  bars <- ggplot()+
    geom_bar(aes(x=subcontinent, y=population, showSelected=year),
             data=worldPop, stat="identity", position="identity")
  ## First test without flip.
  no.flip <- gg2animint(list(bars=bars), open.browser=FALSE)
  ax <- no.flip$plots$bars$axis
  expect_identical(ax$xname, "subcontinent")
  expect_identical(ax$yname, "population")
  ## Then test with flip.
  flip <- gg2animint(list(bars=bars+coord_flip()), open.browser=FALSE)
  ax <- flip$plots$bars$axis
  expect_identical(ax$yname, "subcontinent")
  expect_identical(ax$xname, "population")
})

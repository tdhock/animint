acontext("facet-trivial")

miris <- iris
miris$kingdom <- "plantae"

gg <- ggplot()+
  geom_point(aes(Petal.Length, Petal.Width),
             data=miris)

viz <-
  list(kk=gg+facet_grid(kingdom ~ kingdom),
       kx=gg+facet_grid(kingdom ~ .),
       xk=gg+facet_grid(. ~ kingdom),
       kS=gg+facet_grid(kingdom ~ Species),
       Sk=gg+facet_grid(Species ~ kingdom))

translatePattern <-
  paste0("translate[(]",
         "(?<x>.*?)",
         ",",
         "(?<y>.*?)",
         "[)]")

test_that("facet_grid(1 row and/or 1 column) is fine", {
  info <- animint2HTML(viz)
  expect_axes <- function(plot.name, expected.x, expected.y){
    svg.xpath <- sprintf("//svg[@id='%s']", plot.name)
    x.xpath <- paste0(svg.xpath, "//g[@id='xaxis']")
    x.axes <- getNodeSet(info$html, x.xpath)
    expect_equal(length(x.axes), expected.x)
    y.xpath <- paste0(svg.xpath, "//g[@id='yaxis']")
    y.axes <- getNodeSet(info$html, y.xpath)
    expect_equal(length(y.axes), expected.y)
    xtitle.xpath <- paste0(svg.xpath, "//text[@id='xtitle']")
    xtitle.nodes <- getNodeSet(info$html, xtitle.xpath)
    expect_equal(length(xtitle.nodes), 1)
    xtitle.node <- xtitle.nodes[[1]]
    xtitle.attrs <- xmlAttrs(xtitle.node)
    trans.mat <- str_match_perl(xtitle.attrs[["transform"]], translatePattern)
    trans.y <- as.numeric(trans.mat[, "y"])
    ## 400 is the default animint plot height.
    expect_less_than(trans.y, 400)
  }
  expect_axes("kk", 1, 1)
  expect_axes("kx", 1, 1)
  expect_axes("xk", 1, 1)
  expect_axes("kS", 3, 1)
  expect_axes("Sk", 1, 3)
})

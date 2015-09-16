library(animint)
library(testthat)

context("many facets")

n.circles <- 40
df <- data.frame(x=0, y=0, facet=1:n.circles)
gg <- ggplot()+
  geom_point(aes(x, y), data=df)+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "cm"))
viz <-
  list(horizontal=gg+
         theme_animint(width=n.circles * 30 + 50)+
         facet_grid(. ~ facet),
       vertical=gg+
         theme_animint(height=n.circles * 30 + 50)+
         facet_grid(facet ~ .))
info <- animint2HTML(viz)

test_that("points have positive positions", {
  circle.list <-
    getNodeSet(info$html, '//circle')
  expect_equal(length(circle.list), n.circles * 2)
  xy.vec <- as.numeric(sapply(circle.list, function(circle){
    xmlAttrs(circle)[c("cx", "cy")]
  }))
  expect_true(all(0 < xy.vec))
})

test_that("no horizontal space between border_rects", {
  rect.list <-
    getNodeSet(info$html, '//svg[@id="horizontal"]//rect[@class="border_rect"]')
  expect_equal(length(rect.list), n.circles)
  first <- xmlAttrs(rect.list[[1]])
  first.right <- as.numeric(first[["x"]])+as.numeric(first[["width"]])
  second <- xmlAttrs(rect.list[[2]])
  second.left <- as.numeric(second[["x"]])
  expect_equal(first.right, second.left)
})

test_that("no vertical space between border_rects", {
  rect.list <-
    getNodeSet(info$html, '//svg[@id="vertical"]//rect[@class="border_rect"]')
  expect_equal(length(rect.list), n.circles)
  first <- xmlAttrs(rect.list[[1]])
  first.bottom <- as.numeric(first[["y"]])+as.numeric(first[["height"]])
  second <- xmlAttrs(rect.list[[2]])
  second.top <- as.numeric(second[["y"]])
  expect_equal(first.bottom, second.top)
})

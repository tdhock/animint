library(animint)
library(testthat)

context("many facets")

n.circles <- 40
df <- data.frame(x=0, y=0, facet=1:n.circles)
viz <-
  list(horizontal=ggplot()+
         theme_animint(width=n.circles * 30 + 50)+
         geom_point(aes(x, y), data=df)+
         facet_grid(. ~ facet),
       vertical=ggplot()+
         theme_animint(height=n.circles * 30 + 50)+
         geom_point(aes(x, y), data=df)+
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


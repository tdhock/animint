acontext("Inf")

limits <- data.frame(
  i=1:3,
  lower=c(-Inf, 0, -1),
  upper=c(1, 2, Inf))
viz <- list(
  gg=ggplot()+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(side ~ top)+
    geom_segment(aes(
      i, lower, yend=upper, xend=i),
      data=data.frame(limits, side="yInf", top="left"))+
    geom_segment(aes(
      lower, i, xend=upper, yend=i),
      data=data.frame(limits, side="xInf", top="right")))
info <- animint2HTML(viz)

## First panel, test y values.
bg.rect <- getNodeSet(
  info$html,
  "//g[contains(@class, 'bgr1')]//rect[@class='background_rect']")[[1]]
attr.vec <- xmlAttrs(bg.rect)
panel.top <- as.numeric(attr.vec[["y"]])
h <- as.numeric(attr.vec[["height"]])
panel.bottom <- panel.top + h
line.list <- getNodeSet(
  info$html,
  "//g[contains(@class, 'PANEL1')]//line")
attr.mat <- sapply(line.list, xmlAttrs)
seg.bottom <- as.numeric(attr.mat["y1",])
seg.top <- as.numeric(attr.mat["y2",])
n.top <- sum(seg.top == panel.top)
n.bottom <- sum(seg.bottom == panel.bottom)
test_that("one y at top of panel", {
  expect_equal(n.top, 1)
})
test_that("one y at bottom of panel", {
  expect_equal(n.bottom, 1)
})

## Last panel, test x values.
bg.rect <- getNodeSet(
  info$html,
  "//g[contains(@class, 'bgr4')]//rect[@class='background_rect']")[[1]]
attr.vec <- xmlAttrs(bg.rect)
panel.left <- as.numeric(attr.vec[["x"]])
w <- as.numeric(attr.vec[["width"]])
panel.right <- panel.left + w
line.list <- getNodeSet(
  info$html,
  "//g[contains(@class, 'PANEL4')]//line")
attr.mat <- sapply(line.list, xmlAttrs)
seg.left <- as.numeric(attr.mat["x1",])
seg.right <- as.numeric(attr.mat["x2",])
n.left <- sum(seg.left == panel.left)
n.right <- sum(seg.right == panel.right)
test_that("one x at left of panel", {
  expect_equal(n.left, 1)
})
test_that("one x at right of panel", {
  expect_equal(n.right, 1)
})


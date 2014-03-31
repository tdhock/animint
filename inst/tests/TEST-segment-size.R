context("segment size")

df <- data.frame(x=c(0, 0),
                 y=c(0, 1),
                 xend=c(1, 1),
                 yend=c(1, 0),
                 size=c(5, 10))

test_that("segment size is translated to stroke-width", {
  viz <- list(segs=ggplot()+
              geom_segment(aes(x, y, xend=xend, yend=yend),
                           data=df, size=1))
  html <- animint2HTML(viz)
  expect_match(html, "stroke-width: 1")
})
              
test_that("segment size range is translated to stroke-width", {
  viz <- list(segs=ggplot()+
              geom_segment(aes(x, y, xend=xend, yend=yend, size=size),
                           data=df)+
              scale_size_identity())
  html <- animint2HTML(viz)
  expect_match(html, "stroke-width: 10")
  expect_match(html, "stroke-width: 5")
})
              


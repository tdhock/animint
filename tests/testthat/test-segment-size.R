context("Segment size")

df <- data.frame(x=c(0, 0),
                 y=c(0, 1),
                 xend=c(1, 1),
                 yend=c(1, 0),
                 size=c(5, 10))

test_that("segment size translates to stroke-width", {
  viz <- list(segs=ggplot()+
              geom_segment(aes(x, y, xend=xend, yend=yend),
                           data=df, size=1))
  info <- animint2HTML(viz)
  ## Need the ^$ so that we do not match e.g. 210.
  expect_styles(info$html, list("stroke-width"="^1[a-z]*$"))
})
              
test_that("segment size range translates to stroke-width", {
  viz <- list(segs=ggplot()+
              geom_segment(aes(x, y, xend=xend, yend=yend, size=size),
                           data=df)+
              scale_size_identity())
  info <- animint2HTML(viz)
  expect_styles(info$html, list("stroke-width"=c("^5[a-z]*$", "^10[a-z]*$")))
})

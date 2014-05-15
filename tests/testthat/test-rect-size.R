context("rect size")

df <- data.frame(xmin=c(0, 0),
                 ymin=c(0, 2),
                 xmax=c(1, 1),
                 ymax=c(1, 3),
                 size=c(0, 5))

test_that("rect size param is translated to stroke-width", {
  viz <- list(segs=ggplot()+
              geom_rect(aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax),
                        data=df, size=5, color="violet"))
  html <- animint2HTML(viz)
  expect_match(html, "stroke-width: 5")
  expect_match(html, "stroke: violet")
  expect_match(html, "fill: black")
})
              
test_that("zero rect size param is translated to stroke-width", {
  viz <- list(segs=ggplot()+
              geom_rect(aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax),
                        data=df, size=0, fill="violet"))
  html <- animint2HTML(viz)
  expect_match(html, "fill: violet")
  expect_match(html, "stroke-width: 0")
})
              
test_that("rect size range is translated to stroke-width", {
  viz <- list(segs=ggplot()+
              geom_rect(aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax,
                            size=size),
                        data=df, color="violet")+
              scale_size_identity())
  html <- animint2HTML(viz)
  expect_match(html, "stroke-width: 0")
  expect_match(html, "stroke: violet")
  expect_match(html, "stroke-width: 5")
})
              


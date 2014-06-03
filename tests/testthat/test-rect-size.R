context("rect size")

df <- data.frame(xmin = c(0, 0),
                 ymin = c(0, 2),
                 xmax = c(1, 1),
                 ymax = c(1, 3),
                 size = c(0, 5))

test_that("rect size translates to stroke-width", {
  viz <-
    list(segs = ggplot() +
         geom_rect(data = df, size = 5, color = "violet",
                   aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)))
  info <- animint2HTML(viz)
  expect_styles(info, list("stroke-width"="^5[a-z]*$"))
})

test_that("zero rect size translates to stroke-width", {
  viz <- list(segs = ggplot() +
              geom_rect(data = df, size = 0, color = "violet",
                        aes(xmin = xmin, ymin = ymin,
                            xmax = xmax, ymax = ymax)))
  info <- animint2HTML(viz)
  expect_styles(info, list("stroke-width"="^0[a-z]*$"))
})

test_that("rect size range translates to stroke-width", {
  viz <- list(segs = ggplot() + geom_rect(data = df, color = "violet",
                 aes(xmin = xmin, ymin = ymin,
                     xmax = xmax, ymax = ymax, size = size)) +
                 scale_size_identity())
  info <- suppressWarnings(animint2HTML(viz))
  expect_styles(info, list("stroke-width"=c("^0[a-z]*$", "^5[a-z]*$")))
})

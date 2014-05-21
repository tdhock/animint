context <- "rect-size"
context(context)

# Setup a directory specific to this context
# Note this should be running in tests/testthat
current.dir <- file.path(getwd(), context)
if (!file_test("-d", current.dir)) dir.create(current.dir)
# Remove the directory when this context is done
on.exit(unlink(current.dir, recursive = TRUE))

df <- data.frame(xmin = c(0, 0),
                 ymin = c(0, 2),
                 xmax = c(1, 1),
                 ymax = c(1, 3),
                 size = c(0, 5))

viz1 <- list(segs = ggplot() + geom_rect(data = df, size = 5, color = "violet",
                    aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)))
viz2 <- list(segs = ggplot() + geom_rect(data = df, size = 0, color = "violet",
                    aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)))
viz3 <- list(segs = ggplot() + geom_rect(data = df, color = "violet",
                    aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, size = size)) +
                    scale_size_identity())
info1 <- animint2HTML(viz1, context, "size5")
info2 <- animint2HTML(viz2, context, "size0")
info3 <- animint2HTML(viz3, context, "size") #suppressWarning?
html1 <- parse_page(info1)
html2 <- parse_page(info2)
html3 <- parse_page(info3)

test_that("rect size translates to stroke-width", {
  style1 <- getStyle(html1)
  expect_equal(style1[1], "opacity: 1; stroke-dasharray: 0; stroke-width: 5; fill: violet; stroke: violet;")
  expect_equal(style1[2], "opacity: 1; stroke-dasharray: 0; stroke-width: 5; fill: violet; stroke: violet;")
})

test_that("zero rect size translates to stroke-width", {
  
  style2 <- getStyle(html2)
  expect_equal(style2[1], "opacity: 1; stroke-dasharray: 0; stroke-width: 0; fill: violet; stroke: violet;")
  expect_equal(style2[2], "opacity: 1; stroke-dasharray: 0; stroke-width: 0; fill: violet; stroke: violet;")
})

test_that("rect size range translates to stroke-width", {
  
  style3 <- getStyle(html3)
  expect_equal(style3, c("opacity: 1; stroke-dasharray: 0; stroke-width: 0; fill: violet; stroke: violet;",
                         "opacity: 1; stroke-dasharray: 0; stroke-width: 5; fill: violet; stroke: violet;"))
})

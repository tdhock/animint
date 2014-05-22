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

test_that("rect size translates to stroke-width", {
  viz <- list(segs = ggplot() + geom_rect(data = df, size = 5, color = "violet",
                                           aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)))
  info <- animint2HTML(viz, context, "size5")
  html <- parse_page(info)
  geom <- getNodeSet(html, '//rect[@class="geom"]')
  style <- as.character(sapply(geom, function(x) xmlAttrs(x)["style"]))
  styles <- strsplit(style, ";") #chop up style attributes
  stroke <- sapply(styles, function(x) x[grep("stroke-width", x)]) # grab stroke-width
  expect_that(stroke, matches("stroke-width: 5[a-z]*"))  # some browsers add 'px' to the value
})

test_that("zero rect size translates to stroke-width", {
  viz <- list(segs = ggplot() + geom_rect(data = df, size = 0, color = "violet",
                                          aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)))
  info <- animint2HTML(viz, context, "size0")
  html <- parse_page(info)
  geom <- getNodeSet(html, '//rect[@class="geom"]')
  style <- as.character(sapply(geom, function(x) xmlAttrs(x)["style"]))
  styles <- strsplit(style, ";")
  stroke <- sapply(styles, function(x) x[grep("stroke-width", x)])
  expect_that(stroke, matches("stroke-width: 0[a-z]*"))
})

test_that("rect size range translates to stroke-width", {
  viz <- list(segs = ggplot() + geom_rect(data = df, color = "violet",
                 aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, size = size)) +
                 scale_size_identity())
  info <- suppressWarnings(animint2HTML(viz, context, "size"))
  html <- parse_page(info)
  geom <- getNodeSet(html, '//rect[@class="geom"]')
  style <- as.character(sapply(geom, function(x) xmlAttrs(x)["style"]))
  styles <- strsplit(style, ";")
  stroke <- sapply(styles, function(x) x[grep("stroke-width", x)])
  expect_that(stroke[1], matches("stroke-width: 0[a-z]*"))
  expect_that(stroke[2], matches("stroke-width: 5[a-z]*"))
})

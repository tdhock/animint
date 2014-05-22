context <- "segment-size"
context(context)

# Setup a directory specific to this context
# Note this should be running in tests/testthat
current.dir <- file.path(getwd(), context)
if (!file_test("-d", current.dir)) dir.create(current.dir)
# Remove the directory when this context is done
on.exit(unlink(current.dir, recursive = TRUE))

df <- data.frame(x=c(0, 0),
                 y=c(0, 1),
                 xend=c(1, 1),
                 yend=c(1, 0),
                 size=c(5, 10))

test_that("segment size translates to stroke-width", {
  viz <- list(segs=ggplot()+
              geom_segment(aes(x, y, xend=xend, yend=yend),
                           data=df, size=1))
  info <- animint2HTML(viz, context, "size1")
  html <- parse_page(info)
  geom <- getNodeSet(html, '//line[@class="geom"]')
  style <- as.character(sapply(geom, function(x) xmlAttrs(x)["style"]))
  styles <- strsplit(style, ";") #chop up style attributes
  stroke <- sapply(styles, function(x) x[grep("stroke-width", x)]) # grab stroke-width
  expect_that(stroke, matches("stroke-width: 1[a-z]*"))  # some browsers add 'px' to the value
})
              
test_that("segment size range translates to stroke-width", {
  viz <- list(segs=ggplot()+
              geom_segment(aes(x, y, xend=xend, yend=yend, size=size),
                           data=df)+
              scale_size_identity())
  info <- animint2HTML(viz, context, "size")
  html <- parse_page(info)
  geom <- getNodeSet(html, '//line[@class="geom"]')
  style <- as.character(sapply(geom, function(x) xmlAttrs(x)["style"]))
  styles <- strsplit(style, ";")
  stroke <- sapply(style, function(x) x[grep("stroke-width", x)])
  expect_that(stroke[1], matches("stroke-width: 5[a-z]*"))
  expect_that(stroke[2], matches("stroke-width: 10[a-z]*"))
})
              


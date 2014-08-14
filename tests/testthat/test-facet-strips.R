context("facet-strips")

p <- ggplot(mtcars, aes(mpg, wt)) + 
  geom_point(colour='grey50', size = 4) + 
  geom_point(aes(colour = cyl)) 

viz1 <- p + facet_grid(cyl~am, labeller = label_both)
viz2 <- p + facet_wrap(~cyl+am)

test_that("facet_grid() strip labels are placed correctly", {
  info <- animint2HTML(list(plot = viz1))
  top <- getNodeSet(info$html, "//g[@id='top_strip']")
  # there should one be one 'top_strip' group (their children contain the vital info)
  kids <- xmlChildren(top[[1]])
  labs <- as.character(sapply(kids, xmlValue))
  expect_equal(labs, c("am: 0", "am: 1"))
  attrs <- lapply(kids, xmlAttrs)
  styles <- as.character(sapply(attrs, "[[", "style"))
  expect_equal(styles, rep("text-anchor: middle;", 2))
  transforms <- as.character(sapply(attrs, "[[", "transform"))
  expect_transform(transforms[1], c(108.5, 17, 0), context = c("translate", "rotate"))
  expect_transform(transforms[2], c(308.5, 17, 0), context = c("translate", "rotate"))
  # there should one be one 'right_strip' group (their children contain the vital info)
  right <- getNodeSet(info$html, "//g[@id='right_strip']")
  kids <- xmlChildren(right[[1]])
  labs <- as.character(sapply(kids, xmlValue))
  expect_equal(labs, c("cyl: 4", "cyl: 6", "cyl: 8"))
  attrs <- lapply(kids, xmlAttrs)
  styles <- as.character(sapply(attrs, "[[", "style"))
  expect_equal(styles, rep("text-anchor: middle;", 3))
  transforms <- as.character(sapply(attrs, "[[", "transform"))
  expect_transform(transforms[1], c(389, 67, 90), context = c("translate", "rotate"))
  expect_transform(transforms[2], c(389, 200, 90), context = c("translate", "rotate"))
  expect_transform(transforms[3], c(389, 333, 90), context = c("translate", "rotate"))
})

test_that("facet_wrap() strip labels are placed correctly", {
  info <- animint2HTML(list(plot = viz2))
  top <- getNodeSet(info$html, "//g[@id='top_strip']")
  kids <- xmlChildren(top[[1]])
  labs <- as.character(sapply(kids, xmlValue))
  expect_equal(labs, c("4, 0", "4, 1", "6, 0", 
                       "6, 1", "8, 0", "8, 1"))
})
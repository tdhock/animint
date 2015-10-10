acontext("One character value")

df <-
  data.frame(numeric=1,
             integer=1L,
             character="character value",
             factor=factor("factor value"),
             stringsAsFactors=FALSE)

viz <-
  list(dots=qplot(factor, character, data=df))

info <- animint2HTML(viz)

test_that("categorical variables with 1 value", {
  xlab.vec <- as.character(info$plots$dots$axis$xlab)
  expect_true(is.list(info$plots$dots$axis$x))
  expect_identical(xlab.vec, "factor value")
  ylab.vec <- as.character(info$plots$dots$axis$ylab)
  expect_true(is.list(info$plots$dots$axis$y))
  expect_identical(ylab.vec, "character value")
  xticks <- getNodeSet(info$html, "//g[@id='xaxis']/g[@class='tick major']")
  expect_identical(sapply(xticks, xmlValue), "factor value")
  yticks <- getNodeSet(info$html, "//g[@id='yaxis']/g[@class='tick major']")
  expect_identical(sapply(yticks, xmlValue), "character value")
})

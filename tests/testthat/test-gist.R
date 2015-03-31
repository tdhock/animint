context("gists")

test_that("animint2gist() returns an object of class 'gist'", {
  g <- animint2gist(list(p = qplot(1:10)), screenshot = FALSE)
  expect_is(g, "gist")
  gistr::delete(g)
})

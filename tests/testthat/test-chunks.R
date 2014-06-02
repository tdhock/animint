context("chunks")

test_that("produce as many chunk files as specified", {
  viz <- list(iris=ggplot()+
    geom_point(aes(Petal.Width, Sepal.Length, showSelected=Species),
               data=iris, chunk_vars="Species"))
  tdir <- tempfile()
  dir.create(tdir)
  tsv.files <- Sys.glob(file.path(tdir, "*.tsv"))
  expect_equal(length(tsv.files), 0)
  animint2dir(viz, tdir, open.browser=FALSE)
  tsv.files <- Sys.glob(file.path(tdir, "*.tsv"))
  expect_equal(length(tsv.files), 3)
  
  viz <- list(iris=ggplot()+
    geom_point(aes(Petal.Width, Sepal.Length, showSelected=Species),
               data=iris, chunk_vars=character()))
  tdir <- tempfile()
  dir.create(tdir)
  tsv.files <- Sys.glob(file.path(tdir, "*.tsv"))
  expect_equal(length(tsv.files), 0)
  animint2dir(viz, tdir, open.browser=FALSE)
  tsv.files <- Sys.glob(file.path(tdir, "*.tsv"))
  expect_equal(length(tsv.files), 1)
})

test_that("produce informative errors for bad chunk_vars", {
  viz <- list(iris=ggplot()+
    geom_point(aes(Petal.Width, Sepal.Length, showSelected=Species),
               data=iris, chunk_vars="species"))
  expect_error({
    animint2dir(viz, open.browser=FALSE)
  }, "invalid chunk_vars species; possible showSelected variables: Species")
  
  viz <- list(iris=ggplot()+
    geom_point(aes(Petal.Width, Sepal.Length, showSelected=Species),
               data=iris, chunk_vars=NA))
  expect_error({
    animint2dir(viz, open.browser=FALSE)
  }, paste("chunk_vars must be a character vector;",
           "use chunk_vars=character() to specify 1 chunk"), fixed=TRUE)
})

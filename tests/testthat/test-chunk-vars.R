acontext("chunk vars")

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

data(breakpoints)
only.error <- subset(breakpoints$error,type=="E")
only.segments <- subset(only.error,bases.per.probe==bases.per.probe[1])
signal.colors <- c(estimate="#0adb0a",
                   latent="#0098ef")
breakpointError <- 
  list(signal=ggplot()+
         geom_point(aes(position, signal, showSelected=bases.per.probe),
                    data=breakpoints$signals)+
         geom_line(aes(position, signal), colour=signal.colors[["latent"]],
                   data=breakpoints$imprecision)+
         geom_segment(aes(first.base, mean, xend=last.base, yend=mean,
                          showSelected=segments,
                          showSelected2=bases.per.probe),
                      colour=signal.colors[["estimate"]],
                      data=breakpoints$segments)+
         geom_vline(aes(xintercept=base,
                        showSelected=segments,
                        showSelected2=bases.per.probe),
                    colour=signal.colors[["estimate"]],
                    linetype="dashed",
                    data=breakpoints$breaks),
       error=ggplot()+
         geom_vline(aes(xintercept=segments, clickSelects=segments),
                    data=only.segments, lwd=17, alpha=1/2)+
         geom_line(aes(segments, error, group=bases.per.probe,
                       clickSelects=bases.per.probe),
                   data=only.error, lwd=4))

bytes.used <- function(file.vec, apparent.size = FALSE){
  ## Note: the apparent.size flag gives sizes that are consistent
  ## with file.info, but those sizes actually under-estimate the
  ## actual amount of space used on disk.
  file.str <- paste(file.vec, collapse=" ")
  if(apparent.size){
    cmd <- paste("ls -l", file.str, "| awk '{print $5}'")
  } else{
    cmd <- paste("du -k", file.str, "| awk '{print $1 * 1024}'")
  }
  tryCatch({
    du.lines <- system(cmd, intern=TRUE)
    as.integer(sub("\t.*", "", du.lines))
  }, error=function(e){
    rep(NA_integer_, length(file.vec))
  })
}

test.paths <- 
  c(tempfile=tempfile(),
    HOME=file.path(Sys.getenv("HOME"), "ANIMINT_TEST_FOO"),
    getwd=file.path(getwd(),"ANIMINT_TEST_FOO"))
for(f in test.paths){
  unlink(f)
  cat("foo", file=f)
}
du.bytes <- bytes.used(test.paths)
apparent.bytes <- bytes.used(test.paths, apparent.size = TRUE)
byte.df <- data.frame(du.bytes, apparent.bytes,
                      file.size=file.size(test.paths),
                      test.paths)

test_that("default chunks are at least 4KB", {
  tdir <- tempfile()
  dir.create(tdir)
  tsv.files <- Sys.glob(file.path(tdir, "*.tsv"))
  expect_equal(length(tsv.files), 0)
  animint2dir(breakpointError, tdir, open.browser=FALSE)
  tsv.files <- Sys.glob(file.path(tdir, ".+chunk[0-9]+.tsv"))  # exclude common tsv
  geom <- sub("_.*", "", basename(tsv.files))
  files.by.geom <- split(tsv.files, geom)
  for(files in files.by.geom){
    if(length(files) > 1){
      info <- file.info(files)
      expect_true(all(4096 < info$size))
    }
  }
})

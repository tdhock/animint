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

data(WorldBank)

not.na <- subset(WorldBank, !(is.na(life.expectancy) | is.na(fertility.rate)))
subset(not.na, is.na(not.na$population))
not.na[not.na$country=="Kuwait", "population"] <- 1700000

good <-
  list(scatter=ggplot()+
       geom_point(aes(life.expectancy, fertility.rate, clickSelects=country,
                      showSelected=year, colour=region, size=population,
                      tooltip=paste(country, "population", population),
                      key=country), # key aesthetic for animated transitions!
                  data=not.na)+
       geom_text(aes(life.expectancy, fertility.rate, label=country,
                     showSelected=country, showSelected2=year,
                     key=country), #also use key here!
                 data=not.na)+
       scale_size_animint(breaks=10^(5:9))+
       make_text(WorldBank, 55, 9, "year"),
       ts=ggplot()+
       make_tallrect(WorldBank, "year")+
       geom_line(aes(year, life.expectancy, group=country, colour=region,
                     clickSelects=country),
                 data=WorldBank, size=4, alpha=3/5),
       time=list(variable="year",ms=3000),
       bar=ggplot()+
       theme_animint(height=2400)+
       geom_bar(aes(country, life.expectancy, fill=region,
                    showSelected=year, clickSelects=country),
                data=WorldBank, stat="identity", position="identity")+
       coord_flip(),
       duration=list(year=1000),
       first=list(year=1975, country="United States"),
       title="World Bank data (single selection)")

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

bytes.used <- function(file.vec, ...){
  ## Note: the --apparent-size flag gives sizes that are consistent
  ## with file.info, but those sizes actually under-estimate the
  ## actual amount of space used on disk.
  file.str <- paste(file.vec, collapse=" ")
  cmd <- paste("du --block-size=1", ..., file.str)
  du.lines <- system(cmd, intern=TRUE)
  as.integer(sub("\t.*", "", du.lines))
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
apparent.bytes <- bytes.used(test.paths, "--apparent-size")
byte.df <- data.frame(du.bytes, apparent.bytes,
                      file.size=file.size(test.paths),
                      test.paths)
print(byte.df)

test_that("default chunks are at least 4KB", {
  tdir <- tempfile()
  dir.create(tdir)
  tsv.files <- Sys.glob(file.path(tdir, "*.tsv"))
  expect_equal(length(tsv.files), 0)
  animint2dir(breakpointError, tdir, open.browser=FALSE)
  tsv.files <- Sys.glob(file.path(tdir, "*.tsv"))
  geom <- sub("_.*", "", basename(tsv.files))
  files.by.geom <- split(tsv.files, geom)
  for(files in files.by.geom){
    if(length(files) > 1){
      info <- file.info(files)
      expect_true(all(4096 < info$size))
    }
  }
})

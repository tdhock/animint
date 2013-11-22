##' Make documentation web site.
##' @param doc.dir containing example subdirectories.
##' @return nothing.
##' @author Toby Dylan Hocking
##' @export
makeDocs <- function(doc.dir){
  require(knitr)
  viz.path <- Sys.glob(file.path(doc.dir, "*", "viz.R"))
  sub.dirs <- dirname(viz.path)
  Rmd.tmp.file <- file.path(doc.dir, "template.Rmd")
  Rmd.tmp.lines <- readLines(Rmd.tmp.file)
  Rmd.template <- paste(Rmd.tmp.lines, collapse="\n")
  owd <- getwd()
  on.exit(setwd(owd))
  base <- basename(sub.dirs)
  n.vars <- n.interactive <- name.vars <- name.interactive <-
    rep(NA, length(base))
  animated <- rep("no", length(base))
  for(sub.dir.i in seq_along(sub.dirs)){
    sub.dir <- sub.dirs[[sub.dir.i]]
    setwd(sub.dir)
    ##print(sub.dir)
    vizLines <- readLines("viz.R")
    vizCode <- paste(vizLines, collapse="\n")
    descLines <- tryCatch({
      readLines("description.md")
    },error=function(e){
      "There should be an interactive animation above."
    })
    desc <- paste(descLines, collapse="\n")
    Rmd.filled <- sub("DESCRIPTION", desc, sub("VIZ", vizCode, Rmd.template))
    ##Rmd.filled <- sub("VIZ", vizCode, Rmd.template)
    writeLines(Rmd.filled, "viz.Rmd")
    convert.cmd <- "convert big.png -geometry 200 small.png"
    system(convert.cmd)
    knit2html("viz.Rmd")
    meta <- fromJSON("plot.json")
    aes.list <- lapply(meta$geoms, "[[", "aes")
    i.aes <- c("clickSelects", "showSelected", "showSelected2", "showSelected3")
    ivars <- sapply(aes.list, "[", i.aes)
    uniq <- function(x){
      y <- unique(as.vector(unlist(x)))
      not.na <- !is.na(y)
      not.trivial <- !y %in% c("x", "y", "label", "xmin", "xmax", "")
      not.derived <- !grepl("interaction", y)
      y[not.na & not.trivial & not.derived]
    }
    uivars <- uniq(ivars)
    uvars <- uniq(aes.list)
    n.interactive[[sub.dir.i]] <- length(uivars)
    n.vars[[sub.dir.i]] <- length(uvars)
    name.interactive[[sub.dir.i]] <- paste(uivars, collapse=", ")
    name.vars[[sub.dir.i]] <- paste(uvars, collapse=", ")
    if(!is.null(meta$time)){
      animated[[sub.dir.i]] <- "yes"
    }
    setwd(owd)
  }
  ## Now setup the index.
  tr.tmp <-
    paste('<tr>',
          '<td>%s</td>',
          '<td><a href="%s/viz.html"><img src="%s/small.png" /></a></td>',
          '<td>%s</td>',
          '<td class="numeric">%d</td>',
          '<td>%s</td>',
          '<td class="numeric">%d</td>',
          '<td>%s</td>',
          '</tr>',
          collapse="\n")
  tr.vec <- sprintf(tr.tmp, base, base, base, animated,
                    n.interactive, name.interactive, 
                    n.vars, name.vars)
  trs <- paste(tr.vec, collapse="\n")
  index.tmp.file <- file.path(doc.dir,"index-template.html")
  index.tmp.lines <- readLines(index.tmp.file)
  index.tmp <- paste(index.tmp.lines, collapse="\n")
  vers <- packageVersion("animint")
  index.filled <- sub("VERSION", vers, sub("TABLE", trs, index.tmp))
  index.file <- file.path(doc.dir, "index.html")
  writeLines(index.filled, index.file)
}

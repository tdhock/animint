animint <- function
### Convert a list of ggplots to an interactive animation.
(plot.list,
### List of ggplots with showSelected, time, and/or clickSelects aes.
 out.dir=tempfile()
### Directory to store html/js/csv files.
 ){
  stopifnot(is.list(plot.list))
  dir.create(out.dir,showWarnings=FALSE)
  i <- 1
  special <- c(showSelected="subset",time="subset",clickSelects="hilite")
  result <- list(geoms=list(),selectors=list(),plots=list())
  for(plot.name in names(plot.list)){
    p <- plot.list[[plot.name]]
    result$plots[[plot.name]] <- list()
    stopifnot(is.ggplot(p))
    ##print(p) ## to catch any bugs.
    for(l in p$layers){
      g <- list()
      g.name <- sprintf("geom%d",i)
      g$classed <- g.name
      g$geom <- l$geom$objname
      g$aes <- as.character(l$mapping)
      ## Output data to csv.
      csv.name <- sprintf("%s.csv",g.name)
      g$data <- csv.name
      write.csv(l$data,file.path(out.dir,csv.name),
                quote=FALSE,row.names=FALSE)
      ## Construct the selector.
      for(s in names(special)){
        if(s %in% names(g$aes)){
          v.name <- g$aes[[s]]
          l.name <- special[[s]]
          if(!v.name %in% names(result$selectors)){
            result$selectors[[v.name]] <- list()
          }
          if(!l.name %in% names(result$selectors[[v.name]])){
            result$selectors[[v.name]][[l.name]] <- list()
          }
          result$selectors[[v.name]][[l.name]] <-
            c(result$selectors[[v.name]][[l.name]],g.name)
        }
      }
      result$geoms[[g.name]] <- g
      result$plots[[plot.name]] <- c(result$plots[[plot.name]],g.name)
      i <- i+1
    }
  }
  json <- RJSONIO::toJSON(result)
  cat(json,file=file.path(out.dir,"plot.json"))
  ## TODO: copy files.
}

gg2animint <- structure(function
### Convert a list of ggplots to an interactive animation.
(plot.list,
### List of ggplots with showSelected, time, and/or clickSelects aes.
 out.dir=tempfile(),
### Directory to store html/js/csv files.
 open.browser=interactive()
### Open a web browser?
 ){
  stopifnot(is.list(plot.list))
  dir.create(out.dir,showWarnings=FALSE)
  i <- 1
  special <- c(showSelected="subset",time="subset",clickSelects="hilite")
  result <- list(geoms=list(),selectors=list(),plots=list())
  for(plot.name in names(plot.list)){
    p <- plot.list[[plot.name]]
    result$plots[[plot.name]] <- list(geoms=list())
    stopifnot(is.ggplot(p))
    ##print(p) ## to catch any bugs.
    ranges <- list(x=c(),y=c())
    range.map <- c(xintercept="x",x="x",y="y")
    for(l in p$layers){
      g <- list()
      g.name <- sprintf("geom%d",i)
      g$classed <- g.name
      g$geom <- l$geom$objname
      g$params <- l$geom_params
      g$aes <- lapply(l$mapping,function(x){
        if(is.symbol(x))return(as.character(x))
        if(is.numeric(x))return(x)
        str(x)
        stop("dont know how to convert")
      })
      subset.vars <- c(g$aes[grepl("showSelected|time",names(g$aes))],
                       g$aes[names(g$aes)=="group"])
      g$subord <- as.list(names(subset.vars))
      g$subvars <- as.list(subset.vars)
      ## Figure out ranges (duplicated with ggplot2).
      for(aesname in names(range.map)){
        if(aesname %in% names(g$aes)){
          var.name <- g$aes[[aesname]]
          ax.name <- range.map[[aesname]]
          r <- range(l$data[[var.name]])
          ranges[[ax.name]] <- rbind(ranges[[ax.name]],r)
        }
      }
      ## Output data to csv.
      csv.name <- sprintf("%s.csv",g.name)
      g$data <- csv.name
      g$types <- sapply(l$data,class)
      write.csv(l$data,file.path(out.dir,csv.name),
                quote=FALSE,row.names=FALSE)

      ## Construct the selector.
      for(s in names(special)){
        if(s %in% names(g$aes)){
          v.name <- g$aes[[s]]
          l.name <- special[[s]]
          if(!v.name %in% names(result$selectors)){
            ## select the first one. TODO: customize.
            result$selectors[[v.name]] <- list(selected=l$data[[v.name]][1])
          }
          if(!l.name %in% names(result$selectors[[v.name]])){
            result$selectors[[v.name]][[l.name]] <- list()
          }
          result$selectors[[v.name]][[l.name]] <-
            c(result$selectors[[v.name]][[l.name]],g.name)
        }
      }
      result$geoms[[g.name]] <- g
      result$plots[[plot.name]]$geoms <-
        c(result$plots[[plot.name]]$geoms,g.name)
      i <- i+1
    }
    result$plots[[plot.name]]$ranges <- lapply(ranges,range)
    result$plots[[plot.name]]$options <- list(width=300,height=300)
  }
  src.dir <- system.file("htmljs",package="animint")
  to.copy <- Sys.glob(file.path(src.dir, "*"))
  file.copy(to.copy, out.dir)
  json <- RJSONIO::toJSON(result)
  ## TODO: open web browser.
  cat(json,file=file.path(out.dir,"plot.json"))
  if(open.browser){
    browseURL(sprintf("%s/index.html",out.dir))
  }
  invisible(result)
### The R representation of the exported JSON, so we can easily do
### checks.
},ex=function(){
  data(generation.loci)
  ## Example: 2 plots, 2 selectors.
  generations <- data.frame(generation=unique(generation.loci$generation))
  loci <- data.frame(locus=unique(generation.loci$locus))
  two.selectors.not.animated <- {
    list(ts=ggplot()+
         geom_vline(aes(xintercept=generation, clickSelects=generation),
                    data=generations, alpha=1/2, lwd=4)+
         geom_line(aes(generation, frequency, group=population,
                       showSelected=locus), data=generation.loci),
         loci=ggplot()+
         geom_vline(aes(xintercept=locus, clickSelects=locus),
                    data=loci, alpha=1/2, size=4)+
         geom_point(aes(locus, frequency, showSelected=generation,
                        duration=1000),
                    data=generation.loci)
         )}
  gg2animint(two.selectors.not.animated)

  ## Example: 3 plots, 1 selector.
  first <- subset(generation.loci,generation==1)
  ancestral <- do.call(rbind,lapply(split(first,first$locus),with,{
    stopifnot(all(frequency==frequency[1]))
    data.frame(locus=locus[1],ancestral=frequency[1])
  }))
  gl.list <- split(generation.loci,with(generation.loci,list(generation,locus)))
  generation.pop <- do.call(rbind,lapply(gl.list,with,{
    data.frame(generation=generation[1], locus=locus[1],
               estimated=mean(frequency))
  }))
  generation.pop$ancestral <- ancestral$ancestral[generation.pop$locus]

  ## Calculate the subset for just the last generation, to plot.
  generation.loci.last <- subset(generation.loci,generation==max(generation))
  generation.pop.last <- subset(generation.pop,generation==max(generation))
  one.selector.not.animated <- {
    list(ts=ggplot()+
         geom_line(aes(generation, frequency, group=population,
                       showSelected=locus), data=generation.loci),
         predictions=ggplot()+
         geom_point(aes(ancestral, estimated, clickSelects=locus),
                    data=generation.pop.last, size=4, alpha=3/4),
         loci=ggplot()+
         geom_vline(aes(xintercept=locus, clickSelects=locus),
                    data=loci, alpha=1/2, lwd=4)+
         geom_point(aes(locus, frequency), data=generation.loci.last)
         )}
  gg2animint(one.selector.not.animated)
})

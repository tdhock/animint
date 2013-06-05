### Convert a ggplot to a list.
gg2list <- function(p){
  plist <- list(ranges=list(x=c(),y=c()))
  plistextra <- ggplot2::ggplot_build(p)
  for(sc in p$scales$scales){
    ## TODO: make use of other scales than manual.
    if(sc$scale_name == "manual"){
      plist$scales[[sc$aesthetics]] <- sc$palette(0)
    }
  }
  for(i in seq_along(p$layers)){
    g <- layer2list(p, i, plistextra)
    plist$geoms[[i]] <- g
    for(ax.name in names(plist$ranges)){
      plist$ranges[[ax.name]] <-
        c(plist$ranges[[ax.name]], g$ranges[ax.name,])
    }
  }
  plist$ranges <- lapply(plist$ranges, range, na.rm=TRUE)
  plist$axis <- list(
    x = plistextra$panel$ranges[[1]]$x.major,
    xlab = plistextra$panel$ranges[[1]]$x.labels,
    y = plistextra$panel$ranges[[1]]$y.major,
    ylab = plistextra$panel$ranges[[1]]$y.labels
  )
  plist$options <- list(width=300,height=300)
  plist
  ### List representing a ggplot, with elements ranges, scales, geoms,
  ### options.
}

### Convert a layer to a list.
layer2list <- function(p, i, plistextra){
  g <- list(geom=p$layers[[i]]$geom$objname, data=plistextra$data[[i]])
  
  ## use un-named parameters so that they will not be exported
  ## to JSON as a named object, since that causes problems with
  ## e.g. colour.
  g$params <- p$layers[[i]]$geom_params
  for(p.name in names(g$params)){
    names(g$params[[p.name]]) <- NULL
  }
  g$aes <- list()
  for(aes.name in names(p$layers[[i]]$mapping)){
    x <- p$layers[[i]]$mapping[[aes.name]]
    g$aes[[aes.name]] <- if(is.symbol(x)){
      as.character(x)
    }else if(is.language(x)){
      newcol <- as.character(as.expression(x))
      g$data[[newcol]] <- eval(x, g$data)
      newcol
    }else{
      str(x)
      stop("don't know how to convert")
    }
  }
  some.vars <- c(g$aes[grepl("showSelected",names(g$aes))])
  g$update <- c(some.vars, g$aes[names(g$aes)=="clickSelects"])
  subset.vars <- c(some.vars, g$aes[names(g$aes)=="group"])
  g$subord <- as.list(names(subset.vars))
  g$subvars <- as.list(subset.vars)
  ## TODO: use actual ggplot2 x and y scales! How?
  g$ranges <- matrix(c(plistextra$panel$ranges[[1]]$x.range, 
                       plistextra$panel$ranges[[1]]$y.range),
                     2,2,dimnames=list(axis=c("x","y"),limit=c("min","max")), byrow=TRUE)
  #   range.map <- c(xintercept="x",x="x",xend="x",xmin="x",xmax="x",
  #                  yintercept="y",y="y",yend="y",ymin="y",ymax="y")
  #   for(aesname in names(range.map)){
  #     if(aesname %in% names(g$aes)){
  #       var.name <- g$aes[[aesname]]
  #       ax.name <- range.map[[aesname]]
  #       r <- range(g$data[[var.name]], na.rm=TRUE, finite=TRUE)
  #       g$ranges[ax.name,] <- range(c(g$ranges[ax.name,],r),na.rm=TRUE)
  #       ## TODO: handle Inf like in ggplot2.
  #       size <- r[2]-r[1]
  #       g$data[[var.name]][g$data[[var.name]]==Inf] <- r[2]+size
  #       g$data[[var.name]][g$data[[var.name]]==-Inf] <- r[1]-size
  #     }
  #   }
  g
  ### List representing a layer.
}

gg2animint <- structure(function
### Convert a list of ggplots to an interactive animation.
(plot.list,
### List of named ggplots with showSelected and clickSelects
### aesthetics.
 out.dir=tempfile(),
### Directory to store html/js/csv files.
 open.browser=interactive()
### Open a web browser?
 ){
  ## Check that it is a list and every element is named.
  stopifnot(is.list(plot.list))
  stopifnot(!is.null(names(plot.list)))
  stopifnot(all(names(plot.list)!=""))
  
  plist <- list() ## for extracted plots.
  olist <- list() ## for options.
  df.list <- list() ## for data.frames so we can look at their values
                    ## to create an animation.

  ## Extract essential info from ggplots, reality checks.
  for(plot.name in names(plot.list)){
    p <- plot.list[[plot.name]]
    if(is.ggplot(p)){
      plist[[plot.name]] <- gg2list(p)
    }else if(is.list(p)){ ## for options.
      olist[[plot.name]] <- p
    }else{
      stop("list items must be ggplots or option lists")
    }
  }

  dir.create(out.dir,showWarnings=FALSE)
  i <- 1 #geom counter.
  result <- list(geoms=list(), selectors=list(), plots=list())
  for(plot.name in names(plist)){
    p <- plist[[plot.name]]
    result$plots[[plot.name]]$geoms <- list()
    for(g in p$geoms){
      g$classed <- sprintf("geom%d_%s_%s", i, g$geom, plot.name)
      result$plots[[plot.name]]$geoms <-
        c(result$plots[[plot.name]]$geoms, g$classed)
      df.list[[g$classed]] <- g$data
      ## Construct the selector.
      for(v.name in g$update){
        if(!v.name %in% names(result$selectors)){
          ## select the first one. TODO: customize.
          result$selectors[[v.name]] <- list(selected=g$data[[v.name]][1])
        }
        result$selectors[[v.name]]$subset <-
          c(result$selectors[[v.name]]$subset, g$classed)
      }
      ## Output data to csv.
      csv.name <- sprintf("%s.csv", g$classed)
      write.csv(g$data, file.path(out.dir, csv.name),
                quote=FALSE,row.names=FALSE)
      g$types <- as.list(sapply(g$data, class))
      g$data <- csv.name
      ## Finally save to the master geom list.
      result$geoms[[g$classed]] <- g
      i <- i+1
    }
    result$plots[[plot.name]]$scales <- p$scales
    result$plots[[plot.name]]$options <- p$options
    result$plots[[plot.name]]$ranges <- p$ranges
    result$plots[[plot.name]]$axis <- p$axis
  }
  ## add nextgeom so that drawing order is preserved.
  
  if(length(result$geoms)-1>0){
    for(i in 1:(length(result$geoms)-1)){
      result$geoms[[i]]$nextgeom <- result$geoms[[i+1]]$classed
    }
  }
  ## Go through options and add to the list.
  for(v.name in names(olist$duration)){
    for(g.name in result$selectors[[v.name]]$subset){
      result$geoms[[g.name]]$duration <- olist$duration[[v.name]]
    }
  }
  ## Set plot sizes.
  for(d in c("width","height")){
    if(is.list(olist[[d]])){
      if(is.null(names(olist[[d]]))){ #use this size for all plots.
        for(plot.name in names(result$plots)){
          result$plots[[plot.name]]$options[[d]] <- olist[[d]][[1]]
        }
      }else{ #use the size specified for the named plot.
        for(plot.name in names(olist[[d]])){
          result$plots[[plot.name]]$options[[d]] <- olist[[d]][[plot.name]]
        }
      }
    }
  }
  if(is.list(olist$time)){
    v.name <- olist$time$variable
    geom.names <- result$selectors[[v.name]]$subset
    u.list <- lapply(geom.names,function(g)unique(df.list[[g]][,v.name]))
    olist$time$sequence <- sort(unique(unlist(u.list)))
    result$time <- olist$time
  }
  ## Finally, copy html/js/json files to out.dir.
  src.dir <- system.file("htmljs",package="animint")
  to.copy <- Sys.glob(file.path(src.dir, "*"))
  file.copy(to.copy, out.dir, overwrite=TRUE)
  json <- RJSONIO::toJSON(result)
  cat(json,file=file.path(out.dir,"plot.json"))
  if(open.browser){
    browseURL(sprintf("%s/index.html",out.dir))
  }
  invisible(result)
### An invisible copy of the R list that was exported to JSON.
},ex=function(){
  data(generation.loci)
  ## Example: 2 plots, 2 selectors.
  generations <- data.frame(generation=unique(generation.loci$generation))
  loci <- data.frame(locus=unique(generation.loci$locus))
  two.selectors.not.animated <- 
    list(ts=ggplot()+
         geom_vline(aes(xintercept=generation, clickSelects=generation),
                    data=generations, alpha=1/2, lwd=4)+
         geom_line(aes(generation, frequency, group=population,
                       showSelected=locus), data=generation.loci),
         loci=ggplot()+
         geom_vline(aes(xintercept=locus, clickSelects=locus),
                    data=loci, alpha=1/2, size=4)+
         geom_point(aes(locus, frequency, showSelected=generation),
                    data=generation.loci),
         duration=list(generation=1000)
         )
  gg2animint(two.selectors.not.animated)

  ## Example: 3 plots, 1 selector.
  first <- subset(generation.loci,generation==1)
  ancestral <- do.call(rbind,lapply(split(first,first$locus),with,{
    stopifnot(all(frequency==frequency[1]))
    data.frame(locus=locus[1],ancestral=frequency[1])
  }))
  gl.list <- split(generation.loci,
                   with(generation.loci,list(generation,locus)))
  generation.pop <- do.call(rbind,lapply(gl.list,with,{
    data.frame(generation=generation[1], locus=locus[1],
               estimated=mean(frequency))
  }))
  generation.pop$ancestral <- ancestral$ancestral[generation.pop$locus]

  ## Calculate the subset for just the last generation, to plot.
  generation.loci.last <- subset(generation.loci,generation==max(generation))
  generation.pop.last <- subset(generation.pop,generation==max(generation))
  one.selector.not.animated <- 
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
         )
  gg2animint(one.selector.not.animated)

  ## Example: 2 plots, 2 selectors, but only interacting with 1 plot.
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
  gg2animint(breakpointError)

  ## Example: animated time series with 3 plots and 2 selectors.
  two.selectors.animated <- 
    list(ts=ggplot()+
         geom_vline(aes(xintercept=generation,
                        clickSelects=generation),
                    data=generations, alpha=1/2, lwd=4)+
         geom_line(aes(generation, frequency, group=population,
                       showSelected=locus), data=generation.loci),
         predictions=ggplot()+
         geom_point(aes(ancestral, estimated, showSelected=generation,
                        clickSelects=locus),
                    data=generation.pop, size=4, alpha=3/4),
         loci=ggplot()+
         geom_vline(aes(xintercept=locus, clickSelects=locus),
                    data=loci, alpha=1/2, lwd=4)+
         geom_point(aes(locus, frequency, showSelected=generation),
                    data=generation.loci),
         duration=list(generation=1000),
         time=list(variable="generation",ms=2000))
  gg2animint(two.selectors.animated)

  ## Example: 4 plots, 2 selectors.
  data(intreg)
  signal.colors <- c(estimate="#0adb0a", latent="#0098ef")
  breakpoint.colors <- c("1breakpoint"="#ff7d7d", "0breakpoints"='#f6f4bf')
  model.linetypes <- c(margin="dotted",limit="dashed",regression="solid")
  intreg$annotations$logratio <- max(intreg$sig$log)
  ## To get the bottom 3 plots to line up properly, we need to plot some
  ## geom_blanks bigger than the x range, so we calculate that here.
  blank.items <- with(intreg,{
    list(segments=list(data=selection,x="min.L",y="segments"),
         error=list(data=selection,x="max.L",y="cost"),
         regression=list(data=model,x=c("min.L","max.L"),
           y=c("min.feature","max.feature")),
         intervals=list(data=intervals,x=c("min.L","max.L"),y="feature"))
  })
  Lrange <- c()
  for(N in names(blank.items)){
    L <- blank.items[[N]]
    Lrange <- range(c(Lrange,unlist(L$data[,L$x])),finite=TRUE)
    blank.items[[N]]$yrange <- range(unlist(L$data[,L$y]))
  }
  Lrange[1] <- Lrange[1]-1
  Lrange[2] <- Lrange[2]+1
  for(N in names(blank.items)){
    L <- blank.items[[N]]
    blank.items[[N]]$blank <- data.frame(x=Lrange, y=L$yrange)
  }

  mmir.plot <- 
    list(signal=ggplot()+
         scale_x_continuous("position on chromosome (mega base pairs)",
                            breaks=c(100,200))+
         geom_tallrect(aes(xmin=first.base/1e6, xmax=last.base/1e6,
                           fill=annotation,
                           showSelected=signal),
                       data=intreg$ann)+
         scale_fill_manual(values=breakpoint.colors,guide="none")+
         geom_text(aes((first.base+last.base)/2e6, logratio+1/8,
                       label=annotation,
                       showSelected=signal),
                   data=intreg$ann)+
         geom_blank(aes(first.base/1e6, logratio+2/8), data=intreg$ann)+
         geom_point(aes(base/1e6, logratio,
                        showSelected=signal),
                    data=intreg$sig)+
         geom_segment(aes(first.base/1e6, mean, xend=last.base/1e6, yend=mean,
                          showSelected=signal,
                          showSelected2=segments),
                      data=intreg$seg, colour=signal.colors[["estimate"]])+
         geom_vline(aes(xintercept=base/1e6,
                        showSelected=signal,
                        showSelected2=segments),
                    colour=signal.colors[["estimate"]],
                    linetype="dashed",
                    data=intreg$breaks),
         regression=ggplot()+
         geom_blank(aes(x,y), data=blank.items$regression$blank)+
         geom_segment(aes(min.L, feature, xend=max.L, yend=feature,
                          clickSelects=signal),
                      size=5,
                      data=intreg$int)+
         geom_segment(aes(min.L, min.feature, xend=max.L, yend=max.feature,
                          linetype=line),
                      colour="red",
                      size=3,
                      data=intreg$model)+
         scale_linetype_manual(values=model.linetypes),
         error=ggplot()+
         geom_blank(aes(x,y), data=blank.items$error$blank)+
         geom_segment(aes(min.L, cost, xend=max.L, yend=cost,
                          showSelected=signal), data=intreg$selection),
         segments=ggplot()+
         geom_blank(aes(x,y), data=blank.items$segments$blank)+
         geom_segment(aes(min.L, segments, xend=max.L, yend=segments,
                          showSelected=signal), data=intreg$selection)+
         geom_tallrect(aes(xmin=min.L, xmax=max.L,
                           showSelected=signal,
                           clickSelects=segments),
                       data=intreg$selection,
                       alpha=1/2),
         width=list(800),
         height=list(signal=300,regression=150,error=50,segments=100))
  ## This is a normal ggplot of all the data, subsets of which can be
  ## shown by clicking the plots.
  sig.facets <- mmir.plot$sig+
    facet_grid(segments~signal, scales="free", space="free_x")+
    theme_bw()+
    theme(panel.margin=unit(0,"cm"))
  print(sig.facets)
  gg2animint(mmir.plot)

  ## TODO: mmir.plot is way too complicated, since facets are not yet
  ## implemented in animint. The easier facetted version would look
  ## like this:
  mmir.facet <- 
    list(signal=mmir.plot$signal,
         penalty=ggplot()+
         geom_segment(aes(min.L, feature, xend=max.L, yend=feature,
                          clickSelects=signal),
                      size=5,
                      data=data.frame(intreg$int, what="regression"))+
         geom_segment(aes(min.L, min.feature, xend=max.L, yend=max.feature,
                          linetype=line),
                      colour="red",
                      size=3,
                      data=data.frame(intreg$model, what="regression"))+
         scale_linetype_manual(values=model.linetypes)+
         geom_segment(aes(min.L, cost, xend=max.L, yend=cost,
                          showSelected=signal),
                      data=data.frame(intreg$selection, what="error"))+
         geom_segment(aes(min.L, segments, xend=max.L, yend=segments,
                          showSelected=signal),
                      data=data.frame(intreg$selection, what="segments"))+
         geom_tallrect(aes(xmin=min.L, xmax=max.L,
                           showSelected=signal,
                           clickSelects=segments),
                       data=data.frame(intreg$selection, what="segments"),
                       alpha=1/2)+
         ylab("")+
         xlab("penalty value $L=f(x)$")+ # TODO: mathjax.
         facet_grid(what~.,scales="free"),
         width=list(800),
         height=list(signal=300,regression=150,error=50,segments=100))
  gg2animint(mmir.facet) # doesn't work yet.
  ## This plot has an additional facet for signal, which would not be
  ## present in the interactive plot, but is useful here to see all
  ## the data in regular ggplot2.
  too.many.facets <- mmir.facet$penalty+
    facet_grid(what~signal, scales="free")+
    theme_bw()+
    theme(panel.margin=unit(0, "cm"))
  print(too.many.facets)
})


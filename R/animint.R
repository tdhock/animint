#' Convert a ggplot to a list. Called from gg2animint(). 
#' @param p ggplot2 plot
#' @return list representing a ggplot, with elements, ranges, axes, scales, geoms, and options
#' @export
#' @seealso \code{\link{gg2animint}}
#' @examples
#' gg2list(ggplot() + geom_point(data=data.frame(x=rnorm(100, 3, 1), y=rnorm(100, 5, 1))), aes(x=x, y=y))
#' 
gg2list <- function(p){
  plist <- list()
  plistextra <- ggplot2::ggplot_build(p)
  for(sc in plistextra$plot$scales$scales){
    if(sc$scale_name == "manual"){
      plist$scales[[sc$aesthetics]] <- sc$palette(0)
    }else if(sc$scale_name == "brewer"){
      plist$scales[[sc$aesthetics]] <- sc$palette(length(sc$range$range))
    }else if(sc$scale_name == "hue"){
      plist$scales[[sc$aesthetics]] <- sc$palette(length(sc$range$range))
    }else if(sc$scale_name == "linetype_d"){
      plist$scales[[sc$aesthetics]] <- sc$palette(length(sc$range$range))
    }else if(sc$scale_name == "alpha_c"){
      plist$scales[[sc$aesthetics]] <- sc$palette(sc$range$range)
    }else if(sc$scale_name == "size_c"){
      plist$scales[[sc$aesthetics]] <- sc$palette(sc$range$range)
    }else if(sc$scale_name == "gradient"){
      plist$scales[[sc$aesthetics]] <- ggplot2:::scale_map(sc, ggplot2:::scale_breaks(sc))
    }
  }
  for(i in seq_along(plistextra$plot$layers)){
    ## This is the layer from the original ggplot object.
    L <- plistextra$plot$layers[[i]]

    ## for each layer, there is a correpsonding data.frame which
    ## evaluates the aesthetic mapping.
    df <- plistextra$data[[i]]

    ## This extracts essential info for this geom/layer.
    g <- layer2list(L, df, plistextra$panel$ranges[[1]])
    
    ## Idea: use the ggplot2:::coord_transform(coords, data, scales)
    ## function to handle cases like coord_flip. scales is a list of
    ## 12, coords is a list(limits=list(x=NULL,y=NULL)) with class
    ## e.g. c("cartesian","coord"). The result is a transformed data
    ## frame where all the data values are between 0 and 1. TODO:
    ## change the JS code to reflect this fact.
    g$data <- ggplot2:::coord_transform(plistextra$plot$coord, g$data,
                                        plistextra$panel$ranges[[1]])
    plist$geoms[[i]] <- g

    ## TODO: use ranges calculated by ggplot2.
    
    ## for(ax.name in names(plist$ranges)){
    ##   plist$ranges[[ax.name]] <-
    ##     c(plist$ranges[[ax.name]], g$ranges[ax.name,])
    ## }
  }
  ##plist$ranges <- lapply(plist$ranges, range, na.rm=TRUE)
  
  # Export axis specification as a combination of breaks and
  # labels, on the relevant axis scale (i.e. so that it can
  # be passed into d3 on the x axis scale instead of on the 
  # grid 0-1 scale). This allows transformations to be used 
  # out of the box, with no additional d3 coding. 
  theme.pars <- ggplot2:::plot_theme(p)  
  
  ## TODO: Allow setting these elements as NULL
  plist$axis <- list(
    x = plistextra$panel$ranges[[1]]$x.major,
    xlab = plistextra$panel$ranges[[1]]$x.labels,
    xrange = plistextra$panel$ranges[[1]]$x.range,
    xname = plistextra$plot$labels$x,
    y = plistextra$panel$ranges[[1]]$y.major,
    ylab = plistextra$panel$ranges[[1]]$y.labels,
    yrange = plistextra$panel$ranges[[1]]$y.range,
    yname = plistextra$plot$labels$y
  )
  # Flip labels if coords are flipped - transform does not take care of this.
  # I wonder if there is a better way to do this, though...?
  if("flip"%in%attr(plistextra$plot$coordinates, "class")){
    temp = plist$axis$xname
    plist$axis$xname = plist$axis$yname
    plist$axis$yname = temp
  }
  
  plist$legend <- getLegendList(plistextra)
  if(length(plist$legend)>0){
    plist$legend <- plist$legend[which(sapply(plist$legend, function(i) length(i)>0))]
  }  # only pass out legends that have guide = "legend" or guide="colorbar"
  
  plist$title <- plistextra$plot$labels$title
  plist$options <- list(width=400,height=400)
  plist
}

#' Convert a layer to a list. Called from gg2list()
#' @param l one layer of the ggplot object
#' @param d one layer of calculated data from ggplot2::ggplot_build(p)
#' @return list representing a layer, with corresponding aesthetics, ranges, and groups.
#' @export
#' @seealso \code{\link{gg2animint}}
layer2list <- function(l, d, ranges){
  g <- list(geom=l$geom$objname,
            data=d)
  g$aes <- sapply(l$mapping, as.character)
  
  ## use un-named parameters so that they will not be exported
  ## to JSON as a named object, since that causes problems with
  ## e.g. colour.
  g$params <- l$geom_params
  for(p.name in names(g$params)){
    names(g$params[[p.name]]) <- NULL
  }

  ## Make a list of variables to use for subsetting.
  some.vars <- c(g$aes[grepl("showSelected",names(g$aes))])
  g$update <- c(some.vars, g$aes[names(g$aes)=="clickSelects"])
  subset.vars <- c(some.vars, g$aes[names(g$aes)=="group"])
  g$subord <- as.list(names(subset.vars))
  g$subvars <- as.list(subset.vars)

  ## Pre-process some complex geoms so that they are treated as
  ## special cases of basic geoms. In ggplot2, this processing is done
  ## in the draw method of the geoms.
  if(g$geom=="abline"){
    # "Trick" ggplot coord_transform into transforming the slope and intercept
    g$data[,"x"] <- ranges$x.range[1]
    g$data[,"xend"] <- ranges$x.range[2]
    g$data[,"y"] <- g$data$slope*ranges$x.range[1]+g$data$intercept
    g$data[,"yend"] <-  g$data$slope*ranges$x.range[2]+g$data$intercept
    g$data <- as.data.frame(g$data)
    if(g$aes[["group"]]=="1"){ 
      # ggplot2 defaults to adding a group attribute
      # which misleads for situations where there are 
      # multiple lines with the same group. 
      # if the group attribute conveys no additional 
      # information, remove it.
      ## TODO: Figure out a better way to handle this...
      g$aes <- g$aes[-which(names(g$aes)=="group")]
      subset.vars <- c(some.vars, g$aes[names(g$aes)=="group"])
      g$subord <- as.list(names(subset.vars))
      g$subvars <- as.list(subset.vars)
    } 
    g$geom <- "segment"
  } else if(g$geom=="point"){
    # Fill set to match ggplot2 default of filled in circle. 
    if(!"fill"%in%names(g$data) & "colour"%in%names(g$data)){
      g$data[["fill"]] <- g$data[["colour"]]
    }
  } else if(g$geom=="density" | g$geom=="area"){
    g$geom <- "ribbon"
  } else if(g$geom=="tile" | g$geom=="raster"){
    # Color set to match ggplot2 default of tile with no outside border.
    if(!"colour"%in%names(g$data) & "fill"%in%names(g$data)){
      g$data[["colour"]] <- g$data[["fill"]]
      # Make outer border of 0 size if size isn't already specified.
      if(!"size"%in%names(g$data)) g$data[["size"]] <- 0 
    }
    g$geom <- "rect"
  } else if(g$geom=="histogram" | g$geom=="bar"){
    g$geom <- "rect"
  } else if(g$geom=="bin2d"){
    stop("bin2d is not supported in animint. Try using geom_tile() and binning the data yourself.")
  } else if(g$geom=="boxplot"){
    stop("boxplots are not supported in animint")
    g$data$outliers <- sapply(g$data$outliers, FUN=paste, collapse=" @ ") 
    # outliers are specified as a list... change so that they are specified 
    # as a single string which can then be parsed in JavaScript.
    # there has got to be a better way to do this!!
  } else if(g$geom=="violin"){
    g$data <- transform(g$data, xminv = x-violinwidth*(x-xmin),xmaxv = x+violinwidth*(xmax-x))
    newdata <- ddply(g$data, .(group), function(df){
                  rbind(arrange(transform(df, x=xminv), y), arrange(transform(df, x=xmaxv), -y))
                })
    newdata <- ddply(newdata, .(group), function(df) rbind(df, df[1,]))
    g$data <- newdata
    g$geom <- "polygon"
  } else if(g$geom=="step"){
    datanames <- names(g$data)
    g$data <- ddply(g$data, .(group), function(df) ggplot2:::stairstep(df))
    g$geom <- "path"
  } else if(g$geom=="contour" | g$geom=="density2d"){
    g$aes[["group"]] <- "piece"
    # reset g$subord, g$subvars now that group aesthetic exists.
    subset.vars <- c(some.vars, g$aes[names(g$aes)=="group"])
    g$subord <- as.list(names(subset.vars))
    g$subvars <- as.list(subset.vars)
    g$geom <- "path"
  } else if(g$geom=="freqpoly"){
    g$geom <- "line"
    # reset g$subord, g$subvars now that group aesthetic exists.
    subset.vars <- c(some.vars, group="group")
    g$subord <- as.list(names(subset.vars))
    g$subvars <- as.list(subset.vars)
  } else if(g$geom=="quantile"){
    g$geom <- "path"
    # reset g$subord, g$subvars now that group aesthetic exists.
    subset.vars <- c(some.vars, group="group")
    g$subord <- as.list(names(subset.vars))
    g$subvars <- as.list(subset.vars)
  } else { 
    ## all other geoms are basic, and keep the same name.
    g$geom
  }
  
  ## Check g$data for color/fill - convert to hexadecimal so JS can parse correctly.
  for(color.var in c("colour", "color", "fill")){
    if(color.var %in% names(g$data)){
      g$data[,color.var] <- toRGB(g$data[,color.var])
    }
  }
  
  g
}

#' Workhorse function for the animint package.
#' Convert a list of ggplots to a d3-ready graphic. 
#' Adds aesthetics clickSelects and updateSelected to utilize 
#' d3's mouseover and interactivity features for multiple linked plots,
#' and allows animated sequences of graphics. 
#' 
#' Supported ggplot2 geoms: 
#' \itemize{
#' \item point 
#' \item jitter
#' \item line
#' \item rect
#' \item tallRect (new with this package)
#' \item segment
#' \item hline
#' \item vline
#' \item bar
#' \item text
#' \item tile
#' \item raster
#' \item ribbon
#' \item abline
#' \item density
#' \item path
#' \item polygon
#' \item histogram
#' \item violin
#' \item linerange
#' \item step
#' \item contour
#' \item density2d
#' \item area
#' \item freqpoly
#' }
#' Unsupported geoms: 
#' \itemize{
#' \item rug
#' \item dotplot
#' \item hex
#' \item quantile - should *theoretically* work but in practice does not work
#' \item smooth - can be created using geom_line and geom_ribbon
#' \item boxplot - can be created using geom_rect and geom_segment
#' \item crossbar - can be created using geom_rect and geom_segment
#' \item pointrange - can be created using geom_linerange and geom_point
#' \item bin2d - bin using ddply() and then use geom_tile()
#' \item map - can be created using geom_polygon or geom_path
#'}
#' Supported scales: 
#' \itemize{
#' \item alpha, 
#' \item fill/colour (brewer, gradient, identity, manual)
#' \item linetype
#' \item x and y axis scales, manual break specification, label formatting
#' \item area 
#' \item size
#' }
#' Unsupported scales: 
#' \itemize{
#' \item shape. Open and closed circles can be represented by manipulating fill and colour scales and using default (circle) points, but d3 does not support many R shape types, so mapping between the two is difficult.
#' }
#' TODO: 
#' \itemize{
#' \item add legends
#' }
#' 
#' @title gg2animint
#' @param plot.list list of named ggplots with showSelected and clickSelects aesthetics. Input must be a list, so to use a single ggplot named g, it must be passed to the function as plot.list = list(g=g).
#' @param out.dir directory to store html/js/csv files 
#' @param open.browser Should R open a browser? Note: Chrome will not display local html files unless you are running a local webserver or have launched chrome with the option --allow-file-access-from-files. Firefox should display local html files (including those containing javascript).
#' @return invisible list of ggplots in list format
#' @export 
#' @seealso \code{\link{ggplot2}}
#' @example examples/breakpointExamples.R
gg2animint <- function(plot.list, out.dir=tempfile(), open.browser=interactive()){
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
      for(sel.i in seq_along(g$update)){
        v.name <- g$update[[sel.i]]
        col.name <- names(g$update)[[sel.i]]
        if(!v.name %in% names(result$selectors)){
          ## select the first one. TODO: customize.
          result$selectors[[v.name]] <- list(selected=g$data[[col.name]][1])
        }
        result$selectors[[v.name]]$subset <-
          c(result$selectors[[v.name]]$subset, list(g$classed))
      }
      ## Output data to csv.
      csv.name <- sprintf("%s.csv", g$classed)
      write.csv(g$data, file.path(out.dir, csv.name),
                quote=FALSE,row.names=FALSE)
      
      ## Output types
      ## Check to see if character type is d3's rgb type. 
      is.linetype <- function(x){
        x <- tolower(x)
        namedlinetype <- x%in%c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
        xsplit <- sapply(x, function(i) sum(is.na(strtoi(strsplit(i,"")[[1]],16)))==0)
        return(namedlinetype | xsplit)
      }
      g$types <- as.list(sapply(g$data, class))
      charidx <- which(g$types=="character")
      g$types[charidx] <- sapply(charidx, function(i) 
        if(sum(!is.rgb(g$data[[i]]))==0){"rgb"
        }else if(sum(!is.linetype(g$data[[i]]))==0){"linetype"
        }else "character")
      
      g$data <- csv.name
      ## Finally save to the master geom list.
      result$geoms[[g$classed]] <- g
      i <- i+1
    }
    result$plots[[plot.name]]$scales <- p$scales
    result$plots[[plot.name]]$options <- p$options
    result$plots[[plot.name]]$ranges <- p$ranges
    result$plots[[plot.name]]$axis <- p$axis
    result$plots[[plot.name]]$title <- p$title
    result$plots[[plot.name]]$legend <- p$legend
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
}


#' Check if character is an RGB hexadecimal color value
#' @param x character 
#' @return True/False value
#' @export 
is.rgb <- function(x){
  grepl("NULL", x) | (grepl("#", x) & nchar(x)==7)
}

#' Convert R colors to RGB hexadecimal color values
#' @param x character
#' @return hexadecimal color value
#' @export
toRGB <- function(x) rgb(t(col2rgb(as.character(x))), maxColorValue=255)

#' Function to get legend information from ggplot
#' @param plistextra output from ggplot2::ggplot_build(p)
#' @return list containing information for each legend
#' @export
getLegendList <- function(plistextra){
  aes.scales <- which(sapply(plistextra$plot$scales$scales, function(i) sum(i$aesthetics%in%c("colour", "size", "fill", "linetype", "alpha"))>0))
  lapply(aes.scales, getLegend, mb = plistextra)
}

#' Function to get legend information for each scale
#' @param mb output from ggplot2::ggplot_build(p)
#' @i index of scale containing legend-generating information. Position scales do not generate legends, and must be excluded from possible indices.
getLegend <- function(mb, i){
  sc <- mb$plot$scales$scales[[i]]
  guidetype <- sc$guide
  sc.aes <- sc$aesthetics
  bk <- ggplot2:::scale_breaks(sc)
  val <- ggplot2:::scale_map(sc, bk)
  labels <- ggplot2:::scale_labels(sc)
  if(sc.aes %in% c("colour", "fill")){
    val <- toRGB(val)
  }
  df <- data.frame(breaks = bk, value = val, label = labels)
  df <- df[which(rowSums(is.na(df))==0),] # return only those entries that have breaks, values, and labels.
  val <- val[which(rowSums(is.na(df))==0)]
  labels <- labels[which(rowSums(is.na(df))==0)]
  labels[is.na(labels)] <- "" # for NA labels, replace with empty string. (Useful for colorbar in particular)
  if(guidetype=="none"){
    NULL
  } else{
    list(guide = guidetype, 
         aesthetic = sc.aes, 
         title = as.character(as.expression(mb$plot$mapping[[sc.aes]])), 
         legend = list(values = val, labels = labels))
  }
}

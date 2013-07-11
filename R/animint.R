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
    # TODO: make use of other scales than manual.
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
    }
  }
  for(i in seq_along(plistextra$plot$layers)){
    ## This is the layer from the original ggplot object.
    L <- plistextra$plot$layers[[i]]

    ## for each layer, there is a correpsonding data.frame which
    ## evaluates the aesthetic mapping.
    df <- plistextra$data[[i]]

    ## This extracts essential info for this geom/layer.
    g <- layer2list(L, df)
    
    ## Idea: use the ggplot2:::coord_transform(coords, data, scales)
    ## function to handle cases like coord_flip. scales is a list of
    ## 12, coords is a list(limits=list(x=NULL,y=NULL)) with class
    ## e.g. c("cartesian","coord"). The result is a transformed data
    ## frame where all the data values are between 0 and 1. TODO:
    ## change the JS code to reflect this fact.
    g$data <- ggplot2:::coord_transform(plistextra$plot$coord, df,
                                        plistextra$panel$ranges[[1]])
    browser()
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
  plist$axis <- list(

#     if("element_blank"%in%attr(theme.pars$axis.text.x, "class")) # code to get blank elements... come back later?
    x = plistextra$panel$ranges[[1]]$x.major_source,
    xlab = plistextra$panel$ranges[[1]]$x.labels,
    xrange = plistextra$panel$ranges[[1]]$x.range,
    xname = plistextra$plot$labels$x,
    y = plistextra$panel$ranges[[1]]$y.major_source,
    ylab = plistextra$panel$ranges[[1]]$y.labels,
    yrange = plistextra$panel$ranges[[1]]$y.range,
    yname = plistextra$plot$labels$y
  )
  plist$title <- plistextra$plot$labels$title
  plist$options <- list(width=300,height=300)
  plist
}

#' Convert a layer to a list. Called from gg2list()
#' @param l one layer of the ggplot object
#' @param d one layer of calculated data from ggplot2::ggplot_build(p)
#' @return list representing a layer, with corresponding aesthetics, ranges, and groups.
#' @export
#' @seealso \code{\link{gg2animint}}
layer2list <- function(l, d){
  g <- list(geom=l$geom$objname,
            data=d)
  g$aes <- sapply(l$mapping, as.character)
  # use un-named parameters so that they will not be exported
  # to JSON as a named object, since that causes problems with
  # e.g. colour.
  g$params <- l$geom_params
  for(p.name in names(g$params)){
    names(g$params[[p.name]]) <- NULL
  }
  
  # Check g$data for color/fill - convert to hexadecimal.
  toRGB <- function(x) rgb(t(col2rgb(as.character(x))), maxColorValue=255)
  for(color.var in c("colour", "color", "fill")){
    if(color.var %in% names(g$data)){
      g$data[,color.var] <- toRGB(g$data[,color.var])
    }
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
  g$geom <- if(g$geom=="abline"){
    slope <- plistextra$data[[i]]$slope
    intercept <- plistextra$data[[i]]$intercept
    temp.x <- matrix(plistextra$panel$ranges[[1]]$x.range, ncol=2, nrow=length(slope), byrow=TRUE)
    temp.y <- slope*temp.x+intercept
    g$data <- unique(data.frame(x=temp.x[,1], 
                   xend=temp.x[,2], 
                   y=temp.y[,1], 
                   yend=temp.y[,2]), 
                   group=1:length(slope))

    ##g$aes <- g$aes[-which(names(g$aes)%in%c("intercept", "slope"))]

    ## WHY do we need to erase subvars/subord here?
    g$subvars <- list()
    g$subord <- list()
    "segment"
  } else if(g$geom=="density" | g$geom=="area"){
    "ribbon"
  } else if(g$geom=="tile" | g$geom=="raster" | g$geom=="bar"){
    ## WHY do we set colour here?
    if(!"colour"%in%names(g$aes) & "fill"%in%names(g$aes)){
      g$aes[["colour"]] <- g$aes[["fill"]]
    }
    "rect"
  } else if(g$geom=="boxplot"){
    stop("boxplots are not supported in animint")
    g$data$outliers <- sapply(g$data$outliers, FUN=paste, collapse=" @ ") 
    # outliers are specified as a list... change so that they are specified as a single string which can then be parsed in JavaScript.
    # there has got to be a better way to do this!!
  } else if(g$geom=="histogram"){
    "rect"
  } else if(g$geom=="violin"){
    g$data <- transform(g$data, xminv = x-violinwidth*(x-xmin),xmaxv = x+violinwidth*(xmax-x))
    newdata <- ddply(g$data, .(group), function(df) rbind(arrange(transform(df, x=xminv), y), arrange(transform(df, x=xmaxv), -y)))
    newdata <- ddply(newdata, .(group), function(df) rbind(df, df[1,]))
    g$data <- newdata
    "polygon"
  } else if(g$geom=="step"){
    datanames <- names(g$data)
    g$data <- ddply(g$data, .(group), function(df) ggplot2:::stairstep(df))
    "path"
  } else if(g$geom=="contour" | g$geom=="density2d"){
    g$aes$group <- "piece"
    # reset g$subord, g$subvars now that group aesthetic exists.
    subset.vars <- c(some.vars, g$aes[names(g$aes)=="group"])
    g$subord <- as.list(names(subset.vars))
    g$subvars <- as.list(subset.vars)
    "path"
  } else { ## all other geoms are basic, and keep the same name.
    g$geom
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
#' }
#' Currently unsupported (TODO): 
#' \itemize{
#' \item area
#' \item freqpoly
#' \item smooth
#' \item rug
#' \item quantile
#' \item boxplot
#' \item crossbar
#' \item pointrange
#' \item dotplot
#' \item contour
#' \item density2d
#' \item bin2d
#' \item hex
#' \item map
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

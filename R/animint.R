#' Convert a ggplot to a list.
#' @param meta environment with previously calculated plot data, and a new plot to parse, already stored in plot and plot.name.
#' @return nothing, info is stored in meta.
#' @export
#' @import ggplot2 plyr
parsePlot <- function(meta){
  ## adding data and mapping to each layer from base plot, if necessary
  for(layer.i in seq_along(meta$plot$layers)) {
    
    ## if data is not specified, get it from plot
    if(length(meta$plot$layers[[layer.i]]$data) == 0){
      meta$plot$layers[[layer.i]]$data <- meta$plot$data
    }
    
    ## if mapping is not specified, get it from plot
    if(is.null(meta$plot$layers[[layer.i]]$mapping)){
      meta$plot$layers[[layer.i]]$mapping <- meta$plot$mapping
    }
    
  }
  
  
  meta$built <- ggplot2::ggplot_build(meta$plot)
  plot.meta <- list()
  scaleFuns <-
    list(manual=function(sc)sc$palette(0),
         brewer=function(sc)sc$palette(length(sc$range$range)),
         hue=function(sc)sc$palette(length(sc$range$range)),
         linetype_d=function(sc)sc$palette(length(sc$range$range)),
         alpha_c=function(sc)sc$palette(sc$range$range),
         size_c=function(sc)sc$palette(sc$range$range),
         gradient=function(sc){
           ggplot2:::scale_map(sc, ggplot2:::scale_breaks(sc))
         })
  for(sc in meta$plot$scales$scales){
    if(!is.null(sc$range$range)){
      makeScale <- scaleFuns[[sc$scale_name]]
      plot.meta$scales[[sc$aesthetics]] <- makeScale(sc)
    }
  }
  
  ## Export axis specification as a combination of breaks and
  ## labels, on the relevant axis scale (i.e. so that it can
  ## be passed into d3 on the x axis scale instead of on the
  ## grid 0-1 scale). This allows transformations to be used
  ## out of the box, with no additional d3 coding.
  theme.pars <- ggplot2:::plot_theme(meta$plot)

  ## Interpret panel.margin as the number of lines between facets
  ## (ignoring whatever grid::unit such as cm that was specified).
  
  ## Now ggplot specifies panel.margin in 'pt' instead of 'lines'
  ## Maybe use a standard converting factor in the future?
  pt.to.lines <- function(margin.value){
    if(attributes(margin.value)$unit == "pt"){
      margin.value <- round(as.numeric(margin.value) * (0.25/5.5), digits = 2)
    }
    as.numeric(margin.value)
  }
  plot.meta$panel_margin_lines <- pt.to.lines(theme.pars$panel.margin)
  
  ## No legend if theme(legend.postion="none").
  plot.meta$legend <- if(theme.pars$legend.position != "none"){
    getLegendList(meta$built)
  }
  
  ## scan for legends in each layer.
  for(layer.i in seq_along(meta$plot$layers)){
    ##cat(sprintf("%4d / %4d layers\n", layer.i, length(meta$plot$layers)))
    ## This is the layer from the original ggplot object.
    L <- meta$plot$layers[[layer.i]]
    ## If any legends are specified, add showSelected aesthetic
    for(legend.i in seq_along(plot.meta$legend)) {
      one.legend <- plot.meta$legend[[legend.i]]
      ## the name of the selection variable used in this legend.
      s.name <- one.legend$selector
      is.variable.name <- is.character(s.name) && length(s.name) == 1
      layer.has.variable <- s.name %in% names(L$data)
      
      if(is.variable.name && layer.has.variable) {
        ## grabbing the variable from the data
        var <- L$data[, s.name]
        is.interactive.aes <-
          grepl("showSelected|clickSelects", names(L$mapping))
        is.legend.var <- L$mapping == s.name
        ## If s.name is used with another interactive aes, then do
        ## not add any showSelected aesthetic for it.
        var.is.interactive <- any(is.interactive.aes & is.legend.var)
        if(!var.is.interactive){
          ## only add showSelected aesthetic if the variable is
          ## used by the geom
          type.vec <- one.legend$legend_type
          if(any(type.vec %in% names(L$mapping))){
            type.str <- paste(type.vec, collapse="")
            a.name <- paste0("showSelectedlegend", type.str)
            L$mapping[[a.name]] <- as.symbol(s.name)
          }
        }
        ## if selector.types has not been specified, create it
        if(is.null(meta$selector.types)) {
          meta$selector.types <- list()
        }
        ## if selector.types is not specified for this variable, set
        ## it to multiple.
        if(is.null(meta$selector.types[[s.name]])) {
          meta$selector.types[[s.name]] <- "multiple"
          meta$selectors[[s.name]]$type <- "multiple"
        }
        ## if first is not specified, create it
        if(is.null(meta$first)) {
          meta$first <- list()
        }
        ## if first is not specified, add all to first
        if(is.null(meta$first[[s.name]])) {
          u.vals <- unique(var)
        }
        ## Tell this selector that it has a legend somewhere in the
        ## viz. (if the selector has no interactive legend and no
        ## clickSelects, then we show the widgets by default).
        meta$selectors[[s.name]]$legend <- TRUE
      }#length(s.name)
    }#legend.i
  }#layer.i

  ## need to call ggplot_build again because we've added to the plot.
  ## I'm sure that there is a way around this, but not immediately sure how. 
  ## There's sort of a Catch-22 here because to create the interactivity, 
  ## we need to specify the variable corresponding to each legend. 
  ## To do this, we need to have the legend. 
  ## And to have the legend, I think that we need to use ggplot_build
  meta$built <- ggplot2::ggplot_build(meta$plot)
  ## TODO: implement a compiler that does not call ggplot_build at
  ## all, and instead does all of the relevant computations in animint
  ## code.
  ## 'strips' are really titles for the different facet panels
  plot.meta$strips <- with(meta$built, getStrips(plot$facet, panel))
  ## the layout tells us how to subset and where to plot on the JS side
  plot.meta$layout <- with(meta$built, flag_axis(plot$facet, panel$layout))
  plot.meta$layout <- with(meta$built, train_layout(
    plot$facet, plot$coordinates, plot.meta$layout, panel$ranges))

  ## Export axis specification as a combination of breaks and
  ## labels, on the relevant axis scale (i.e. so that it can
  ## be passed into d3 on the x axis scale instead of on the
  ## grid 0-1 scale). This allows transformations to be used
  ## out of the box, with no additional d3 coding.
  theme.pars <- ggplot2:::plot_theme(meta$plot)
  
  ## extract panel background and borders from theme.pars
  get_bg <- function(pars) {
    # if pars is not an empty list - occurs when using element_blank()
    if(length(pars) > 0) {
      
      ## if elements are not specified, they inherit from theme.pars$rect
      for(i in 1:length(pars)) {
        if(is.null(pars[[i]])) pars[[i]] <- unname(theme.pars$rect[[i]])
      }
      
      # convert fill to RGB if necessary
      if(!(is.rgb(pars$fill))) pars$fill <- unname(toRGB(pars$fill))
      # convert color to RGB if necessary
      if(!(is.rgb(pars$colour))) pars$colour <- unname(toRGB(pars$colour))
      
      # remove names (JSON file was getting confused)
      pars <- lapply(pars, unname)
      
    }
    pars
  }
  # saving background info
  plot.meta$panel_background <- get_bg(theme.pars$panel.background)
  plot.meta$panel_border <- get_bg(theme.pars$panel.border)
  
  ### function to extract grid info
  get_grid <- function(pars, major = T) {
    # if pars is not an empty list - occurs when using element_blank()
    if(length(pars) > 0) {
      
      ## if elements are not specified, they inherit from 
      ##    theme.pars$panel.grid then from theme.pars$line
      for(i in names(pars)) {
        if(is.null(pars[[i]])) pars[[i]] <- 
          if(!is.null(theme.pars$panel.grid[[i]])) {
            theme.pars$panel.grid[[i]]
          } else {
            theme.pars$line[[i]]
          }
      }
      # convert colour to RGB if necessary
      if(!is.rgb(pars$colour)) pars$colour <- unname(toRGB(pars$colour))
      
      # remove names (JSON file was getting confused)
      pars <- lapply(pars, unname)
      
    }
    
    ## x and y locations
    if(major) {
      pars$loc$x <- as.list(meta$built$panel$ranges[[1]]$x.major)
      pars$loc$y <- as.list(meta$built$panel$ranges[[1]]$y.major)
    } else {
      pars$loc$x <- as.list(meta$built$panel$ranges[[1]]$x.minor)
      pars$loc$y <- as.list(meta$built$panel$ranges[[1]]$y.minor)
      ## remove minor lines when major lines are already drawn
      pars$loc$x <- pars$loc$x[
        !(pars$loc$x %in% plot.meta$grid_major$loc$x)
        ]
      pars$loc$y <- pars$loc$y[
        !(pars$loc$y %in% plot.meta$grid_major$loc$y)
        ]
    }
    
    pars
  }
  # extract major grid lines
  plot.meta$grid_major <- get_grid(theme.pars$panel.grid.major)
  # extract minor grid lines
  plot.meta$grid_minor <- get_grid(theme.pars$panel.grid.minor, major = F)
  
  ## Flip labels if coords are flipped - transform does not take care
  ## of this. Do this BEFORE checking if it is blank or not, so that
  ## individual axes can be hidden appropriately, e.g. #1.
  if("CoordFlip"%in%attr(meta$plot$coordinates, "class")){
    temp <- meta$plot$labels$x
    meta$plot$labels$x <- meta$plot$labels$y
    meta$plot$labels$y <- temp
  }
  is.blank <- function(el.name){
    x <- ggplot2::calc_element(el.name, meta$plot$theme)
    "element_blank"%in%attr(x,"class")
  }

  # Instead of an "axis" JSON object for each plot,
  # allow for "axis1", "axis2", etc. where
  # "axis1" corresponds to the 1st PANEL
  ranges <- meta$built$panel$ranges
  n.axis <- length(ranges)
  axes <- setNames(vector("list", n.axis),
                   paste0("axis", seq_len(n.axis)))
  plot.meta <- c(plot.meta, axes)

  # translate axis information
  for (xy in c("x", "y")) {
    s <- function(tmp) sprintf(tmp, xy)
    # one axis name per plot (ie, a xtitle/ytitle is shared across panels)
    plot.meta[[s("%stitle")]] <- if(is.blank(s("axis.title.%s"))){
      ""
    } else {
      scale.i <- which(meta$plot$scales$find(xy))
      lab.or.null <- if(length(scale.i) == 1){
        meta$plot$scales$scales[[scale.i]]$name
      }
      if(is.null(unlist(lab.or.null))){
        meta$plot$labels[[xy]]
      }else{
        lab.or.null
      }
    }
    # theme settings are shared across panels
    axis.text <- theme.pars[[s("axis.text.%s")]]
    ## TODO: also look at axis.text! (and text?)
    anchor <- hjust2anchor(axis.text$hjust)
    angle <- if(is.numeric(axis.text$angle)){
      -axis.text$angle
    }
    if(is.null(angle)){
      angle <- 0
    }
    if(is.null(anchor)){
      anchor <- if(angle == 0){
        "middle"
      }else{
        "end"
      }
    }
    plot.meta[[s("%sanchor")]] <- as.character(anchor)
    plot.meta[[s("%sangle")]] <- as.numeric(angle)
    # translate panel specific axis info
    ctr <- 0
    for (axis in names(axes)) {
      ctr <- ctr + 1
      range <- ranges[[ctr]]
      plot.meta[[axis]][[xy]] <- as.list(range[[s("%s.major")]])
      plot.meta[[axis]][[s("%slab")]] <- if(is.blank(s("axis.text.%s"))){
        NULL
      } else {
        as.list(range[[s("%s.labels")]])
      }
      plot.meta[[axis]][[s("%srange")]] <- range[[s("%s.range")]]
      plot.meta[[axis]][[s("%sline")]] <- !is.blank(s("axis.line.%s"))
      plot.meta[[axis]][[s("%sticks")]] <- !is.blank(s("axis.ticks.%s"))
    }
  }
  # grab the unique axis labels (makes rendering simpler)
  axis.info <- plot.meta[grepl("^axis[0-9]+$", names(plot.meta))]
  plot.meta$xlabs <- as.list(unique(unlist(lapply(axis.info, "[", "xlab"))))
  plot.meta$ylabs <- as.list(unique(unlist(lapply(axis.info, "[", "ylab"))))

  if("element_blank"%in%attr(theme.pars$plot.title, "class")){
    plot.meta$title <- ""
  } else {
    plot.meta$title <- meta$plot$labels$title
  }

  ## Set plot width and height from animint.* options if they are
  ## present.
  plot.meta$options <- list()
  theme <- meta$plot$theme
  for(wh in c("width", "height")){
    awh <- paste0("animint.", wh)
    plot.meta$options[[wh]] <- if(awh %in% names(theme)){
      theme[[awh]]
    }else{
      400
    }
  }

  update_axes <- "animint.update_axes"
  if(update_axes %in% names(theme)){
    plot.meta$options$update_axes <- theme[[update_axes]]
  }

  meta$plots[[meta$plot.name]] <- plot.meta

  list(
    ggplot=meta$plot,
    built=meta$built)
}

hjust2anchor <- function(hjust){
  if(is.null(hjust))return(NULL)
  stopifnot(is.numeric(hjust))
  trans <-
    c("0"="start",
      "0.5"="middle",
      "1"="end")
  hjust.str <- as.character(hjust)
  is.valid <- hjust.str %in% names(trans)
  if(all(is.valid)){
    ## as.character removes names.
    as.character(trans[hjust.str])
  }else{
    print(hjust[!is.valid])
    stop("animint only supports hjust values 0, 0.5, 1")
  }
}

#' Save a layer to disk, save and return meta-data.
#' @param l one layer of the ggplot object.
#' @param d one layer of calculated data from ggplot2::ggplot_build(p).
#' @param meta environment of meta-data.
#' @return list representing a layer, with corresponding aesthetics, ranges, and groups.
#' @export
saveLayer <- function(l, d, meta){
  # carson's approach to getting layer types
  ggtype <- function (x, y = "geom") {
    sub(y, "", tolower(class(x[[y]])[1]))
  }
  ranges <- meta$built$panel$ranges
  g <- list(geom=ggtype(l))
  g$classed <-
    sprintf("geom%d_%s_%s",
            meta$geom.count, g$geom, meta$plot.name)
  ## For each geom, save the nextgeom to preserve drawing order.
  if(is.character(meta$prev.class)){
    meta$geoms[[meta$prev.class]]$nextgeom <- g$classed
  }
  
  meta$geom.count <- meta$geom.count + 1
  ## needed for when group, etc. is an expression:
  g$aes <- sapply(l$mapping, function(k) as.character(as.expression(k)))

  ## use un-named parameters so that they will not be exported
  ## to JSON as a named object, since that causes problems with
  ## e.g. colour.
  ## 'colour', 'size' etc. have been moved to aes_params
  g$params <- c(l$geom_params, l$stat_params, l$aes_params, l$extra_params)
  for(p.name in names(g$params)){
    if("chunk_vars" %in% names(g$params) && is.null(g$params[["chunk_vars"]])){
      g$params[["chunk_vars"]] <- character()
    }
    names(g$params[[p.name]]) <- NULL
    ## Ignore functions.
    if(is.function(g$params[[p.name]])){
      g$params[[p.name]] <- NULL
    }
  }

  ## Make a list of variables to use for subsetting. subset_order is the
  ## order in which these variables will be accessed in the recursive
  ## JavaScript array structure.

  ## subset_order IS in fact useful with geom_segment! For example, in
  ## the first plot in the breakpointError example, the geom_segment has
  ## the following exported data in plot.json

  ## "subset_order": [
  ##  "showSelected",
  ## "showSelected2"
  ## ],

  ## This information is used to parse the recursive array data structure
  ## that allows efficient lookup of subsets of data in JavaScript. Look at
  ## the Firebug DOM browser on
  ## http://sugiyama-www.cs.titech.ac.jp/~toby/animint/breakpoints/index.html
  ## and navigate to plot.Geoms.geom3.data. You will see that this is a
  ## recursive array that can be accessed via
  ## data[segments][bases.per.probe] which is an un-named array
  ## e.g. [{row1},{row2},...] which will be bound to the <line> elements by
  ## D3. The key point is that the subset_order array stores the order of the
  ## indices that will be used to select the current subset of data (in
  ## this case showSelected=segments, showSelected2=bases.per.probe). The
  ## currently selected values of these variables are stored in
  ## plot.Selectors.

  s.aes <- selector.aes(g$aes)
  meta$selector.aes[[g$classed]] <- s.aes

  ## Do not copy group unless it is specified in aes, and do not copy
  ## showSelected variables which are specified multiple times.
  group.not.specified <- ! "group" %in% names(g$aes)
  n.groups <- length(unique(NULL))
  need.group <- c("violin", "step", "hex")
  group.meaningless <- g$geom %in% c(
    "abline", "blank",
    ##"crossbar", "pointrange", #documented as unsupported
    ## "rug", "dotplot", "quantile", "smooth", "boxplot",
    ## "bin2d", "map"
    "errorbar", "errorbarh",
    ##"bar", "histogram", #?
    "hline", "vline",
    "jitter", "linerange",
    "point", 
    "rect", "segment")
  dont.need.group <- ! g$geom %in% need.group
  remove.group <- group.meaningless ||
    group.not.specified && 1 < n.groups && dont.need.group
  do.not.copy <- c(
    if(remove.group)"group",
    s.aes$showSelected$ignored,
    s.aes$clickSelects$ignored)
  copy.cols <- ! names(d) %in% do.not.copy
  g.data <- d[copy.cols]
  
  is.ss <- names(g$aes) %in% s.aes$showSelected$one
  show.vars <- g$aes[is.ss]
  pre.subset.order <- as.list(names(show.vars))

  is.cs <- names(g$aes) %in% s.aes$clickSelects$one
  update.vars <- g$aes[is.ss | is.cs]

  update.var.names <- if(0 < length(update.vars)){
    data.frame(variable=names(update.vars), value=NA)
  }
  
  interactive.aes <- with(s.aes, {
    rbind(clickSelects$several, showSelected$several,
          update.var.names)
  })

  ## Construct the selector.
  for(row.i in seq_along(interactive.aes$variable)){
    aes.row <- interactive.aes[row.i, ]
    is.variable.value <- !is.na(aes.row$value)
    selector.df <- if(is.variable.value){
      selector.vec <- g.data[[paste(aes.row$variable)]]
      data.frame(value.col=aes.row$value,
                 selector.name=unique(paste(selector.vec)))
    }else{
      value.col <- paste(aes.row$variable)
      data.frame(value.col,
                 selector.name=update.vars[[value.col]])
    }
    for(sel.i in 1:nrow(selector.df)){
      sel.row <- selector.df[sel.i,]
      value.col <- paste(sel.row$value.col)
      selector.name <- paste(sel.row$selector.name)
      ## If this selector was defined by .variable .value aes, then we
      ## will not generate selectize widgets.
      meta$selectors[[selector.name]]$is.variable.value <- is.variable.value
      ## If this selector has no defined type yet, we define it once
      ## and for all here, so we can use it later for chunk
      ## separation.
      if(is.null(meta$selectors[[selector.name]]$type)){
        selector.type <- meta$selector.types[[selector.name]]
        if(is.null(selector.type))selector.type <- "single"
        stopifnot(is.character(selector.type))
        stopifnot(length(selector.type)==1)
        stopifnot(selector.type %in% c("single", "multiple"))
        meta$selectors[[selector.name]]$type <- selector.type
      }
      ## If this selector does not have any clickSelects then we show
      ## the selectize widgets by default.
      for(look.for in c("showSelected", "clickSelects")){
        if(grepl(look.for, aes.row$variable)){
          meta$selectors[[selector.name]][[look.for]] <- TRUE
        }
      }
      ## We also store all the values of this selector in this layer,
      ## so we can accurately set levels after all geoms have been
      ## compiled.
      value.vec <- unique(g.data[[value.col]])
      key <- paste(g$classed, row.i, sel.i)
      meta$selector.values[[selector.name]][[key]] <-
        list(values=paste(value.vec), update=g$classed)
    }
  }

  is.show <- grepl("showSelected", names(g$aes))
  has.show <- any(is.show)
  ## Error if non-identity stat is used with showSelected, since
  ## typically the stats will delete the showSelected column from the
  ## built data set. For example geom_bar + stat_bin doesn't make
  ## sense with clickSelects/showSelected, since two
  ## clickSelects/showSelected values may show up in the same bin.
  stat.type <- class(l$stat)[[1]]
  if(has.show && stat.type != "StatIdentity"){
    show.names <- names(g$aes)[is.show]
    data.has.show <- show.names %in% names(g.data)
    signal <- if(all(data.has.show))warning else stop
    print(l)
    signal(
      "showSelected does not work with ",
      stat.type,
      ", problem: ",
      g$classed)
  }
  ## Warn if non-identity position is used with animint aes.
  position.type <- class(l$position)[[1]]
  if(has.show && position.type != "PositionIdentity"){
    print(l)
    warning("showSelected only works with position=identity, problem: ",
            g$classed)
  }

  ##print("before pre-processing")

  ## Pre-process some complex geoms so that they are treated as
  ## special cases of basic geoms. In ggplot2, this processing is done
  ## in the draw method of the geoms.
  if(g$geom=="abline"){
    ## loop through each set of slopes/intercepts
    
    ## TODO: vectorize this code!
    for(i in 1:nrow(g.data)) {
      
      # "Trick" ggplot coord_transform into transforming the slope and intercept
      g.data[i, "x"] <- ranges[[ g.data$PANEL[i] ]]$x.range[1]
      g.data[i, "xend"] <- ranges[[ g.data$PANEL[i] ]]$x.range[2]
      g.data[i, "y"] <- g.data$slope[i] * g.data$x[i] + g.data$intercept[i]
      g.data[i, "yend"] <- g.data$slope[i] * g.data$xend[i] + g.data$intercept[i]
      
      # make sure that lines don't run off the graph
      if(g.data$y[i] < ranges[[ g.data$PANEL[i] ]]$y.range[1] ) {
        g.data$y[i] <- ranges[[ g.data$PANEL[i] ]]$y.range[1]
        g.data$x[i] <- (g.data$y[i] - g.data$intercept[i]) / g.data$slope[i]
      }
      if(g.data$yend[i] > ranges[[ g.data$PANEL[i] ]]$y.range[2]) {
        g.data$yend[i] <- ranges[[ g.data$PANEL[i] ]]$y.range[2]
        g.data$xend[i] <- (g.data$yend[i] - g.data$intercept[i]) / g.data$slope[i]
      }
    }
    ## ggplot2 defaults to adding a group aes for ablines!
    ## Remove it since it is meaningless.
    g$aes <- g$aes[names(g$aes)!="group"]
    g.data <- g.data[! names(g.data) %in% c("slope", "intercept")]
    g$geom <- "segment"
  } else if(g$geom=="point"){
    # Fill set to match ggplot2 default of filled in circle.
    # Check for fill in both data and params
    fill.in.data <- ("fill" %in% names(g.data) && any(!is.na(g.data[["fill"]])))
    fill.in.params <- "fill" %in% names(g$params)
    fill.specified <- fill.in.data || fill.in.params
    if(!fill.specified & "colour" %in% names(g.data)){
      g.data[["fill"]] <- g.data[["colour"]]
    }
  } else if(g$geom=="text"){
    ## convert hjust to anchor.
    hjustRemove <- function(df.or.list){
      df.or.list$anchor <- hjust2anchor(df.or.list$hjust)
      df.or.list[names(df.or.list) != "hjust"]
    }
    vjustWarning <- function(vjust.vec){
      not.supported <- vjust.vec != 0
      if(any(not.supported)){
        bad.vjust <- unique(vjust.vec[not.supported])
        print(bad.vjust)
        warning("animint only supports vjust=0")
      }
    }
    if ("hjust" %in% names(g$params)) {
      g$params <- hjustRemove(g$params)
    } else if ("hjust" %in% names(g.data)) {
      g.data <- hjustRemove(g.data)
    } 
    if("vjust" %in% names(g$params)) {
      vjustWarning(g$params$vjust)
    } else if ("vjust" %in% names(g$aes)) {
      vjustWarning(g.data$vjust)
    } 
  } else if(g$geom=="ribbon"){
    # Color set to match ggplot2 default of fill with no outside border.
    if("fill"%in%names(g.data) & !"colour"%in%names(g.data)){
      g.data[["colour"]] <- g.data[["fill"]]
    }
  } else if(g$geom=="density" | g$geom=="area"){
    g$geom <- "ribbon"
  } else if(g$geom=="tile" | g$geom=="raster" | g$geom=="histogram" ){
    # Color set to match ggplot2 default of tile with no outside border.
    if(!"colour"%in%names(g.data) & "fill"%in%names(g.data)){
      g.data[["colour"]] <- g.data[["fill"]]
      # Make outer border of 0 size if size isn't already specified.
      if(!"size"%in%names(g.data)) g.data[["size"]] <- 0
    }
    g$geom <- "rect"
  } else if(g$geom=="bar"){
    is.xy <- names(g.data) %in% c("x", "y")
    g.data <- g.data[!is.xy]
    g$geom <- "rect"
  } else if(g$geom=="bin2d"){
    stop("bin2d is not supported in animint. Try using geom_tile() and binning the data yourself.")
  } else if(g$geom=="boxplot"){
    stop("boxplots are not supported. Workaround: rects, lines, and points")
    ## TODO: boxplot support. But it is hard since boxplots are drawn
    ## using multiple geoms and it is not straightforward to deal with
    ## that using our current JS code. There is a straightforward
    ## workaround: combine working geoms (rects, lines, and points).

    g.data$outliers <- sapply(g.data$outliers, FUN=paste, collapse=" @ ")
    # outliers are specified as a list... change so that they are specified
    # as a single string which can then be parsed in JavaScript.
    # there has got to be a better way to do this!!
  } else if(g$geom=="violin"){
    g.data$xminv <- with(g.data, x - violinwidth * (x - xmin))
    g.data$xmaxv <- with(g.data, x + violinwidth * (xmax - x))
    newdata <- plyr::ddply(g.data, "group", function(df){
                  rbind(plyr::arrange(transform(df, x=xminv), y), 
                        plyr::arrange(transform(df, x=xmaxv), -y))
                })
    newdata <- plyr::ddply(newdata, "group", function(df) rbind(df, df[1,]))
    g.data <- newdata
    g$geom <- "polygon"
  } else if(g$geom=="step"){
    datanames <- names(g.data)
    g.data <- plyr::ddply(g.data, "group", function(df) ggplot2:::stairstep(df))
    g$geom <- "path"
  } else if(g$geom=="contour" | g$geom=="density2d"){
    g$aes[["group"]] <- "piece"
    g$geom <- "path"
  } else if(g$geom=="freqpoly"){
    g$geom <- "line"
  } else if(g$geom=="quantile"){
    g$geom <- "path"
  } else if(g$geom=="hex"){
    g$geom <- "polygon"
    ## TODO: for interactivity we will run into the same problems as
    ## we did with histograms. Again, if we put several
    ## clickSelects/showSelected values in the same hexbin, then
    ## clicking/hiding hexbins doesn't really make sense. Need to stop
    ## with an error if showSelected/clickSelects is used with hex.
    g$aes[["group"]] <- "group"
    dx <- ggplot2::resolution(g.data$x, FALSE)
    dy <- ggplot2::resolution(g.data$y, FALSE) / sqrt(3) / 2 * 1.15
    hex <- as.data.frame(hexbin::hexcoords(dx, dy))[,1:2]
    hex <- rbind(hex, hex[1,]) # to join hexagon back to first point
    g.data$group <- as.numeric(interaction(g.data$group, 1:nrow(g.data)))
    ## this has the potential to be a bad assumption -
    ##   by default, group is identically 1, if the user
    ##   specifies group, polygons aren't possible to plot
    ##   using d3, because group will have a different meaning
    ##   than "one single polygon".
    # CPS (07-24-14) what about this? --
    # http://tdhock.github.io/animint/geoms/polygon/index.html
    newdata <- plyr::ddply(g.data, "group", function(df){
      df$xcenter <- df$x
      df$ycenter <- df$y
      cbind(x=df$x+hex$x, y=df$y+hex$y, df[,-which(names(df)%in%c("x", "y"))])
    })
    g.data <- newdata
    # Color set to match ggplot2 default of tile with no outside border.
    if(!"colour"%in%names(g.data) & "fill"%in%names(g.data)){
      g.data[["colour"]] <- g.data[["fill"]]
      # Make outer border of 0 size if size isn't already specified.
      if(!"size"%in%names(g.data)) g.data[["size"]] <- 0
    }
  } else {
    ## all other geoms are basic, and keep the same name.
    g$geom
  }

  ## Some geoms need their data sorted before saving to tsv.
  if(g$geom %in% c("ribbon", "line")){
    g.data <- g.data[order(g.data$x), ]
  }

  ## Check g.data for color/fill - convert to hexadecimal so JS can parse correctly.
  for(color.var in c("colour", "color", "fill")){
    if(color.var %in% names(g.data)){
      g.data[,color.var] <- toRGB(g.data[,color.var])
    }
    if(color.var %in% names(g$params)){
      g$params[[color.var]] <- toRGB(g$params[[color.var]])
    }
  }

  has.no.fill <- g$geom %in% c("path", "line")
  zero.size <- any(g.data$size == 0, na.rm=TRUE)
  if(zero.size && has.no.fill){
    warning(sprintf("geom_%s with size=0 will be invisible",g$geom))
  }

  ## Idea: use the ggplot2:::coord_transform(coords, data, scales)
  ## function to handle cases like coord_flip. scales is a list of
  ## 12, coords is a list(limits=list(x=NULL,y=NULL)) with class
  ## e.g. c("cartesian","coord"). The result is a transformed data
  ## frame where all the data values are between 0 and 1.

  ## TODO: coord_transform maybe won't work for
  ## geom_dotplot|rect|segment and polar/log transformations, which
  ## could result in something nonlinear. For the time being it is
  ## best to just ignore this, but you can look at the source of
  ## e.g. geom-rect.r in ggplot2 to see how they deal with this by
  ## doing a piecewise linear interpolation of the shape.

  # Apply coord_transform seperately to each panel
  # Note the plotly implementation does not use
  # coord_transform...do they take care of the transformation
  # at a different point in time?

  # Flip axes in case of coord_flip
  # Switches column names. Eg. xmin to ymin, 
  # yntercept to xintercept etc.
  switch_axes <- function(col.names){
    for(elem in seq_along(col.names)){
      if(grepl("^x", col.names[elem])){
        col.names[elem] <- sub("^x", "y", col.names[elem])
      } else if(grepl("^y", col.names[elem])){
        col.names[elem] <- sub("^y", "x", col.names[elem])
      }
    }
    col.names
  }

  if(inherits(meta$plot$coordinates, "CoordFlip")){
    names(g.data) <- switch_axes(names(g.data))
  }

  # Rescale data to range 0:1 like the coord_trans function
  # in ggplot v1.0.1
  rescale_data <- function(g.data.i, ranges.i){
    for(col.name in names(g.data.i)){
      if(grepl("^x", col.name)){
        g.data.i[[col.name]] <- scales::rescale(g.data.i[[col.name]], 
                                                0:1, ranges.i$x.range)
      } else if(grepl("^y", col.name)){
        g.data.i[[col.name]] <- scales::rescale(g.data.i[[col.name]], 
                                                0:1, ranges.i$y.range)
      }
    }
    g.data.i
  }

  # g.data <- do.call("rbind", mapply(rescale_data, 
  #                                   split(g.data, g.data[["PANEL"]]), 
  #                                   ranges, SIMPLIFY = FALSE))

#   g.data <- do.call("rbind", mapply(function(x, y) {
#     ggplot2:::coord_trans(meta$plot$coord, x, y)
#   }, split(g.data, g.data[["PANEL"]]), ranges, SIMPLIFY = FALSE))

  ## Output types
  ## Check to see if character type is d3's rgb type.
  is.linetype <- function(x){
    x <- tolower(x)
    namedlinetype <-
      x%in%c("blank", "solid", "dashed",
             "dotted", "dotdash", "longdash", "twodash")
    xsplit <- sapply(x, function(i){
      sum(is.na(strtoi(strsplit(i,"")[[1]],16)))==0
    })
    namedlinetype | xsplit
  }
  g$types <- sapply(g.data, function(x) {
    type <- paste(class(x), collapse="-")
    if(type == "character"){
      if(sum(!is.rgb(x))==0){
        "rgb"
      }else if(sum(!is.linetype(x))==0){
        "linetype"
      }else {
        "character"
      }
    }else{
      type
    }
  })

  ## convert ordered factors to unordered factors so javascript
  ## doesn't flip out.
  ordfactidx <- which(g$types=="ordered-factor")
  for(i in ordfactidx){
    g.data[[i]] <- factor(as.character(g.data[[i]]))
    g$types[[i]] <- "factor"
  }

  ## Make the time variable the first subset_order variable.
  time.col <- if(is.null(meta$time)){ # if this is not an animation,
    NULL
  }else{
    click.or.show <- grepl("clickSelects|showSelected", names(g$aes))
    names(g$aes)[g$aes==meta$time$var & click.or.show]
  }
  if(length(time.col)){
    pre.subset.order <- pre.subset.order[order(pre.subset.order != time.col)]
  }
  ## Get unique values of time variable.
  if(length(time.col)){ # if this layer/geom is animated,
    meta$timeValues[[paste(g$classed)]] <- unique(g.data[[time.col]])
  }

  ## Determine which showSelected values to use for breaking the data
  ## into chunks. This is a list of variables which have the same
  ## names as the selectors. E.g. if chunk_order=list("year") then
  ## when year is clicked, we may need to download some new data for
  ## this geom.

  subset.vec <- unlist(pre.subset.order)
  if("chunk_vars" %in% names(g$params)){ #designer-specified chunk vars.
    designer.chunks <- g$params$chunk_vars
    if(!is.character(designer.chunks)){
      stop("chunk_vars must be a character vector; ",
           "use chunk_vars=character() to specify 1 chunk")
    }
    not.subset <- !designer.chunks %in% g$aes[subset.vec]
    if(any(not.subset)){
      stop("invalid chunk_vars ",
           paste(designer.chunks[not.subset], collapse=" "),
           "; possible showSelected variables: ",
           paste(g$aes[subset.vec], collapse=" "))
    }
    is.chunk <- g$aes[subset.vec] %in% designer.chunks
    chunk.cols <- subset.vec[is.chunk]
    nest.cols <- subset.vec[!is.chunk]
  }else{ #infer a default, either 0 or 1 chunk vars:
    if(length(meta$selectors)==0){
      ## no selectors, just make 1 chunk.
      nest.cols <- subset.vec
      chunk.cols <- NULL
    }else{
      selector.types <- sapply(meta$selectors, "[[", "type")
      selector.names <- g$aes[subset.vec]
      subset.types <- selector.types[selector.names]
      can.chunk <- subset.types != "multiple"
      names(can.chunk) <- subset.vec
      ## Guess how big the chunk files will be, and reduce the number of
      ## chunks if there are any that are too small.
      tmp <- tempfile()
      some.lines <- rbind(head(g.data), tail(g.data))
      write.table(some.lines, tmp,
                  col.names=FALSE,
                  quote=FALSE, row.names=FALSE, sep="\t")
      bytes <- file.info(tmp)$size
      bytes.per.line <- bytes/nrow(some.lines)
      bad.chunk <- function(){
        if(all(!can.chunk))return(NULL)
        can.chunk.cols <- subset.vec[can.chunk]
        maybe.factors <- g.data[, can.chunk.cols, drop=FALSE]
        for(N in names(maybe.factors)){
          maybe.factors[[N]] <- paste(maybe.factors[[N]])
        }
        rows.per.chunk <- table(maybe.factors)
        bytes.per.chunk <- rows.per.chunk * bytes.per.line
        if(all(4096 < bytes.per.chunk))return(NULL)
        ## If all of the tsv chunk files are greater than 4KB, then we
        ## return NULL here to indicate that the current chunk
        ## variables (indicated in can.chunk) are fine.

        ## In other words, the compiler will not break a geom into
        ## chunks if any of the resulting chunk tsv files is estimated
        ## to be less than 4KB (of course, if the layer has very few
        ## data overall, the compiler creates 1 file which may be less
        ## than 4KB, but that is fine).
        dim.byte.list <- list()
        if(length(can.chunk.cols) == 1){
          dim.byte.list[[can.chunk.cols]] <- sum(bytes.per.chunk)
        }else{
          for(dim.i in seq_along(can.chunk.cols)){
            dim.name <- can.chunk.cols[[dim.i]]
            dim.byte.list[[dim.name]] <-
              apply(bytes.per.chunk, -dim.i, sum)
          }
        }
        selector.df <-
          data.frame(chunks.for=length(rows.per.chunk),
                     chunks.without=sapply(dim.byte.list, length),
                     min.bytes=sapply(dim.byte.list, min))
        ## chunks.for is the number of chunks you get if you split the
        ## data set using just this column. If it is 1, then it is
        ## fine to chunk on this variable (since we certainly won't
        ## make more than 1 small tsv file) and in fact we want to
        ## chunk on this variable, since then this layer's data won't
        ## be downloaded at first if it is not needed.
        not.one <- subset(selector.df, 1 < chunks.for)
        if(nrow(not.one) == 0){
          NULL
        }else{
          rownames(not.one)[[which.max(not.one$min.bytes)]]
        }
      }
      while({
        bad <- bad.chunk()
        !is.null(bad)
      }){
        can.chunk[[bad]] <- FALSE
      }
      if(any(can.chunk)){
        nest.cols <- subset.vec[!can.chunk]
        chunk.cols <- subset.vec[can.chunk]
      }else{
        nest.cols <- subset.vec
        chunk.cols <- NULL
      }
    } # meta$selectors > 0
  }
  
  # If there is only one PANEL, we don't need it anymore.
  plot.has.panels <- nrow(meta$built$panel$layout) > 1
  g$PANEL <- unique(g.data[["PANEL"]])
  geom.has.one.panel <- length(g$PANEL) == 1
  if(geom.has.one.panel && (!plot.has.panels)) {
    g.data <- g.data[names(g.data) != "PANEL"]
  }
  
  ## Also add pointers to these chunks from the related selectors.
  if(length(chunk.cols)){
    selector.names <- as.character(g$aes[chunk.cols])
    chunk.name <- paste(selector.names, collapse="_")
    g$chunk_order <- as.list(selector.names)
    for(selector.name in selector.names){
      meta$selectors[[selector.name]]$chunks <-
        unique(c(meta$selectors[[selector.name]]$chunks, chunk.name))
    }
  }else{
    g$chunk_order <- list()
  }
  g$nest_order <- as.list(nest.cols)
  names(g$chunk_order) <- NULL
  names(g$nest_order) <- NULL
  g$subset_order <- g$nest_order
  
  ## If this plot has more than one PANEL then add it to subset_order
  ## and nest_order.
  if(plot.has.panels){
    g$subset_order <- c(g$subset_order, "PANEL")
    g$nest_order <- c(g$nest_order, "PANEL")
  }

  ## nest_order should contain both .variable .value aesthetics, but
  ## subset_order should contain only .variable.
  if(0 < nrow(s.aes$showSelected$several)){
    g$nest_order <- with(s.aes$showSelected$several, {
      c(g$nest_order, paste(variable), paste(value))
    })
    g$subset_order <-
      c(g$subset_order, paste(s.aes$showSelected$several$variable))
  }
    
  ## group should be the last thing in nest_order, if it is present.
  data.object.geoms <- c("line", "path", "ribbon", "polygon")
  if("group" %in% names(g$aes) && g$geom %in% data.object.geoms){
    g$nest_order <- c(g$nest_order, "group")
  }

  ## Determine if there are any "common" data that can be saved
  ## separately to reduce disk usage.
  data.or.null <- getCommonChunk(g.data, chunk.cols, g$aes)
  g.data.varied <- if(is.null(data.or.null)){
    split.x(g.data, chunk.cols)
  }else{
    g$columns$common <- as.list(names(data.or.null$common))
    tsv.name <- sprintf("%s_chunk_common.tsv", g$classed)
    tsv.path <- file.path(meta$out.dir, tsv.name)
    write.table(data.or.null$common, tsv.path,
                quote = FALSE, row.names = FALSE, 
                sep = "\t")
    data.or.null$varied
  }

  ## Save each variable chunk to a separate tsv file.
  meta$chunk.i <- 1L
  meta$g <- g
  g$chunks <- saveChunks(g.data.varied, meta)
  g$total <- length(unlist(g$chunks))

  ## Finally save to the master geom list.
  meta$geoms[[g$classed]] <- g

  g
}

##' Save the common columns for each tsv to one chunk
##' @param built data.frame of built data.
##' @param vars character vector of chunk variable names to split on.
##' @param aes.list a character vector of aesthetics.
##' @return a list of common and varied data to save, or NULL if there is
##' no common data.
getCommonChunk <- function(built, chunk.vars, aes.list){
  if(length(chunk.vars) == 0){
    return(NULL)
  }
  if(! "group" %in% names(aes.list)){
    ## user did not specify group, so do not use any ggplot2-computed
    ## group for deciding common data.
    built$group <- NULL
  }
  
  ## Remove columns with all NA values
  ## so that common.not.na is not empty
  ## due to the plot's alpha, stroke or other columns
  all.nas <- sapply(built, function(x){all(is.na(x))})
  built <- built[, !all.nas]
  
  ## Treat factors as characters, to avoid having them be coerced to
  ## integer later.
  for(col.name in names(built)){
    if(is.factor(built[, col.name])){
      built[, col.name] <- paste(built[, col.name])
    }
  }
  built.by.chunk <- split(built, built[, chunk.vars], drop = TRUE)
  if(length(built.by.chunk) == 1) return(NULL)
  ## If there is no group column, and all the chunks are the same
  ## size, then add one based on the row number.
  if(! "group" %in% names(built)){
    chunk.rows.vec <- sapply(built.by.chunk, nrow)
    chunk.rows <- chunk.rows.vec[1]
    same.size <- chunk.rows == chunk.rows.vec
    if(all(same.size)){
      for(chunk.name in names(built.by.chunk)){
        built.by.chunk[[chunk.name]]$group <- 1:chunk.rows
      }
    }else{
      ## do not save a common chunk file.
      return(NULL)
    }
  }
  all.col.names <- names(built.by.chunk[[1]])
  col.name.vec <- all.col.names[!all.col.names %in% chunk.vars]
  values.by.group <- list()
  for(chunk.name in names(built.by.chunk)){
    chunk.df <- built.by.chunk[[chunk.name]]
    chunk.by.group <- split(chunk.df, chunk.df$group, drop=TRUE)
    for(group.name in names(chunk.by.group)){
      values.by.group[[group.name]][[chunk.name]] <-
        chunk.by.group[[group.name]]
    }
  }
  is.common.mat <-
    matrix(NA, length(values.by.group), length(col.name.vec),
           dimnames=list(group=names(values.by.group),
                         col.name=col.name.vec))
  group.info.list <- list()
  for(group.name in names(values.by.group)){
    values.by.chunk <- values.by.group[[group.name]]
    row.count.vec <- sapply(values.by.chunk, nrow)
    same.size.chunks <- all(row.count.vec[1] == row.count.vec)
    ## For every group, save values for creating common tsv later.
    one.group.info <- values.by.chunk[[1]]
    for(col.name in col.name.vec){
      value.list <- lapply(values.by.chunk, function(df)df[[col.name]])
      is.common.mat[group.name, col.name] <- if(same.size.chunks){
        value.mat <- do.call(cbind, value.list)
        least.missing <- which.min(colSums(is.na(value.mat)))
        value.vec <- value.mat[, least.missing]
        one.group.info[[col.name]] <- value.vec
        all(value.vec == value.mat)
      }else{
        value.vec <- unlist(value.list)
        one.group.info[[col.name]] <- value.vec[[1]]
        value.tab <- table(value.vec)
        length(value.tab) == 1
      }
    }
    group.info.list[[group.name]] <- one.group.info
  }
  is.common <- apply(is.common.mat, 2, all, na.rm=TRUE)
  ## TODO: another criterion could be used to save disk space even if
  ## there is only 1 chunk.
  if(is.common[["group"]] && sum(is.common) >= 2){
    common.cols <- names(is.common)[is.common]
    group.info <- do.call(rbind, group.info.list)
    group.info.common <- group.info[, names(which(is.common))]
    common.unique <- unique(group.info.common)
    ## For geom_polygon and geom_path we may have two rows that should
    ## both be kept (the start and the end of each group may be the
    ## same if the shape is closed), so we define common.data as all
    ## of the rows (common.not.na) in that case, and just the unique
    ## data per group (common.unique) in the other case.
    data.per.group <- table(common.unique$group)
    common.data <- if(all(data.per.group == 1)){
      common.unique
    }else{
      group.info.common
    }
    built.group <- do.call(rbind, built.by.chunk)
    varied.df.list <- split.x(na.omit(built.group), chunk.vars)
    varied.cols <- c("group", names(is.common)[!is.common])
    varied.data <- varied.chunk(varied.df.list, varied.cols)
    return(list(common=na.omit(common.data),
                varied=varied.data))
  }
}

##' Extract subset for each data.frame in a list of data.frame
##' @param df.or.list a data.frame or a list of data.frame.
##' @param cols cols that each data.frame would keep.
##' @return list of data.frame.
varied.chunk <- function(df.or.list, cols){
  if(is.data.frame(df.or.list)){
    df <- df.or.list[, cols, drop = FALSE]
    u.df <- unique(df)
    group.counts <- table(u.df$group)
    if(all(group.counts == 1)){
      u.df
    }else{
      df
    }
  } else{
    lapply(df.or.list, varied.chunk, cols)
  }
}

##' Split data.frame into recursive list of data.frame.
##' @param x data.frame.
##' @param vars character vector of variable names to split on.
##' @return recursive list of data.frame.
split.x <- function(x, vars){
  if(length(vars)==0)return(x)
  if(is.data.frame(x)){
    
    ## Remove columns with all NA values
    ## so that x is not empty due to
    ## the plot's alpha, stroke or other columns
    all.nas <- sapply(x, function(col.m){all(is.na(col.m))})
    x <- x[, !all.nas]
    
    # rows with NA should not be saved
    x <- na.omit(x)
    if(length(vars) == 1){
      split(x[names(x) != vars], x[vars], drop = TRUE)
    }else{
      use <- vars[1]
      rest <- vars[-1]
      df.list <- split(x[names(x) != use], x[use], drop = TRUE)
      split.x(df.list, rest)
    }
  }else if(is.list(x)){
    lapply(x, split.x, vars)
  }else{
    str(x)
    stop("unknown object")
  }
}
  
##' Split data set into chunks and save them to separate files.
##' @param x data.frame.
##' @param meta environment.
##' @return recursive list of chunk file names.
##' @author Toby Dylan Hocking
saveChunks <- function(x, meta){
  if(is.data.frame(x)){
    this.i <- meta$chunk.i
    csv.name <- sprintf("%s_chunk%d.tsv", meta$g$classed, this.i)
    write.table(x, file.path(meta$out.dir, csv.name), quote=FALSE, 
                row.names=FALSE, sep="\t")
    meta$chunk.i <- meta$chunk.i + 1L
    this.i
  }else if(is.list(x)){
    lapply(x, saveChunks, meta)
  }else{
    str(x)
    stop("unknown object")
  }
}

##' Parse selectors from aes names.
##' @title Parse selectors from aes names.
##' @param a.vec character vector of aes names.
##' @return list of selector info.
##' @author Toby Dylan Hocking
##' @export
selector.aes <- function(a.list){
  a.vec <- names(a.list)
  if(is.null(a.vec))a.vec <- character()
  stopifnot(is.character(a.vec))
  cs.or.ss <- grepl("clickSelects|showSelected", a.vec)
  for(v in c("value", "variable")){
    regex <- paste0("[.]", v, "$")
    is.v <- grepl(regex, a.vec)
    if(any(is.v)){
      a <- a.vec[is.v & cs.or.ss]
      other.v <- if(v=="value")"variable" else "value"
      other.a <- sub(paste0(v, "$"), other.v, a)
      not.found <- ! other.a %in% a.vec
      if(any(not.found)){
        stop(".variable or .value aes not found")
      }
    }
  }
  aes.list <- list()
  for(a in c("clickSelects", "showSelected")){
    is.a <- grepl(a, a.vec)
    is.value <- grepl("[.]value$", a.vec)
    is.variable <- grepl("[.]variable$", a.vec)
    var.or.val <- is.variable | is.value
    a.value <- a.vec[is.a & is.value]
    a.variable <- sub("value$", "variable", a.value)
    single <- a.vec[is.a & (!var.or.val)]
    ignored <- c()
    if(1 < length(single)){
      single.df <- data.frame(
        aes.name=single,
        data.var=paste(a.list[single]))
      single.sorted <- single.df[order(single.df$data.var), ]
      single.sorted$keep <- c(TRUE, diff(as.integer(single.df$data.var))!=0)
      single <- with(single.sorted, paste(aes.name[keep]))
      ignored <- with(single.sorted, paste(aes.name[!keep]))
    }
    aes.list[[a]] <-
      list(several=data.frame(variable=a.variable, value=a.value),
           one=single, ignored=ignored)
  }
  aes.list
}

##' Deprecated alias for animint2dir.
##' @title animint2dir
##' @param ... passed to animint2dir
##' @return same as animint2dir
##' @author Toby Dylan Hocking
##' @export
gg2animint <- function(...){
  warning("gg2animint is deprecated, use animint2dir instead")
  animint2dir(...)
}

#' Compile and render an animint in a local directory
#'
#' An animint is a list of ggplots and options that defines
#' an interactive animation and can be viewed in a web browser.
#' Several new aesthetics control interactivity.
#' The most important two are
#' \itemize{
#' \item \code{aes(showSelected=variable)} means that
#'   only the subset of the data that corresponds to
#'   the selected value of variable will be shown.
#' \item \code{aes(clickSelects=variable)} means that clicking
#'   this geom will change the currently selected value of variable.
#' }
#' The others are described on https://github.com/tdhock/animint/wiki/Advanced-features-present-animint-but-not-in-ggplot2
#'
#' Supported ggplot2 geoms:
#' \itemize{
#' \item point
#' \item jitter
#' \item line
#' \item rect
#' \item tallrect (new with this package)
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
#' \item hex
#' }
#' Unsupported geoms:
#' \itemize{
#' \item rug
#' \item dotplot
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
#' \item x and y axis theme elements: axis.line, axis.ticks, axis.text, axis.title can be set to element_blank(); other theme modifications not supported at this time, but would be possible with custom css files.
#' \item area
#' \item size
#' }
#' Unsupported scales:
#' \itemize{
#' \item shape. Open and closed circles can be represented by manipulating fill and colour scales and using default (circle) points, but d3 does not support many R shape types, so mapping between the two is difficult.
#' }
#'
#' @aliases animint
#' @param plot.list a named list of ggplots and option lists.
#' @param out.dir directory to store html/js/csv files.
#' @param json.file character string that names the JSON file with metadata associated with the plot.
#' @param open.browser Should R open a browser? If yes, be sure to configure your browser to allow access to local files, as some browsers block this by default (e.g. chrome).
#' @param css.file character string for non-empty css file to include. Provided file will be copied to the output directory as styles.css
#' @return invisible list of ggplots in list format.
#' @export
#' @seealso \code{\link{ggplot2}}
#' @example inst/examples/animint.R
animint2dir <- function(plot.list, out.dir = tempfile(),
                        json.file = "plot.json", open.browser = interactive(),
                        css.file = "") {
  ## Check that plot.list is a list and every element is named.
  if (!is.list(plot.list))
    stop("plot.list must be a list of ggplots")
  if (is.null(names(plot.list)))
    stop("plot.list must be a named list")
  if (any(names(plot.list)==""))
    stop("plot.list must have names with non-empty strings")

  ## Store meta-data in this environment, so we can alter state in the
  ## lower-level functions.
  meta <- new.env()
  meta$plots <- list()
  meta$geoms <- list()
  meta$selectors <- list()
  meta$selector.types <- plot.list$selector.types
  dir.create(out.dir,showWarnings=FALSE)
  meta$out.dir <- out.dir
  meta$geom.count <- 1

  ## Save the animation variable so we can treat it specially when we
  ## process each geom.
  # CPS (7-22-14): What if the user doesn't specify milliseconds? Could we provide a reasonable default?
  if(is.list(plot.list[["time"]])){
    if(!all(c("ms", "variable") %in% names(plot.list$time))){
      stop("time option list needs ms, variable")
    }
    meta$time <- plot.list$time
    ms <- meta$time$ms
    stopifnot(is.numeric(ms))
    stopifnot(length(ms)==1)
    ## NOTE: although we do not use olist$ms for anything in the R
    ## code, it is used to control the number of milliseconds between
    ## animation frames in the JS code.
    time.var <- meta$time$variable
    stopifnot(is.character(time.var))
    stopifnot(length(time.var)==1)
  }

  ## The title option should just be a character, not a list.
  if(is.list(plot.list$title)){
    plot.list$title <- plot.list$title[[1]]
  }
  if(is.character(plot.list$title)){
    meta$title <- plot.list$title[[1]]
    plot.list$title <- NULL
  }

  ## Extract essential info from ggplots, reality checks.
  ggplot.list <- list()
  for(list.name in names(plot.list)){
    p <- plot.list[[list.name]]
    if(is.ggplot(p)){
      pattern <- "^[a-zA-Z][a-zA-Z0-9]*$"
      if(!grepl(pattern, list.name)){
        stop("ggplot names must match ", pattern)
      }
      ## Before calling ggplot_build, we do some error checking for
      ## some animint extensions.
      for(L in p$layers){
        ## This code assumes that the layer has the complete aesthetic
        ## mapping and data. TODO: Do we need to copy any global
        ## values to this layer?
        name.counts <- table(names(L$mapping))
        is.dup <- 1 < name.counts
        if(any(is.dup)){
          print(L)
          stop("aes names must be unique, problems: ",
               paste(names(name.counts)[is.dup], collapse=", "))
        }
        iaes <- selector.aes(L$mapping)
        one.names <- with(iaes, c(clickSelects$one, showSelected$one))
        update.vars <- as.character(L$mapping[one.names])
        # if the layer has a defined data set
        if(length(L$data) > 0) {
          # check whether the variable is in that layer
          has.var <- update.vars %in% names(L$data)
        } else {
          # check whether the variable is in the global data
          has.var <- update.vars %in% names(p$data)
        }
        
        if(!all(has.var)){
          print(L)
          print(list(problem.aes=update.vars[!has.var],
                     data.variables=names(L$data)))
          stop("data does not have interactive variables")
        }
        has.cs <- 0 < with(iaes$clickSelects, nrow(several) + length(one))
        has.href <- "href" %in% names(L$mapping)
        if(has.cs && has.href){
          stop("aes(clickSelects) can not be used with aes(href)")
        }
      }
      meta$plot <- p
      meta$plot.name <- list.name
      ggplot.list[[list.name]] <- parsePlot(meta) # calls ggplot_build.
    }else if(is.list(p)){ ## for options.
      meta[[list.name]] <- p
    }else{
      stop("list items must be ggplots or option lists, problem: ", list.name)
    }
  }

  ## After going through all of the meta-data in all of the ggplots,
  ## now we have enough info to save the TSV file database.
  for(p.name in names(ggplot.list)){
    ggplot.info <- ggplot.list[[p.name]]
    meta$prev.class <- NULL # first geom of any plot should not be next.
    for(layer.i in seq_along(ggplot.info$ggplot$layers)){
      L <- ggplot.info$ggplot$layers[[layer.i]]
      df <- ggplot.info$built$data[[layer.i]]
      ## cat(sprintf(
      ##   "saving layer %4d / %4d of ggplot %s\n",
      ##   layer.i, length(ggplot.info$built$data),
      ##   p.name))
      
      ## This is a total hack, we should clean up the internals
      ## (parsePlot, saveLayer) so that they no longer rely on this
      ## meta object which makes it super confusing to know which
      ## functions need which data.
      meta$plot.name <- p.name
      meta$plot <- ggplot.info$ggplot
      meta$built <- ggplot.info$built
      
      ## Data now contains columns with fill, alpha, colour etc.
      ## Remove from data if they have a single unique value and
      ## are NOT used in mapping to reduce tsv file size
      redundant.cols <- names(L$geom$default_aes)
      for(col.name in names(df)){
        if(col.name %in% redundant.cols){
          all.vals <- unique(df[[col.name]])
          if(length(all.vals) == 1){
            in.mapping <-
              !is.null(L$mapping[[col.name]])
            if(!in.mapping){
              df[[col.name]] <- NULL
            }
          }
        }
      }
      
      g <- saveLayer(L, df, meta)

      ## Every plot has a list of geom names.
      meta$plots[[p.name]]$geoms <- c(
        meta$plots[[p.name]]$geoms, list(g$classed))
    }#layer.i
  }
  
  ## Selector levels and update were stored in saveLayer, so now
  ## compute the unique values to store in meta$selectors.
  for(selector.name in names(meta$selector.values)){
    values.update <- meta$selector.values[[selector.name]]
    value.vec <- unique(unlist(lapply(values.update, "[[", "values")))
    meta$selectors[[selector.name]]$selected <- if(
      meta$selectors[[selector.name]]$type=="single"){
      value.vec[1]
    }else{
      value.vec
    }
    ## Check the selectize option to determine if the designer wants
    ## to show a widget for this selector.
    selectize <- meta$selectize[[selector.name]]
    render.widget <- if(is.logical(selectize)){
      selectize[1]
    }else{
      ## If the designer did not set selectize, then we set a default
      ## (if .variable .value aes, then no selectize; otherwise if
      ## there are less than 1000 values then yes).
      if(isTRUE(meta$selectors[[selector.name]]$is.variable.value)){
        FALSE
      }else{
        if(length(value.vec) < 1000){
          TRUE
        }else{
          FALSE
        }
      }
    }
    if(render.widget){
      ## Showing selectize widgets is optional, and indicated to the
      ## renderer by the compiler by not setting the "levels"
      ## attribute of the selector.
      meta$selectors[[selector.name]]$levels <- value.vec
    }
    ## s.info$update is the list of geom names that will be updated
    ## for this selector.
    meta$selectors[[selector.name]]$update <-
      as.list(unique(unlist(lapply(values.update, "[[", "update"))))
  }

  ## Now that selectors are all defined, go back through geoms to
  ## check if there are any warnings to issue.
  for(g.name in names(meta$geoms)){
    g.info <- meta$geoms[[g.name]]
    g.selectors <- meta$selector.aes[[g.name]]
    show.vars <- g.info$aes[g.selectors$showSelected$one]
    duration.vars <- names(meta$duration)
    show.with.duration <- show.vars[show.vars %in% duration.vars]
    no.key <- ! "key" %in% names(g.info$aes)
    if(length(show.with.duration) && no.key){
      warning(
        "to ensure that smooth transitions are interpretable, ",
        "aes(key) should be specifed for geoms with aes(showSelected=",
        show.with.duration[1],
        "), problem: ", g.name)
    }
  }
  
  ## For a static data viz with no interactive aes, no need to check
  ## for trivial showSelected variables with only 1 level.
  if(0 < length(meta$selectors)){
    n.levels <- sapply(meta$selectors, function(s.info)length(s.info$levels))
    one.level <- n.levels == 1
    has.legend <- sapply(meta$selectors, function(s.info)isTRUE(s.info$legend))
    is.trivial <- one.level & (!has.legend)
    if(any(is.trivial)){
      ## With the current compiler that has already saved the tsv files
      ## by now, we can't really make this data viz more efficient by
      ## ignoring this trivial selector. However we can warn the user so
      ## that they can remove this inefficient showSelected.
      warning("showSelected variables with only 1 level: ",
              paste(names(meta$selectors)[is.trivial], collapse=", "))
    }
  }

  ## Go through options and add to the list.
  for(v.name in names(meta$duration)){
    meta$selectors[[v.name]]$duration <- meta$duration[[v.name]]
  }
  ## Set plot sizes.
  for(d in c("width","height")){
    size <- meta[[d]]
    if(is.list(size)){
      warning("option ", d, " is deprecated, ",
              "use ggplot()+theme_animint(", d,
              "=", size[[1]],
              ") instead")
      if(is.null(names(size))){ #use this size for all plots.
        for(plot.name in names(meta$plots)){
          meta$plots[[plot.name]]$options[[d]] <- size[[1]]
        }
      }else{ #use the size specified for the named plot.
        for(plot.name in names(size)){
          if(plot.name %in% names(meta$plots)){
            meta$plots[[plot.name]]$options[[d]] <- size[[plot.name]]
          }else{
            stop("no ggplot named ", plot.name)
          }
        }
      }
    }
  }

  ## These geoms need to be updated when the time.var is animated, so
  ## let's make a list of all possible values to cycle through, from
  ## all the values used in those geoms.
  if("time" %in% ls(meta)){
    meta$selectors[[meta$time$variable]]$type <- "single"
    anim.values <- meta$timeValues
    if(length(meta$timeValues)==0){
      stop("no interactive aes for time variable ", meta$time$variable)
    }
    anim.not.null <- anim.values[!sapply(anim.values, is.null)]
    time.classes <- sapply(anim.not.null, function(x) class(x)[1])
    time.class <- time.classes[[1]]
    if(any(time.class != time.classes)){
      print(time.classes)
      stop("time variables must all have the same class")
    }
    meta$time$sequence <- if(time.class=="POSIXct"){
      orderTime <- function(format){
        values <- unlist(sapply(anim.not.null, strftime, format))
        sort(unique(as.character(values)))
      }
      hms <- orderTime("%H:%M:%S")
      f <- if(length(hms) == 1){
        "%Y-%m-%d"
      }else{
        "%Y-%m-%d %H:%M:%S"
      }
      orderTime(f)
    }else if(time.class=="factor"){
      levs <- levels(anim.not.null[[1]])
      if(any(sapply(anim.not.null, function(f)levels(f)!=levs))){
        print(sapply(anim.not.null, levels))
        stop("all time factors must have same levels")
      }
      levs
    }else{ #character, numeric, integer, ... what else?
      as.character(sort(unique(unlist(anim.not.null))))
    }
    meta$selectors[[time.var]]$selected <- meta$time$sequence[[1]]
  }

  ## The first selection:
  for(selector.name in names(meta$first)){
    first <- as.character(meta$first[[selector.name]])
    if(selector.name %in% names(meta$selectors)){
      s.type <- meta$selectors[[selector.name]]$type
      if(s.type == "single"){
        stopifnot(length(first) == 1)
      }
      meta$selectors[[selector.name]]$selected <- first
    }else{
      print(list(selectors=names(meta$selectors),
                 missing.first=selector.name))
      stop("missing first selector variable")
    }
  }
  
  ## Compute domains of different subsets, to be used by update_scales
  ## in the renderer
  compute_domains <- function(built_data, axes, geom_name,
                             var="showSelected"){
    # Different geoms will use diff columns to calculate domains for
    # showSelected subsets. Eg. geom_bar will use 'xmin', 'xmax', 'ymin',
    # 'ymax' etc. while geom_point will use 'x', 'y'
    domain_cols <- list(bar=c(paste0(axes, "min"), paste0(axes, "max")),
                       point = c(axes),
                       path = c(axes))
    use_cols <- domain_cols[[geom_name]]
    domain_vals <- list()
    if(length(use_cols) == 1){
      domain_vals[[use_cols[1]]] <-if(use_cols[1] %in% names(built_data)){
        lapply(split(built_data[[use_cols[1]]],
                     interaction(built_data$PANEL, built_data[[var]])),
               range, na.rm=TRUE)
      }else{
        NULL
      }
    }else{
      # Calculate min and max values of each subset separately
      min_vals <- lapply(split(built_data[[use_cols[1]]],
                               interaction(built_data$PANEL,
                                           built_data[[var]])),
                         min, na.rm=TRUE)
      max_vals <- lapply(split(built_data[[use_cols[2]]],
                               interaction(built_data$PANEL,
                                           built_data[[var]])),
                         max, na.rm=TRUE)
      domain_vals <- list(mapply(c, min_vals, max_vals, SIMPLIFY = FALSE))
    }
    domain_vals
  }
  
  ## Out of all the possible geoms, get the min/max value which will
  ## determine the domain to be used in the renderer
  get_domain <- function(subset_domains){
    use_domain <- list()
    ## ggplot gives a margin of 5% at all four sides which does not
    ## have any plotted data. So axis ranges are 10% bigger than the
    ## actual ranges of data. We do the same here
    extra_margin = 0.05
    for(i in names(subset_domains[[1]])){
      all_vals <- lapply(subset_domains, "[[", i)
      min_val <- min(sapply(all_vals, "[[", 1))
      max_val <- max(sapply(all_vals, "[[", 2))
      use_domain[[i]] <- c(min_val - (extra_margin *(max_val-min_val)),
                          max_val + (extra_margin *(max_val-min_val)))
    }
    use_domain
  }
  
  ## Get domains of data subsets if theme_animint(update_axes) is used
  for(p.name in names(ggplot.list)){
    axis_to_update <- meta$plots[[p.name]]$options$update_axes
    if(!is.null(axis_to_update)){
      p_geoms <- meta$plots[[p.name]]$geoms
      subset_domains <- list()
      for(num in seq_along(p_geoms)){
        aesthetic_names <- names(meta$geoms[[ p_geoms[[num]] ]]$aes)
        choose_ss <- grepl("^showSelected", aesthetic_names)
        selector <- meta$geoms[[ p_geoms[[num]] ]]$aes[choose_ss]
        # Do not calculate domains for multiple selectors
        # What if there are more than one single selectors in a plot???
        if(meta$selectors[[selector]]$type == "single"){
          subset_domains[num] <- compute_domains(
            ggplot.list[[p.name]]$built$data[[num]],
            axis_to_update, strsplit(p_geoms[[num]], "_")[[1]][[2]],
            names(selector))
        }
      }
      subset_domains <- subset_domains[!sapply(subset_domains, is.null)]
      use_domain <- get_domain(subset_domains)
      # Save for renderer
      meta$plots[[p.name]]$axis_domains <- use_domain
    }
  }
  
  ## Finally, copy html/js/json files to out.dir.
  src.dir <- system.file("htmljs",package="animint")
  to.copy <- Sys.glob(file.path(src.dir, "*"))
  if(file.exists(paste0(out.dir, "styles.css")) | css.file != "default.file"){
    to.copy <- to.copy[!grepl("styles.css", to.copy, fixed=TRUE)]
  }
  if(css.file!=""){
    # if css filename is provided, copy that file to the out directory as "styles.css"
    to.copy <- to.copy[!grepl("styles.css", to.copy, fixed=TRUE)]
    if(!file.exists(css.file)){
      stop(paste("css.file", css.file, "does not exist. Please check that the file name and path are specified correctly."))
    } else {
      file.copy(css.file, file.path(out.dir, "styles.css"), overwrite=TRUE)
    }
  } else {
    style.file <- system.file("htmljs", "styles.css", package = "animint")
    file.copy(style.file, file.path(out.dir, "styles.css"), overwrite=TRUE)
  }
  file.copy(to.copy, out.dir, overwrite=TRUE, recursive=TRUE)
  export.names <-
    c("geoms", "time", "duration", "selectors", "plots", "title")
  export.data <- list()
  for(export.name in export.names){
    if(export.name %in% ls(meta)){
      export.data[[export.name]] <- meta[[export.name]]
    }
  }
  json <- RJSONIO::toJSON(export.data)
  cat(json, file = file.path(out.dir, json.file))
  if (open.browser) {
    message('opening a web browser with a file:// URL; ',
            'if the web page is blank, try running
if (!requireNamespace("servr")) install.packages("servr")
servr::httd("', normalizePath( out.dir,winslash="/" ), '")')
      browseURL(sprintf("%s/index.html", out.dir))
  }
  invisible(meta)
  ### An invisible copy of the R list that was exported to JSON.
}


#' Check if character is an RGB hexadecimal color value
#' @param x character
#' @return True/False value
#' @export
is.rgb <- function(x){
  if(is.null(x)) {
    TRUE
  } else {
    (grepl("#", x) & nchar(x)==7)
  }
}

#' Convert R colors to RGB hexadecimal color values
#' @param x character
#' @return hexadecimal color value or "transparent" if is.na
#' @export
toRGB <- function(x){
  is.transparent <- is.na(x) | x=="transparent"
  rgb.mat <- col2rgb(x)
  rgb.vec <- rgb(t(rgb.mat), maxColorValue=255)
  named.vec <- ifelse(is.transparent, "transparent", rgb.vec)
  not.named <- as.character(named.vec)
  not.named
}

#' Function to get legend information from ggplot
#' @param plistextra output from ggplot2::ggplot_build(p)
#' @return list containing information for each legend
#' @export
getLegendList <- function(plistextra){
  plot <- plistextra$plot
  scales <- plot$scales
  layers <- plot$layers
  default_mapping <- plot$mapping
  theme <- ggplot2:::plot_theme(plot)
  position <- theme$legend.position
  # by default, guide boxes are vertically aligned
  if(is.null(theme$legend.box)) theme$legend.box <- "vertical" else theme$legend.box

  # size of key (also used for bar in colorbar guide)
  if(is.null(theme$legend.key.width)) theme$legend.key.width <- theme$legend.key.size
  if(is.null(theme$legend.key.height)) theme$legend.key.height <- theme$legend.key.size
  # by default, direction of each guide depends on the position of the guide.
  if(is.null(theme$legend.direction)){
    theme$legend.direction <- 
      if (length(position) == 1 && position %in% c("top", "bottom", "left", "right"))
        switch(position[1], top =, bottom = "horizontal", left =, right = "vertical")
    else
      "vertical"
  }
  # justification of legend boxes
  theme$legend.box.just <-
    if(is.null(theme$legend.box.just)) {
      if (length(position) == 1 && position %in% c("top", "bottom", "left", "right"))
        switch(position, bottom =, top = c("center", "top"), left =, right = c("left", "top"))
      else
        c("center", "center")
    }

  position <- theme$legend.position
  # locate guide argument in scale_*, and use that for a default.
  # Note, however, that guides(colour = ...) has precendence! See https://gist.github.com/cpsievert/ece28830a6c992b29ab6
  guides.args <- list()
  for(aes.name in c("colour", "fill")){
    aes.loc <- which(scales$find(aes.name))
    guide.type <- if (length(aes.loc) == 1){
      scales$scales[[aes.loc]][["guide"]]
    }else{
      "legend"
    }
    if(guide.type=="colourbar")guide.type <- "legend"
    guides.args[[aes.name]] <- guide.type
  }
  guides.result <- do.call(ggplot2::guides, guides.args)
  guides.list <- plyr::defaults(plot$guides, guides.result)
  gdefs <-
    ggplot2:::guides_train(scales = scales,
                           theme = theme,
                           guides = guides.list,
                           labels = plot$labels)
  if (length(gdefs) != 0) {
    gdefs <- ggplot2:::guides_merge(gdefs)
    gdefs <- ggplot2:::guides_geom(gdefs, layers, default_mapping)
  } else (ggplot2:::zeroGrob())
  names(gdefs) <- sapply(gdefs, function(i) i$title)
  
  ## adding the variable used to each LegendList
  for(leg in seq_along(gdefs)) {
    legend_type <- names(gdefs[[leg]]$key)
    legend_type <- legend_type[legend_type != ".label"]
    gdefs[[leg]]$legend_type <- legend_type
    scale.list <- scales$scales[which(scales$find(legend_type))]
    discrete.vec <- sapply(scale.list, inherits, "ScaleDiscrete")
    is.discrete <- all(discrete.vec)
    gdefs[[leg]]$is.discrete <- is.discrete
    ## get the name of the legend/selection variable.
    var.list <- list()
    for(layer.i in seq_along(plot$layers)) {
      L <- plot$layers[[layer.i]]
      var.list[[layer.i]] <- L$mapping[legend_type]
    }
    unique.var.list <- unique(unlist(var.list))
    if(is.discrete){
      var.name <- unique.var.list[[1]]
      if(length(unique.var.list) == 1 && is.symbol(var.name)){
        gdefs[[leg]]$selector <- paste(var.name)
      }else{
        str(unique.var.list)
        stop("need exactly 1 variable name ",
             "(not constant, not language/expression) ",
             "to create interactive discrete legend for aes ",
             paste(legend_type, collapse=", "))
      }
    }
    
    ## do not draw geoms which are constant:
    geom.list <- gdefs[[leg]]$geoms
    geom.data.list <- lapply(geom.list, "[[", "data")
    geom.data.rows <- sapply(geom.data.list, nrow)
    geom.unique.list <- lapply(geom.data.list, unique)
    geom.unique.rows <- sapply(geom.unique.list, nrow)
    is.ignored <- 1 < geom.data.rows & geom.unique.rows == 1
    gdefs[[leg]]$geoms <- geom.list[!is.ignored]
    
    ## Pass a geom.legend.list to be used by the
    ## GetLegend function
    geom.legend.list <- list()
    for(geom.i in seq_along(gdefs[[leg]]$geoms)){
      data.geom.i <- gdefs[[leg]]$geoms[[geom.i]]$data
      params.geom.i <- gdefs[[leg]]$geoms[[geom.i]]$params
      size.geom.i <- gdefs[[leg]]$geoms[[geom.i]]$size
      
      suppressWarnings(draw.key.used <- 
                         gdefs[[leg]]$geoms[[geom.i]]$draw_key(
                           data.geom.i, params.geom.i, size.geom.i)
      )
      geom.legend <- class(draw.key.used)[[1]]
      geom.legend.list <- c(geom.legend.list, geom.legend)
    }
    
    ## Process names to be used by the CleanData function
    convert.names.list <- list(points="point", segments="path", rect="polygon")
    names.to.change <- geom.legend.list %in% names(convert.names.list)
    geom.legend.list[names.to.change] <- 
      convert.names.list[unlist(geom.legend.list[names.to.change])]
    
    gdefs[[leg]]$geom.legend.list <- geom.legend.list
  }
  
  ## Add a flag to specify whether or not breaks was manually
  ## specified. If it was, then it should be respected. If not, and
  ## the legend shows a numeric variable, then it should be reversed.
  for(legend.name in names(gdefs)){
    key.df <- gdefs[[legend.name]]$key
    aes.name <- names(key.df)[1]
    scale.i <- which(scales$find(aes.name))
    if(length(scale.i) == 1){
      sc <- scales$scales[[scale.i]]
      gdefs[[legend.name]]$breaks <- sc$breaks
    }
  }

  legend.list <- lapply(gdefs, getLegend)
  ## Add a flag to specify whether or not there is both a color and a
  ## fill legend to display. If so, we need to draw the interior of
  ## the points in the color legend as the same color.
  if(1 < length(legend.list)){
    is.color <- sapply(legend.list, function(L)"colour" %in% L$legend_type)
    is.fill <- sapply(legend.list, function(L)"fill" %in% L$legend_type)
    is.point <- sapply(legend.list, function(L)"point" %in% L$geoms)
    has.both <- 2 == sum(is.point & (is.color | is.fill))
    if(has.both){
      for(legend.i in which(is.color)){
        entry.list <- legend.list[[legend.i]]$entries
        for(entry.i in seq_along(entry.list)){
          entry <- entry.list[[entry.i]]
          color.names <- grep("colour", names(entry), value=TRUE)
          fill.names <- sub("colour", "fill", color.names)
          entry[fill.names] <- "#FFFFFF"
          legend.list[[legend.i]]$entries[[entry.i]] <- entry
        }
      }
    }
  }
  legend.list[0 < sapply(legend.list, length)]
}

#' Function to get legend information for each scale
#' @param mb single entry from ggplot2:::guides_merge() list of legend data
#' @return list of legend information, NULL if guide=FALSE.
getLegend <- function(mb){
  guidetype <- mb$name

  ## The main idea of legends:
  
  ## 1. Here in getLegend I export the legend entries as a list of
  ## rows that can be used in a data() bind in D3.

  ## 2. In add_legend in the JS code I create a <table> for every
  ## legend, and then I bind the legend entries to <tr>, <td>, and
  ## <svg> elements.
  cleanData <- function(data, key, geom, params) {
    nd <- nrow(data)
    nk <- nrow(key)
    if (nd == 0) return(data.frame()); # if no rows, return an empty df.
    if ("guide" %in% names(params)) {
      if (params[["guide"]] == "none") return(data.frame()); # if no guide, return an empty df
    }
    if (nd != nk) warning("key and data have different number of rows")
    if (!".label" %in% names(key)) return(data.frame()); # if there are no labels, return an empty df.
    data$`.label` <- key$`.label`
    data <- data[, which(colSums(!is.na(data)) > 0)] # remove cols that are entirely na
    if("colour" %in% names(data)) data[["colour"]] <- toRGB(data[["colour"]]) # color hex values
    if("fill" %in% names(data)) data[["fill"]] <- toRGB(data[["fill"]]) # fill hex values
    names(data) <- paste0(geom, names(data))# aesthetics by geom
    names(data) <- gsub(paste0(geom, "."), "", names(data), fixed=TRUE) # label isn't geom-specific
    data$label <- paste(data$label) # otherwise it is AsIs.
    data
  }
  dataframes <- mapply(function(i, j) cleanData(i$data, mb$key, j, i$params),
                       mb$geoms, mb$geom.legend.list, SIMPLIFY = FALSE)
  dataframes <- dataframes[which(sapply(dataframes, nrow)>0)]
  # Check to make sure datframes is non-empty. If it is empty, return NULL.
  if(length(dataframes)>0) {
    data <- merge_recurse(dataframes)
  } else return(NULL)
  label.num <- suppressWarnings({
    as.numeric(data$label)
  })
  ## mb$breaks could be a vector of values to use, NULL, or an empty
  ## list with class "waiver"
  breaks.specified <- length(mb$breaks)
  entry.order <- if(breaks.specified || anyNA(label.num)){
    1:nrow(data)
  }else{
    nrow(data):1
  }
  data <- lapply(entry.order, function(i) as.list(data[i,]))
  if(guidetype=="none"){
    NULL
  }else{
    list(guide = guidetype,
         geoms = unlist(mb$geom.legend.list),
         title = mb$title,
         class = if(mb$is.discrete)mb$selector else mb$title,
         selector = mb$selector,
         is_discrete= mb$is.discrete,
         legend_type = mb$legend_type, 
         entries = data)
  }
}

#' Merge a list of data frames.
#' @param dfs list of data frames
#' @return data frame
merge_recurse <- function(dfs){
  label.vec <- unique(unlist(lapply(dfs, function(df)paste(df$label))))
  result <- data.frame(row.names=label.vec)
  for(df in dfs){
    df.label <- paste(df$label)
    for(col.name in names(df)){
      result[df.label, col.name] <- df[[col.name]]
    }
  }
  result
}

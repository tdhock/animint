#' ggplot2 geom with xmin and xmax aesthetics that covers the entire y range, useful for clickSelects background elements.
#' @param mapping aesthetic mapping
#' @param data data set
#' @param stat statistic mapping, defaults to identity
#' @param position position mapping, defaults to identity
#' @param ... other arguments
#' @return ggplot2 layer
#' @export
#' @import proto
#' @example inst/examples/breakpoints.R
GeomTallRect <- ggplot2::ggproto("GeomTallRect", ggplot2::Geom,
                                 default_aes = aes(colour = "grey35",
                                                   fill = "grey35", 
                                                   size = 0.5, 
                                                   linetype = 1,
                                                   alpha = 0.5),
                                 
                                 required_aes = c("xmin", "xmax"),
                                 
                                 draw_panel = function(self, data, 
                                                       panel_scales, coord) {
                                   coords <- coord$transform(data, panel_scales)
                                   ymax <- grid::unit(1, "npc")
                                   ymin <- grid::unit(0, "npc")
                                   grid::rectGrob(
                                     coords$xmin, ymax,
                                     width = coords$xmax - coords$xmin,
                                     height = ymax - ymin,
                                     default.units = "native",
                                     just = c("left", "top"),
                                     gp = grid::gpar(
                                       col = coords$colour,
                                       fill = scales::alpha(coords$fill, 
                                                            coords$alpha), 
                                       lwd = coords$size * .pt,
                                       lty = coords$linetype,
                                       lineend = "butt"
                                     )
                                   )
                                 },
                                 
                                 draw_key = draw_key_rect
)

geom_tallrect <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity",
                          ...,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  ggplot2::layer(
    geom = GeomTallRect,
    data = data,
    mapping = mapping,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}


#' ggplot2 geom with ymin and ymax aesthetics that covers the entire x range, useful for clickSelects background elements.
#' @param mapping aesthetic mapping
#' @param data data set
#' @param stat statistic mapping, defaults to identity
#' @param position position mapping, defaults to identity
#' @param ... other arguments
#' @return ggplot2 layer
#' @export
#' @examples
#'  \dontrun{ 
#'    source(system.file("examples/WorldBank.R", package = "animint"))
#'  }
GeomWideRect <- ggplot2::ggproto("GeomWideRect", ggplot2::Geom,
                                 default_aes = aes(colour = "grey35", 
                                                   fill = "grey35", 
                                                   size = 0.5, 
                                                   linetype = 1,
                                                   alpha = 0.5),
                                 
                                 required_aes = c("ymin", "ymax"),
                                 
                                 draw_panel = function(self, data, 
                                                       panel_scales, coord) {
                                   coords <- coord$transform(data, panel_scales)
                                   xmax <- grid::unit(1, "npc")
                                   xmin <- grid::unit(0, "npc")
                                   grid::rectGrob(
                                     xmin, coords$ymin,
                                     width = xmax - xmin,
                                     height = coords$ymax - coords$ymin,
                                     default.units = "native",
                                     just = c("left", "bottom"),
                                     gp = grid::gpar(
                                       col = coords$colour,
                                       fill = scales::alpha(coords$fill, 
                                                            coords$alpha), 
                                       lwd = coords$size * .pt,
                                       lty = coords$linetype,
                                       lineend = "butt"
                                     )
                                   )
                                 },
                                 
                                 draw_key = draw_key_rect
)

geom_widerect <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity",
                          ...,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  ggplot2::layer(
    geom = GeomWideRect,
    data = data,
    mapping = mapping,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}


#' Make a clickSelects geom_tallrect that completely tiles the x
#' range. This makes it easy to construct tallrects for the common
#' case of selecting a particular x value.
#' @param data data.frame to analyze for unique x.name values.
#' @param x.name variable to be used for x, clickSelects.
#' @param even Logical parameter, should tallrects be of even width?
#' @param alpha transparency of a selected tallrect, default 1/2.
#' @param ... passed to geom_tallrect.
#' @return a geom_tallrect layer.
#' @author Toby Dylan Hocking
#' @export
make_tallrect <- function(data, x.name, even=FALSE, alpha=1/2, ...){
  data <- as.data.frame(data)
  stopifnot(is.character(x.name))
  stopifnot(length(x.name)==1)
  x <- data[,x.name]
  stopifnot(is.numeric(x))
  vals <- sort(unique(x))
  Delta <- if(even) rep(ggplot2::resolution(vals), length(vals)-1)/2 else diff(vals)/2
  breaks <- c(vals[1] - Delta[1],
              vals[-1] - Delta,
              vals[length(vals)]+Delta[length(Delta)])
  
  stopifnot(length(breaks) == length(vals)+1)
  df <- data.frame(vals,
                   xmin=breaks[-length(breaks)],
                   xmax=breaks[-1])
  names(df)[1] <- x.name
  a <- aes_string(xmin="xmin", xmax="xmax", clickSelects=x.name)
  geom_tallrect(a, df, alpha=alpha, ...)
}

#' Convenience function for an interactive bar that might otherwise be
#' created using stat_summary(geom="bar").
#' @param data data.frame to analyze for unique x.name values.
#' @param x.name variable to be used for x, clickSelects.
#' @param alpha transparency of selected bar, default 1.
#' @return a geom_bar layer.
#' @author Toby Dylan Hocking
#' @export
make_bar <- function(data, x.name, alpha=1){
  data <- as.data.frame(data)
  stopifnot(is.character(x.name))
  stopifnot(length(x.name)==1)
  x <- data[,x.name]
  stopifnot(is.numeric(x))
  stat_summary(aes_string(x=x.name, y=x.name, clickSelects=x.name),
               data=data, alpha=alpha, fun.y=length, geom="bar")
}

#' Convenvience function for a showSelected plot label.
#' @param data data.frame of relevant data
#' @param x x coordinate of label position
#' @param y y coordinate of label position
#' @param label.var variable matching showSelected, used to obtain label value
#' @param format String format for label. Use \%d, \%f, etc. to insert relevant label.var value.
#' @return a geom_text layer.
#' @author Toby Dylan Hocking
#' @export
make_text <- function(data, x, y, label.var, format=NULL){
  data <- as.data.frame(data)
  stopifnot(length(x)==1)
  stopifnot(length(y)==1)
  ## TODO: position based on the data?
  ## if(is.character(x) && x %in% names(data)){
  ##   x <- data[,x]
  ##   x <- (min(x)+max(x))/2
  ## }
  ## if(is.character(y) && y %in% names(data)){
  ##   y <- max(data[,y])
  ## }
  data <- unique(data[,label.var,drop=FALSE])
  data$label <- data[,label.var]
  data$x <- x
  data$y <- y
  if(is.null(format)){
    data$label <- as.character(data$label)
    format <- paste(label.var,"= %s")
  }
  if(is.character(format)){
    fstring <- format
    format <- function(val){
      sprintf(fstring, val)
    }
  }
  stopifnot(is.function(format))
  data$label <- format(data$label)
  a <- aes_string(x="x",y="y",label="label",showSelected=label.var)
  geom_text(a, data)
}

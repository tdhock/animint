#' ggplot2 geom with xmin and xmax aesthetics that covers the entire y range, useful for clickSelects background elements.
#' @param mapping aesthetic mapping
#' @param data data mapping
#' @param stat statistic mapping, defaults to identity
#' @param position position mapping, defaults to identity
#' @param ... other arguments
#' @return ggplot2 layer
#' @export
#' @seealso \code{\link{gg2animint}}
#' @examples ## Example: 4 plots, 2 selectors.
#' data(intreg)
#' signal.colors <- c(estimate="#0adb0a", latent="#0098ef")
#' breakpoint.colors <- c("1breakpoint"="#ff7d7d", "0breakpoints"='#f6f4bf')
#' model.linetypes <- c(margin="dotted",limit="dashed",regression="solid")
#' intreg$annotations$logratio <- max(intreg$sig$log)
#' ## To get the bottom 3 plots to line up properly, we need to plot some
#' ## geom_blanks bigger than the x range, so we calculate that here.
#' blank.items <- with(intreg,{
#'   list(segments=list(data=selection,x="min.L",y="segments"),
#'        error=list(data=selection,x="max.L",y="cost"),
#'        regression=list(data=model,x=c("min.L","max.L"),
#'                        y=c("min.feature","max.feature")),
#'        intervals=list(data=intervals,x=c("min.L","max.L"),y="feature"))
#' })
#' Lrange <- c()
#' for(N in names(blank.items)){
#'   L <- blank.items[[N]]
#'   Lrange <- range(c(Lrange,unlist(L$data[,L$x])),finite=TRUE)
#'   blank.items[[N]]$yrange <- range(unlist(L$data[,L$y]))
#' }
#' Lrange[1] <- Lrange[1]-1
#' Lrange[2] <- Lrange[2]+1
#' for(N in names(blank.items)){
#'   L <- blank.items[[N]]
#'   blank.items[[N]]$blank <- data.frame(x=Lrange, y=L$yrange)
#' }
#' 
#' mmir.plot <- 
#'   list(signal=ggplot()+
#'          scale_x_continuous("position on chromosome (mega base pairs)",
#'                             breaks=c(100,200))+
#'          geom_tallrect(aes(xmin=first.base/1e6, xmax=last.base/1e6,
#'                            fill=annotation,
#'                            showSelected=signal),
#'                        data=intreg$annotations)+
#'          scale_fill_manual(values=breakpoint.colors,guide="none")+
#'          geom_text(aes((first.base+last.base)/2e6, logratio+1/8,
#'                        label=annotation,
#'                        showSelected=signal),
#'                    data=intreg$annotations)+
#'          geom_blank(aes(first.base/1e6, logratio+2/8), data=intreg$ann)+
#'          geom_point(aes(base/1e6, logratio,
#'                         showSelected=signal),
#'                     data=intreg$signals)+
#'          geom_segment(aes(first.base/1e6, mean, xend=last.base/1e6, yend=mean,
#'                           showSelected=signal,
#'                           showSelected2=segments),
#'                       data=intreg$segments, colour=signal.colors[["estimate"]])+
#'          geom_vline(aes(xintercept=base/1e6,
#'                         showSelected=signal,
#'                         showSelected2=segments),
#'                     colour=signal.colors[["estimate"]],
#'                     linetype="dashed",
#'                     data=intreg$breaks),
#'        regression=ggplot()+
#'          geom_blank(aes(x,y), data=blank.items$regression$blank)+
#'          geom_segment(aes(min.L, feature, xend=max.L, yend=feature,
#'                           clickSelects=signal),
#'                       size=5,
#'                       data=intreg$int)+
#'          geom_segment(aes(min.L, min.feature, xend=max.L, yend=max.feature,
#'                           linetype=line),
#'                       colour="red",
#'                       size=3,
#'                       data=intreg$model)+
#'          scale_linetype_manual(values=model.linetypes),
#'        error=ggplot()+
#'          geom_blank(aes(x,y), data=blank.items$error$blank)+
#'          geom_segment(aes(min.L, cost, xend=max.L, yend=cost,
#'                           showSelected=signal), data=intreg$selection),
#'        segments=ggplot()+
#'          geom_blank(aes(x,y), data=blank.items$segments$blank)+
#'          geom_segment(aes(min.L, segments, xend=max.L, yend=segments,
#'                           showSelected=signal), data=intreg$selection)+
#'          geom_tallrect(aes(xmin=min.L, xmax=max.L,
#'                            showSelected=signal,
#'                            clickSelects=segments),
#'                        data=intreg$selection,
#'                        alpha=1/2),
#'        width=list(800),
#'        height=list(signal=300,regression=150,error=50,segments=100))
#' ## This is a normal ggplot of all the data, subsets of which can be
#' ## shown by clicking the plots.
#' sig.facets <- mmir.plot$sig+
#'   facet_grid(segments~signal, scales="free", space="free_x")+
#'   theme_bw()+
#'   theme(panel.margin=unit(0,"cm"))
#' print(sig.facets)
#' gg2animint(mmir.plot)
geom_tallrect <- function(mapping=NULL, data=NULL, stat="identity", position="identity", ...){
  GeomTallRect <- proto(ggplot2:::GeomRect,{
    objname <- "tallrect"
    required_aes <- c("xmin", "xmax")
    draw <- draw_groups <- function(.,data,scales,coordinates,
                                    ymin=0,ymax=1,...){
      ymin <- grid::unit(ymin,"npc")
      ymax <- grid::unit(ymax,"npc")
      with(ggplot2:::coord_transform(coordinates, data, scales),
           ggname(.$my_name(), {
        rectGrob(xmin, ymin, xmax - xmin, ymax-ymin,
                 default.units = "native", just = c("left", "bottom"), 
                 gp=gpar(
                   col=colour, fill=alpha(fill, alpha), 
                   lwd=size * .pt, lty=linetype, lineend="butt"
                   )
                 )
      }))
    }
  })
  GeomTallRect$new(mapping = mapping, data = data, stat = stat,
                   position = position, ...)
}

##' Make a clickSelects geom_tallrect that completely tiles the x
##' range. This makes it easy to construct tallrects for the common
##' case of selecting a particular x value.
##' @param x.name variable to be used for x, clickSelects.
##' @param data data.frame to analyze for unique x.name values.
##' @param alpha transparency of a selected tallrect, default 1/2.
##' @return a geom_tallrect layer.
##' @author Toby Dylan Hocking
##' @export
##' @examples
##' data(worldPop)
##' popPlot <- ggplot()+
##'   make_tallrect("year", worldPop)+
##'   geom_line(aes(year, population, group=subcontinent),
##'             data=worldPop, size=4)
##' print(popPlot)
##' gg2animint(list(popPlot=popPlot))
make_tallrect <- function(x.name, data, alpha=1/2){
  stopifnot(is.data.frame(data))
  stopifnot(is.character(x.name))
  stopifnot(length(x.name)==1)
  x <- data[,x.name]
  stopifnot(is.numeric(x))
  vals <- sort(unique(x))
  Delta <- diff(vals)/2
  breaks <- c(vals[1] - Delta[1],
              vals[-1] - Delta,
              vals[length(vals)]+Delta[length(Delta)])
  
  stopifnot(length(breaks) == length(vals)+1)
  df <- data.frame(vals,
                   xmin=breaks[-length(breaks)],
                   xmax=breaks[-1])
  names(df)[1] <- x.name
  a <- aes_string(xmin="xmin", xmax="xmax", clickSelects=x.name)
  geom_tallrect(a, df, alpha=alpha)
}

### Convenience function for an interactive bar.
make_bar <- function(x.name, data, alpha=1){
  stat_summary(aes_string(x=x.name, y=x.name, clickSelects=x.name),
               data=data, alpha=alpha, fun.y=length, geom="bar")
}

\name{gg2animint}
\alias{gg2animint}
\title{gg2animint}
\usage{
  gg2animint(plot.list, out.dir = tempfile(),
    open.browser = interactive())
}
\arguments{
  \item{plot.list}{list of named ggplots with showSelected
  and clickSelects aesthetics. Input must be a list, so to
  use a single ggplot named g, it must be passed to the
  function as plot.list = list(g=g).}

  \item{out.dir}{directory to store html/js/csv files}

  \item{open.browser}{Should R open a browser? Note: Chrome
  will not display local html files unless you are running
  a local webserver or have launched chrome with the option
  --allow-file-access-from-files. Firefox should display
  local html files (including those containing
  javascript).}
}
\value{
  invisible list of ggplots in list format
}
\description{
  Workhorse function for the animint package. Convert a
  list of ggplots to a d3-ready graphic. Adds aesthetics
  clickSelects and updateSelected to utilize d3's mouseover
  and interactivity features for multiple linked plots, and
  allows animated sequences of graphics.
}
\details{
  Supported ggplot2 geoms: \itemize{ \item point \item
  jitter \item line \item rect \item tallRect (new with
  this package) \item segment \item hline \item vline \item
  bar \item text \item tile \item raster \item ribbon \item
  abline \item density \item path \item polygon \item
  histogram \item violin \item linerange \item step \item
  contour \item density2d \item area \item freqpoly }
  Unsupported geoms: \itemize{ \item rug \item dotplot
  \item hex \item quantile - should *theoretically* work
  but in practice does not work \item smooth - can be
  created using geom_line and geom_ribbon \item boxplot -
  can be created using geom_rect and geom_segment \item
  crossbar - can be created using geom_rect and
  geom_segment \item pointrange - can be created using
  geom_linerange and geom_point \item bin2d - bin using
  ddply() and then use geom_tile() \item map - can be
  created using geom_polygon or geom_path } Supported
  scales: \itemize{ \item alpha, \item fill/colour (brewer,
  gradient, identity, manual) \item linetype \item x and y
  axis scales, manual break specification, label formatting
  \item area \item size } Unsupported scales: \itemize{
  \item shape. Open and closed circles can be represented
  by manipulating fill and colour scales and using default
  (circle) points, but d3 does not support many R shape
  types, so mapping between the two is difficult. }
}
\examples{
library(animint)

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
         geom_segment(aes(x=min.L, y=feature, xend=max.L, yend=feature,
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
}
\seealso{
  \code{\link{ggplot2}}
}


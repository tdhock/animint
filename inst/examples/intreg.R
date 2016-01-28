works_with_R("3.2.2",
             "tdhock/animint@3b1f84ec926ffbd765f0aa004596e43203750fd4")

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
       theme_animint(height=300, width=800)+       
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
       theme_animint(height=150, width=800)+
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
       theme_animint(height=100, width=800)+
       geom_blank(aes(x,y), data=blank.items$error$blank)+
       geom_segment(aes(min.L, cost, xend=max.L, yend=cost,
                        showSelected=signal), data=intreg$selection),
       segments=ggplot()+
       theme_animint(height=100, width=800)+
       geom_blank(aes(x,y), data=blank.items$segments$blank)+
       geom_segment(aes(min.L, segments, xend=max.L, yend=segments,
                        showSelected=signal), data=intreg$selection)+
       geom_tallrect(aes(xmin=min.L, xmax=max.L,
                         showSelected=signal,
                         clickSelects=segments),
                     data=intreg$selection,
                     alpha=1/2))
## This is a normal ggplot of all the data, subsets of which can be
## shown by clicking the plots.
sig.facets <- mmir.plot$sig+
  facet_grid(segments~signal, scales="free", space="free_x")+
  theme_bw()+
  theme(panel.margin=grid::unit(0,"cm"))
print(sig.facets)
animint2dir(mmir.plot, "intreg-nofacets")

## The mmir.plot above is way too complicated, since it does not use
## facets. The simpler facetted version looks like this:
mmir.facet <- 
  list(signal=mmir.plot$signal,
       
       penalty=ggplot()+
       geom_tallrect(aes(xmin=min.L, xmax=max.L,
                         showSelected=signal,
                         clickSelects=segments),
                     data=data.frame(intreg$selection, what="segments"),
                     alpha=1/2)+
       ylab("")+
       theme_bw()+
       theme_animint(height=500, width=800)+
       theme(panel.margin=grid::unit(0, "lines"))+
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
       xlab("penalty value $L=f(x)$")+ # TODO: mathjax.
       facet_grid(what~.,scales="free"),

       title="Max-margin interval regression")
animint2dir(mmir.facet, "intreg-facets") 
## This plot has an additional facet for signal, which would not be
## present in the interactive plot, but is useful here to see all
## the data in regular ggplot2.
too.many.facets <- mmir.facet$penalty+
  facet_grid(what~signal, scales="free")+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "cm"))
print(too.many.facets)

## Regions with linetype indicating errors.
breaks.by.signal <- split(intreg$breaks, intreg$breaks$signal)
anns.by.signal <- split(intreg$ann, intreg$ann$signal)
error.regions.list <- list()
for(signal in names(breaks.by.signal)){
  signal.breaks <- breaks.by.signal[[signal]]
  signal.anns <- anns.by.signal[[signal]]
  signal.anns$target.breaks <-
    ifelse(signal.anns$annotation=="1breakpoint", 1, 0)
  for(model.i in 1:20){
    model.breaks <- subset(signal.breaks, segments==model.i)
    signal.anns$breaks <- NA
    for(region.i in 1:nrow(signal.anns)){
      region <- signal.anns[region.i, ]
      after.start <- region$first.base < model.breaks$base
      before.end <- model.breaks$base < region$last.base
      signal.anns$breaks[region.i] <- sum(after.start & before.end)
    }
    signal.anns$error.type <- with(signal.anns, {
      ifelse(breaks < target.breaks, "false negative",
             ifelse(target.breaks < breaks, "false positive", "correct"))
    })
    error.regions.list[[paste(model.i, signal)]] <-
      data.frame(segments=model.i, signal.anns)
  }
}
error.regions <- do.call(rbind, error.regions.list)

reg <- subset(intreg$model, line=="regression")
slope <- with(reg, (min.L-max.L)/(min.feature-max.feature))
intreg$intervals$pred.L <-
  slope * (intreg$intervals$feature - reg$min.feature) + reg$min.L
intreg.errors <- 
  list(signal=ggplot()+
       theme_animint(height=300, width=800)+       
       scale_x_continuous("position on chromosome (mega base pairs)",
                          breaks=c(100,200))+
       ylab("noisy copy number logratio signal")+
       geom_tallrect(aes(xmin=first.base/1e6, xmax=last.base/1e6,
                         fill=annotation,
                         linetype=error.type,
                         showSelected2=segments,
                         showSelected=signal),
                     color="black",
                     alpha=0.5,
                     data=error.regions)+
       scale_linetype_manual("error type", values=c(correct=0,
                                   "false negative"=3,
                                   "false positive"=1))+
       guides(linetype=guide_legend(override.aes=list(fill="white")))+       
       scale_fill_manual(values=breakpoint.colors)+
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
       penalty=ggplot()+
         theme_bw()+
         theme_animint(height=500, width=800)+
         theme(panel.margin=grid::unit(0, "cm"))+
       geom_tallrect(aes(xmin=min.L, xmax=max.L,
                         showSelected=signal,
                         clickSelects=segments),
                     data=data.frame(intreg$selection
                      ##,what="segments"
                                     ),
                     alpha=1/2)+
       ylab("")+
       geom_vline(aes(xintercept=pred.L, showSelected=signal),
                  color="violet",
                  data=intreg$intervals)+
       geom_segment(aes(min.L, feature, xend=max.L, yend=feature,
                        clickSelects=signal),
                    size=6,
                    data=data.frame(intreg$intervals, what="regression"))+
       geom_segment(aes(min.L, min.feature, xend=max.L, yend=max.feature,
                        linetype=line),
                    colour="violet",
                    size=3,
                    data=data.frame(intreg$model, what="regression"))+
       ## geom_point(aes(pred.L, feature),
       ##            size=5,
       ##            data=data.frame(intreg$intervals, what="regression"))+
       scale_linetype_manual(values=model.linetypes)+
       geom_segment(aes(min.L, cost, xend=max.L, yend=cost,
                        showSelected=signal),
                    data=data.frame(intreg$selection, what="incorrect labels"))+
       geom_segment(aes(min.L, segments, xend=max.L, yend=segments,
                        showSelected=signal),
                    data=data.frame(intreg$selection, what="segments"))+
       xlab("penalty value log(lambda)")+ # TODO: mathjax.
       facet_grid(what~., scales="free"),
       title="Max-margin interval regression")
animint2dir(intreg.errors, "intreg-errors")

##animint2gist(intreg.errors)

## No annotated regions!
mmir.segs <- 
  list(signal=ggplot()+
       theme_animint(height=300, width=800)+       
       scale_x_continuous("position on chromosome (mega base pairs)",
                          breaks=c(100,200))+
       scale_fill_manual(values=breakpoint.colors,guide="none")+
       geom_blank(aes(first.base/1e6, logratio+2/8), data=intreg$ann)+
       ggtitle("Copy number profile and maximum likelihood segmentation")+
       ylab("logratio")+
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
       
       segments=ggplot()+
       theme_animint(height=300, width=800)+
       xlab("log(penalty)")+
       ylab("optimal number of segments")+
       ggtitle("Select profile and number of segments")+
       geom_tallrect(aes(xmin=min.L, xmax=max.L,
                         showSelected=signal,
                         clickSelects=segments),
                     data=intreg$selection,
                     alpha=1/2)+
       geom_segment(aes(min.L, segments, xend=max.L, yend=segments,
                        clickSelects=signal),
                    data=intreg$selection, alpha=0.6, size=5))
animint2dir(mmir.segs, "intreg-segs")

## TODO: plot reconstruction error vs model complexity! (instead of
## penalty)
library(data.table)
library(dplyr)
segs <- data.table(intreg$segments)
sigs <- data.table(intreg$signals) %>%
  mutate(base.after=base+1)
setkey(segs, signal, first.base, last.base)
setkey(sigs, signal, base, base.after)
ov <- foverlaps(sigs, segs)
model.selection <- ov %>%
  group_by(signal, segments) %>%
  summarise(error=sum((logratio-mean)^2),
            data=n()) %>%
  mutate(log.data=log(data),
         penalized.error=error + log.data * segments)
sig.labels <- model.selection %>%
  group_by(signal) %>%
  filter(segments==1)
sig.seg.names <- ov %>%
  group_by(signal, segments) %>%
  summarise(min.base=min(base),
            max.base=max(base)) %>%
  mutate(base=(min.base+max.base)/2)
sig.names <- sig.seg.names %>%
  group_by(signal, base) %>%
  summarise(logratio=max(sigs$logratio))
seg.names <- sig.seg.names %>%
  group_by(signal, segments, base) %>%
  summarise(logratio=min(sigs$logratio)-0.2)
tallrects <- make_tallrect(model.selection, "segments")
tallrects$geom_params$colour <- signal.colors[["estimate"]]

## Plot segments rather than penalty.
mmir.selection <- 
  list(segments=ggplot()+
       ggtitle("Select profile and number of segments")+
       tallrects+
       geom_text(aes(0, error, label=signal, clickSelects=signal),
                 data=sig.labels, hjust=1)+
       scale_x_continuous("segments", breaks=c(1, 5, 10, 20),
                          limits=c(-2, 20))+
       xlab("squared error")+
       geom_line(aes(segments, error,
                     group=signal,
                     clickSelects=signal),
                 data=model.selection,
                 alpha=0.6, size=8),

       signal=ggplot()+
       theme_animint(width=800)+       
       scale_x_continuous("position on chromosome (mega base pairs)",
                          breaks=c(100,200))+
       scale_fill_manual(values=breakpoint.colors,guide="none")+
       geom_blank(aes(first.base/1e6, logratio+2/8), data=intreg$ann)+
       ggtitle("Copy number profile and maximum likelihood segmentation")+
       ylab("logratio")+
       geom_point(aes(base/1e6, logratio,
                      showSelected=signal),
                  data=intreg$sig)+
       geom_segment(aes(first.base/1e6, mean, xend=last.base/1e6, yend=mean,
                        showSelected=signal,
                        showSelected2=segments),
                    data=intreg$seg, colour=signal.colors[["estimate"]])+
       geom_segment(aes(base/1e6, min(sigs$logratio),
                        xend=base/1e6, yend=max(sigs$logratio),
                        showSelected=signal,
                        showSelected2=segments),
                  colour=signal.colors[["estimate"]],
                  linetype="dashed",
                  data=intreg$breaks)+
       geom_text(aes(base/1e6, logratio, label=paste("signal", signal),
                     showSelected=signal),
                 data=sig.names)+
       geom_text(aes(base/1e6, logratio,
                     label=sprintf("%d segment%s", segments,
                       ifelse(segments==1, "", "s")),
                     showSelected=signal,
                     showSelected2=segments),
                 data=seg.names, color=signal.colors[["estimate"]]),

       first=list(signal="4.2", segments=4))
animint2dir(mmir.selection, "intreg-selection")
##animint2gist(mmir.selection)

library(reshape2)
model.tall <- melt(model.selection, measure.vars=c("error", "penalized.error"))
## Plot error AND penalized error versus number of segments.
mmir.buggy <- 
  list(segments=ggplot()+
       ggtitle("Select profile and number of segments")+
       tallrects+
       scale_x_continuous("segments", breaks=c(1, 5, 10, 20),
                          limits=c(-2, 21))+
       xlab("")+
       facet_grid(variable ~ ., scales="free_y")+
       geom_line(aes(segments, value,
                     group=interaction(signal, variable),
                     clickSelects=signal),
                 data=model.tall,
                 alpha=0.6, size=8),

       signal=ggplot()+
       theme_animint(width=800)+       
       scale_x_continuous("position on chromosome (mega base pairs)",
                          breaks=c(100,200))+
       scale_fill_manual(values=breakpoint.colors,guide="none")+
       geom_blank(aes(first.base/1e6, logratio+2/8), data=intreg$ann)+
       ggtitle("Copy number profile and maximum likelihood segmentation")+
       ylab("logratio")+
       geom_point(aes(base/1e6, logratio,
                      showSelected=signal),
                  data=intreg$sig)+
       geom_segment(aes(first.base/1e6, mean, xend=last.base/1e6, yend=mean,
                        showSelected=signal,
                        showSelected2=segments),
                    data=intreg$seg, colour=signal.colors[["estimate"]])+
       geom_segment(aes(base/1e6, min(sigs$logratio),
                        xend=base/1e6, yend=max(sigs$logratio),
                        showSelected=signal,
                        showSelected2=segments),
                  colour=signal.colors[["estimate"]],
                  linetype="dashed",
                  data=intreg$breaks)+
       geom_text(aes(base/1e6, logratio, label=paste("signal", signal),
                     showSelected=signal),
                 data=sig.names)+
       geom_text(aes(base/1e6, logratio,
                     label=sprintf("%d segment%s", segments,
                       ifelse(segments==1, "", "s")),
                     showSelected=signal,
                     showSelected2=segments),
                 data=seg.names, color=signal.colors[["estimate"]]),

       first=list(signal="4.2", segments=4))
animint2dir(mmir.buggy, "intreg-buggy")

## Plot error AND penalized error versus number of segments.
penalized <- model.selection %>%
  group_by(signal) %>%
  filter(penalized.error < penalized.error[1]*2)
mmir.BIC <- 
  list(segments=ggplot()+
       ggtitle("Select profile and number of segments")+
       tallrects+
       scale_x_continuous("segments", breaks=c(1, 5, 10, 20),
                          limits=c(-2, 21))+
       xlab("")+
       facet_grid(variable ~ ., scales="free_y")+
       geom_line(aes(segments, error,
                     group=signal,
                     clickSelects=signal),
                 data=data.frame(model.selection,
                   variable="un-penalized error"),
                 alpha=0.6, size=8)+
       geom_line(aes(segments, penalized.error,
                     group=signal,
                     clickSelects=signal),
                 data=data.frame(penalized, variable="penalized error (BIC)"),
                 alpha=0.6, size=8),

       signal=ggplot()+
       theme_animint(width=800)+       
       scale_x_continuous("position on chromosome (mega base pairs)",
                          breaks=c(100,200))+
       scale_fill_manual(values=breakpoint.colors,guide="none")+
       geom_blank(aes(first.base/1e6, logratio+2/8), data=intreg$ann)+
       ggtitle("Copy number profile and maximum likelihood segmentation")+
       ylab("logratio")+
       geom_point(aes(base/1e6, logratio,
                      showSelected=signal),
                  data=intreg$sig)+
       geom_segment(aes(first.base/1e6, mean, xend=last.base/1e6, yend=mean,
                        showSelected=signal,
                        showSelected2=segments),
                    data=intreg$seg, colour=signal.colors[["estimate"]])+
       geom_segment(aes(base/1e6, min(sigs$logratio),
                        xend=base/1e6, yend=max(sigs$logratio),
                        showSelected=signal,
                        showSelected2=segments),
                  colour=signal.colors[["estimate"]],
                  linetype="dashed",
                  data=intreg$breaks)+
       geom_text(aes(base/1e6, logratio, label=paste("signal", signal),
                     showSelected=signal),
                 data=sig.names)+
       geom_text(aes(base/1e6, logratio,
                     label=sprintf("%d segment%s", segments,
                       ifelse(segments==1, "", "s")),
                     showSelected=signal,
                     showSelected2=segments),
                 data=seg.names, color=signal.colors[["estimate"]]),

       first=list(signal="4.2", segments=4))
animint2dir(mmir.BIC, "intreg-BIC")

##animint2gist(mmir.BIC)


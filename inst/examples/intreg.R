library(animint)

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
animint2dir(mmir.plot)

## TODO: mmir.plot is way too complicated, since facets are not yet
## implemented in animint. The easier facetted version would look
## like this:
mmir.facet <- 
  list(signal=mmir.plot$signal,
       penalty=ggplot()+
       geom_tallrect(aes(xmin=min.L, xmax=max.L,
                         showSelected=signal,
                         clickSelects=segments),
                     data=data.frame(intreg$selection, what="segments"),
                     alpha=1/2)+
       ylab("")+
       theme_animint(height=500, width=800)+
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
       facet_grid(what~.,scales="free"))
animint2dir(mmir.facet) # doesn't work yet.
## This plot has an additional facet for signal, which would not be
## present in the interactive plot, but is useful here to see all
## the data in regular ggplot2.
too.many.facets <- mmir.facet$penalty+
  facet_grid(what~signal, scales="free")+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "cm"))
print(too.many.facets)

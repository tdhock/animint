data(breakpoints)
some.models <- subset(breakpoints$error, segments < 15)
library(plyr)
roc <- ddply(some.models, .(segments, bases.per.probe), function(d){
  rownames(d) <- d$type
  e <- d[,"error",drop=FALSE]
  B <- 5   # number of real breakpoints.
  N <- 8e5 # number of points we could possibly sample.
  max.possible.breaks <- N-1 # there could be a break after every point.
  max.fp <- max.possible.breaks - B
  FNR <- (e["FN",]+e["I",])/B
  FPR <- e["FP",]/max.fp
  TPR <- 1-FNR
  data.frame(TPR, FPR, FNR)
})
only.error <- subset(some.models, type=="E")
only.segments <- subset(only.error,bases.per.probe==bases.per.probe[1])
signal.colors <- c(estimate="#0adb0a",
                   imprecision="#0098ef")
breakpoints$imprecision$line <- "imprecision"
breakpoints$segments$line <- "estimate"
breakpoints$breaks$line <- "estimate"
viz <- 
  list(signal=ggplot()+
       geom_point(aes(position, signal, showSelected=bases.per.probe),
                  data=breakpoints$signals)+
       geom_line(aes(position, signal, colour=line),
                 data=breakpoints$imprecision)+
       geom_segment(aes(first.base, mean, xend=last.base, yend=mean,
                        colour=line,
                        showSelected=segments,
                        showSelected2=bases.per.probe),
                    data=breakpoints$segments, size=3)+
       ggtitle(paste("Noisy signal (black) and model (green)",
                     "with imprecision component of breakpointError (blue)"))+
       geom_vline(aes(xintercept=base, colour=line,
                      showSelected=segments,
                      showSelected2=bases.per.probe),
                  linetype="dashed",
                  data=breakpoints$breaks)+
       scale_colour_manual(values=signal.colors),
       error=ggplot()+
       geom_line(aes(segments, error, group=type,
                     showSelected=bases.per.probe,
                     colour=type, linetype=type, size=type),
                 data=some.models)+
       make_tallrect(only.segments, "segments")+
       ggtitle("breakpointError and its components")+
       geom_line(aes(segments, error, group=bases.per.probe,
                     clickSelects=bases.per.probe),
                 data=only.error, lwd=4),
       roc=ggplot()+
       geom_path(aes(FPR, TPR, group=bases.per.probe,
                     clickSelects=bases.per.probe), data=roc, lwd=4)+
       geom_point(aes(FPR, TPR, showSelected=bases.per.probe,
                      clickSelects=segments), alpha=3/4,
                  colour=signal.colors[["estimate"]],
                  data=roc, size=5)+
       xlab(paste("False positive rate =",
                  "probability(predict breakpoint|no breakpoint)"))+
       ylab(paste("True positive rate =",
                  "probability(predict breakpoint|breakpoint)"))+
       ggtitle("ROC curves"),
       width=list(signal=1000, error=500, roc=500))
## Make manual scales.
fp.fn <- list(colour=c(FP="skyblue",FN="#E41A1C",E="black",I="black"),
              linetype=c(E="solid",FP="solid",FN="solid",I="dashed"),
              size=c(E=1,FP=3,FN=3,I=1)*2)
for(a in names(fp.fn)){
  values <- fp.fn[[a]]
  labels <-
    c("False negative", "imprecision", "False positive", "breakpointError")
  sc <- ggplot2:::manual_scale(a, values, name="component",
                               breaks=c("FN", "I", "FP", "E"),
                               labels=labels)
  viz$error <- viz$error + sc
  print(viz$err)
}

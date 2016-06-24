library(animint)

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
animint2dir(breakpointError)


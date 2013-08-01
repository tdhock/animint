library(ggplot2)
library(animint)
library(plyr)

#' density: should show two normal distributions, centered at 0 and 3, and a gamma distribution with mode approximately 5
boxplotdata <- rbind(data.frame(x=1:50, y=sort(rnorm(50, 3, 1)), group="N(3,1)"),
                     data.frame(x=1:50, y=sort(rnorm(50, 0, 1)), group="N(0,1)"), 
                     data.frame(x=1:50, y=sort(rgamma(50, 2, 1/3)), group="Gamma(2,1/3)"))
boxplotdata <- ddply(boxplotdata, .(group), transform, ymax=max(y), ymin=min(y), med=median(y))

f1 <- ggplot() + geom_density(data=boxplotdata, aes(x=y, group=group, fill=group), alpha=.5) +
  scale_fill_discrete("Distribution") + xlab("x") + 
  ggtitle("geom_density") + facet_wrap(~group)
f1

temp <- ggplot2::ggplot_build(f1)
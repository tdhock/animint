library(ggplot2)
library(animint)
library(plyr) # to access round_any
movies$decade <- round_any(movies$year, 10)
m <- ggplot(movies, aes(x=rating, colour=decade, group=decade)) + 
  geom_density(fill=NA) + scale_colour_continuous(guide="legend") + 
  guides(colour=guide_legend(order=1))
m 

temp <- ggplot2::ggplot_build(m)

temp.tab <- ggplot2::ggplot_gtable(temp)
grid(temp.tab$grobs[[8]])
r <- gtable_filter(ggplot_gtable(ggplot_build(m)), "guide-box") 
gg<- r$grobs[[1]]

gtable_filter(ggplot_gtable(ggplot_build(m)), trim=TRUE, "legend.key.rect") 

ggplot2:::guide_geom.legend(m$guides$colour, m$scales$scales[[1]], m$mapping)
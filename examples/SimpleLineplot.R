library(animint)
library(ggplot2)

#' Demonstrates line plots in animint, debugging, serves as an example

#' Generate data
data <- data.frame(x=rep(1:10, times=5), group = rep(1:5, each=10))
data$lt <- factor(data$group%%2+1) # linetype
data$group <- as.factor(data$group)
data$y <- rnorm(length(data$x), data$x, .5) + rep(rnorm(5, 0, 2), each=10)


#' Simple line plot
p1 <- ggplot() + geom_line(data=data, aes(x=x, y=y, group=group))
p1
# gg2animint(list(p1=p1), out.dir="./junk")


#' Simple line plot with colors...
p2 <- ggplot() + geom_line(data=data, aes(x=x, y=y, colour=group, group=group))
p2
# gg2animint(list(p1=p1, p2=p2), out.dir="./junk")

#' Simple line plot with colors and linetype
p3 <- ggplot() + geom_line(data=data, aes(x=x, y=y, colour=group, group=group, linetype=lt))
p3
# gg2animint(list(p1=p1, p2=p2, p3=p3))

#' Use automatic linetypes from ggplot with coerced factors
p4 <- ggplot() + geom_line(data=data, aes(x=x, y=y, colour=group, group=group, linetype=factor(group)))
p4
# gg2animint(list(p1=p1, p2=p2, p3=p3, p4=p4))

#' Manually specify linetypes using <length, space, length, space...> notation
data$lt <- rep(c("2423", "2415", "331323", "F2F4", "solid"), each=10)
p5 <- ggplot() + geom_line(data=data, aes(x=x, y=y, colour=group, group=group, linetype=lt)) + scale_linetype_identity()
p5
# gg2animint(list(p1=p1, p2=p2, p3=p3, p4=p4, p5=p5))

#' All possible linetypes
lts <- scales::linetype_pal()(13)
lt1 <- data.frame(x=0, xend=.25, y=1:13, yend=1:13, lt=lts)
p6 <- ggplot()+geom_segment(data=lt1, aes(x=x, xend=xend, y=y, yend=yend, linetype=lt)) + 
      scale_linetype_identity() + geom_text(data=lt1, aes(x=-.125, y=y, label=lt), hjust=0)
# gg2animint(list(p1=p1, p2=p2, p3=p3, p4=p4, p5=p5, p6=p6))

lts2 <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
lt2 <- data.frame(x=0, xend=.25, y=1:6, yend=1:6, lt=lts2)
p7 <- ggplot() + geom_segment(data=lt2, aes(x=x, xend=xend, y=y, yend=yend, linetype=lt)) + 
      scale_linetype_identity() + geom_text(data=lt2, aes(x=-.125, y=y, label=lt), hjust=0)
gg2animint(list(p1=p1, p2=p2, p3=p3, p4=p4, p5=p5, p6=p6, p7=p7))

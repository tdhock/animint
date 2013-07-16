library(animint)
library(ggplot2)

#' Demonstrates line plots in animint, debugging, serves as an example

#' Generate data
data <- data.frame(x=rep(1:10, times=5), group = rep(1:5, each=10))
data$lt <- factor(data$group%%2+1) # linetype
data$group <- as.factor(data$group)
data$y <- rnorm(length(data$x), data$x, .5) + rep(rnorm(5, 0, 2), each=10)


#' Simple line plot
p1 <- ggplot() + geom_line(data=data, aes(x=x, y=y, group=group)) + 
  ggtitle("geom_line")
p1
# gg2animint(list(p1=p1), out.dir="./junk")


#' Simple line plot with colors...
p2 <- ggplot() + geom_line(data=data, aes(x=x, y=y, colour=group, group=group)) +
  ggtitle("geom_line + scale_colour_discrete")
p2
# gg2animint(list(p1=p1, p2=p2), out.dir="./junk")

#' Simple line plot with colors and linetype
p3 <- ggplot() + geom_line(data=data, aes(x=x, y=y, colour=group, group=group, linetype=lt)) +
  ggtitle("geom_line + scale_linetype_manual")
p3
# gg2animint(list(p1=p1, p2=p2, p3=p3))

#' Use automatic linetypes from ggplot with coerced factors
p4 <- ggplot() + geom_line(data=data, aes(x=x, y=y, colour=group, group=group, linetype=factor(group))) +
  ggtitle("geom_line + scale_linetype automatic")
p4
# gg2animint(list(p1=p1, p2=p2, p3=p3, p4=p4))

#' Manually specify linetypes using <length, space, length, space...> notation
data$lt <- rep(c("2423", "2415", "331323", "F2F4", "solid"), each=10)
p5 <- ggplot() + geom_line(data=data, aes(x=x, y=y, colour=group, group=group, linetype=lt)) + scale_linetype_identity() + ggtitle("Manual Linetypes: dash-space length")
p5
# gg2animint(list(p1=p1, p2=p2, p3=p3, p4=p4, p5=p5))

#' All possible linetypes
lts <- scales::linetype_pal()(13)
lt1 <- data.frame(x=0, xend=.25, y=1:13, yend=1:13, lt=lts, lx = -.125)
p6 <- ggplot()+geom_segment(data=lt1, aes(x=x, xend=xend, y=y, yend=yend, linetype=lt)) + 
  scale_linetype_identity() + geom_text(data=lt1, aes(x=lx, y=y, label=lt), hjust=0) + 
  ggtitle("Scales package: all linetypes")
# gg2animint(list(p1=p1, p2=p2, p3=p3, p4=p4, p5=p5, p6=p6))

lts2 <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
lt2 <- data.frame(x=0, xend=.25, y=1:6, yend=1:6, lt=lts2, lx=-.125)
p7 <- ggplot() + geom_segment(data=lt2, aes(x=x, xend=xend, y=y, yend=yend, linetype=lt)) + 
  scale_linetype_identity() + geom_text(data=lt2, aes(x=lx, y=y, label=lt), hjust=0) +
  ggtitle("Named linetypes")
# gg2animint(list(p1=p1, p2=p2, p3=p3, p4=p4, p5=p5, p6=p6, p7=p7))

#' Spaghetti Plot Data
n <- 500
pts <- 10
data2 <- data.frame(x=rep(1:pts, times=n), group = rep(1:n, each=pts))
data2$group <- as.factor(data2$group)
data2$y <- rnorm(length(data2$x), data2$x*rep(rnorm(n, 1, .25), each=pts), .25) + rep(rnorm(n, 0, 1), each=pts)
data2$lty <- "solid"
data2$lty[which(data2$group%in%subset(data2, x==10)$group[order(subset(data2, x==10)$y)][1:floor(n/10)])] <- "3133"
data2 <- ddply(data2, .(group), transform, maxy = max(y), miny=min(y))

qplot(data=data2, x=x, y=y, group=group, geom="line", alpha=I(.2))

#' Check whether scale_alpha works.
p8 <- ggplot() + geom_line(data=data2, aes(x=x, y=y, group=group), alpha=.1) +
  ggtitle("scale_alpha")
p8
# gg2animint(list(p1=p1, p2=p2, p3=p3, p4=p4, p5=p5, p6=p6, p7=p7, p8=p8))

p9 <- ggplot() + geom_line(data=subset(data2, as.numeric(group) < 50), 
                           aes(x=x, y=y, group=group, linetype=lty), alpha=.1) +
  scale_linetype_identity() + 
  ggtitle("scale_alpha + scale_linetype")
p9
# gg2animint(list(p1=p1, p2=p2, p3=p3, p4=p4, p5=p5, p6=p6, p7=p7, p8=p8, p9=p9))

p10 <- ggplot() + geom_line(data=subset(data2, as.numeric(group) < 50), 
                            aes(x=x, y=y, group=group, linetype=factor(sign(miny)), alpha=maxy)) + 
  scale_alpha_continuous(range=c(.1, .5)) + 
  ggtitle("scale_alpha_continuous")
p10
# gg2animint(list(p1=p1, p2=p2, p3=p3, p4=p4, p5=p5, p6=p6, p7=p7, p8=p8, p9=p9, p10=p10))

#' Size Scaling
p11 <- ggplot() + geom_line(data=subset(data2, as.numeric(group)%%50 ==1), 
                            aes(x=x, y=y, group=group, size=(floor(miny)+3)/3)) + 
  scale_size_continuous(range=c(1,3)) +
  ggtitle("scale_size_continuous")
p11
# gg2animint(list(p1=p1, p2=p2, p3=p3, p4=p4, p5=p5, p6=p6, p7=p7, p8=p8, p9=p9, p10=p10, p11=p11))

p12 <- ggplot() + geom_line(data=data2, aes(x=x, y=y, group=group, alpha=miny, colour=maxy)) + 
  scale_alpha_continuous(range=c(.1, .3)) + 
  ggtitle("scale_alpha + scale_colour")
p12
gg2animint(list(p1=p1, p2=p2, p3=p3, p4=p4, p5=p5, p6=p6, p7=p7, p8=p8, p9=p9, p10=p10, p11=p11, p12=p12))

#' Tests for each geom
library(ggplot2)
library(plyr)
library(animint)

#' abline: should show two lines: one running through the points, the other with an intercept of 0 and slope of 2.
xydata <- data.frame(x=sort(runif(50, 0, 10)))
xydata$y <- 3+2*xydata$x + rnorm(50, 0, 1)
g1 <- ggplot() + geom_point(data=xydata, aes(x=x, y=y)) + 
  geom_abline(data=data.frame(intercept=c(3, 0), slope=c(2,1)), aes(intercept=intercept, slope=slope)) +
  ggtitle("geom_abline")
g1
# gg2animint(list(g1=g1))

#' ribbon: should show two overlapping ribbons, with the same basic shape, one translated up by one unit.
ribbondata <- data.frame(x=seq(0, 1, .1), ymin=runif(11, 0, 1), ymax=runif(11, 1, 2))
ribbondata <- rbind(cbind(ribbondata, group=1), cbind(ribbondata, group=2))
ribbondata[12:22,2:3] <- ribbondata[12:22,2:3]+1
g2 <- ggplot() + 
  geom_ribbon(data=ribbondata, aes(x=x, ymin=ymin, ymax=ymax, group=group, fill=group), alpha=.5) + 
  ggtitle("geom_ribbon")
g2
# gg2animint(list(g1=g1, g2=g2))

#' density: should show an exponential density curve in blue and a normal(ish) density curve in pink.
densdata <- data.frame(x=c(rnorm(100), rexp(100)), group=rep(1:2, each=100))
g3 <- ggplot() + geom_density(data=densdata, aes(x=x, group=group, fill=factor(group)), alpha=.5) +
  ggtitle("geom_density")
g3
# gg2animint(list(g1=g1, g2=g2, g3=g3))


#' tile: should show an approximately bivariate normal distribution.
tiledata <- data.frame(x=rnorm(1000, 0, 3))
tiledata$y <- rnorm(1000, tiledata$x, 3)
tiledata$rx <- round(tiledata$x)
tiledata$ry <- round(tiledata$y)
tiledata <- ddply(tiledata, .(rx,ry), summarise, n=length(rx))

g4 <- ggplot() + geom_tile(data=tiledata, aes(x=rx, y=ry, fill=n)) +
  scale_fill_gradient(low="#56B1F7", high="#132B43") + 
  xlab("x") + ylab("y") + ggtitle("geom_tile")
g4
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4))

#' path: should show a two-dimensional random walk, where x and y are position, z is time.
pathdata <- data.frame(x=rnorm(30, 0, .5), y=rnorm(30, 0, .5), z=1:30)
g5 <- ggplot() + geom_path(data=pathdata, aes(x=x, y=y), alpha=.5) +
  geom_text(data=pathdata, aes(x=x, y=y, label=z)) + 
  ggtitle("geom_path")
g5
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5))

#' Polygons
polydata <- rbind(
  data.frame(x=c(0, .5, 1, .5, 0), y=c(0, 0, 1, 1, 0), group="parallelogram", fill="blue", xc=.5, yc=.5),
  data.frame(x=c(.5, .75, 1, .5), y=c(.5, 0, .5, .5), group="triangle", fill="red", xc=.75, yc=.33)
  )
g6 <- ggplot() + 
  geom_polygon(data=polydata, aes(x=x, y=y, group=group, fill=fill, colour=fill), alpha=.5)+
  scale_colour_identity() + scale_fill_identity()+
  geom_text(data=polydata, aes(x=xc, y=yc, label=group)) +
  ggtitle("geom_polygon")
g6
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6))

#' Boxplots
boxplotdata <- rbind(data.frame(x=1:50, y=sort(rnorm(50, 3, 1)), group=1),
                     data.frame(x=1:50, y=sort(rnorm(50, 0, 1)), group=2), 
                     data.frame(x=1:50, y=sort(rgamma(50, 2, 1/3)), group=3))
boxplotdata <- ddply(boxplotdata, .(group), transform, ymax=max(y), ymin=min(y), med=median(y))

#' Boxplot does not work (7/5/13)
# g7 <- ggplot() + 
#   geom_boxplot(data=boxplotdata, aes(y=y, x=factor(group))) +
#   ggtitle("geom_boxplot")
# g7
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7))

g7 <- ggplot() + 
  geom_linerange(data=boxplotdata, aes(x=factor(group), ymax=ymax, ymin=ymin, colour=factor(group))) +
  ggtitle("geom_linerange")
g7
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7))

g8 <- ggplot() + 
  geom_histogram(data=subset(boxplotdata, group==3), aes(x=y, fill=..count..), binwidth=1) + 
  ggtitle("geom_histogram")
g8
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7, g8=g8))

g9 <- ggplot() + 
  geom_violin(data=boxplotdata, aes(x=factor(group), y=y, fill=factor(group), group=group)) +
  ggtitle("geom_violin")
g9
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7, g8=g8, g9=g9))

#' Step Plot
#' Must specify group and then use colour=factor(group) to get desired effect.
g10 <- ggplot() + geom_step(data=boxplotdata, aes(x=x, y=y, colour=factor(group), group=group)) +
  ggtitle("geom_step")
g10
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7, g8=g8, g9=g9, g10=g10))

#' contour plot
library(reshape2) # for melt
contourdata <- melt(volcano)
names(contourdata) <- c("x", "y", "z")
g11 <- ggplot() + geom_contour(data=contourdata, aes(x=x, y=y, z=z), binwidth=4, size=0.5) + 
  geom_contour(data=contourdata, aes(x=x, y=y, z=z), binwidth=10, size=1) +
  ggtitle("geom_contour")
g11
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7, g8=g8, g9=g9, g10=g10, g11=g11))

g12 <- ggplot() + 
  geom_tile(data=contourdata, aes(x=x, y=y, fill=z, colour=z)) + 
  geom_contour(data=contourdata, aes(x=x, y=y, z=z), colour="black", size=.5) +
  scale_fill_continuous(low="#56B1F7", high="#132B43", trans="log") +
  scale_colour_continuous(low="#56B1F7", high="#132B43", trans="log") +
  ggtitle("geom_tile + geom_contour") 
g12
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7, g8=g8, g9=g9, g10=g10, g11=g11, g12=g12)) 

library("MASS")
data(geyser,package="MASS")
g13 <- ggplot() +  
  geom_point(data=geyser, aes(x = duration, y = waiting)) + 
  geom_contour(data=geyser, aes(x = duration, y = waiting), colour="blue", size=.5, stat="density2d") + 
  xlim(0.5, 6) + scale_y_log10() +
  ggtitle("geom_contour 2d density")
g13
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7, g8=g8, g9=g9, g10=g10, g11=g11, g12=g12, g13=g13))
# geom_point disappears because it does not get transformed.

g14 <- ggplot() +  xlim(0.5, 6) + scale_y_log10() +
  geom_polygon(data=geyser,aes(x=duration, y=waiting, fill=..level.., 
                               colour=..level.., group=..piece..), 
               stat="density2d", alpha=.5) +
  geom_point(data=geyser, aes(x = duration, y = waiting)) + 
  scale_fill_continuous(low="#56B1F7", high="#132B43", trans="log") +
  xlim(0.5, 6) + ylim(40, 110) +
  ggtitle("geom_density2d polygon")
g14
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7, g8=g8, g9=g9, g10=g10, g11=g11, g12=g12, g13=g13, g14=g14))


data(diamonds)
dsmall <- diamonds[sample(nrow(diamonds), 1000), ]
g15 <- ggplot() + xlim(c(1,3))+
  geom_tile(data=dsmall, aes(x=carat, y=price, fill=..density.., colour=..density..), stat="density2d", contour=FALSE) +
  scale_fill_gradient(limits=c(1e-5,8e-4), na.value="white") + 
  scale_colour_gradient(limits=c(1e-5,8e-4), na.value="white") +
  ggtitle("geom_density2d tile")
g15
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7, g8=g8, g9=g9, g10=g10, g11=g11, g12=g12, g13=g13, g14=g14, g15=g15))
  
g16 <- ggplot() + xlim(c(1,3))+
  geom_point(data=dsmall, aes(x=carat, y=price, size=..density..), stat="density2d", contour=FALSE) +
  ggtitle("geom_density2d points")
g16
gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7, g8=g8, g9=g9, g10=g10, g11=g11, g12=g12, g13=g13, g14=g14, g15=g15, g16=g16))
# doesn't work because statistics and transformations won't work with points

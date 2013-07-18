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
ribbondata <- rbind(cbind(ribbondata, group="low"), cbind(ribbondata, group="high"))
ribbondata[12:22,2:3] <- ribbondata[12:22,2:3]+1
g2 <- ggplot() + 
  geom_ribbon(data=ribbondata, aes(x=x, ymin=ymin, ymax=ymax, group=group, fill=group), alpha=.5) + 
  ggtitle("geom_ribbon")
g2
# gg2animint(list(g1=g1, g2=g2))

#' density: should show an exponential density curve in blue and a normal(ish) density curve in pink.
boxplotdata <- rbind(data.frame(x=1:50, y=sort(rnorm(50, 3, 1)), group="N(3,1)"),
                     data.frame(x=1:50, y=sort(rnorm(50, 0, 1)), group="N(0,1)"), 
                     data.frame(x=1:50, y=sort(rgamma(50, 2, 1/3)), group="Gamma(2,1/3)"))
boxplotdata <- ddply(boxplotdata, .(group), transform, ymax=max(y), ymin=min(y), med=median(y))

g3 <- ggplot() + geom_density(data=boxplotdata, aes(x=y, group=group, fill=group), alpha=.5) +
  scale_fill_discrete("Distribution") + xlab("x") + 
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
#' Boxplot does not work (7/5/13)
# g7 <- ggplot() + 
#   geom_boxplot(data=boxplotdata, aes(y=y, x=factor(group))) +
#   ggtitle("geom_boxplot")
# g7
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7))

g7 <- ggplot() + 
  geom_linerange(data=boxplotdata, aes(x=factor(group), ymax=ymax, ymin=ymin, colour=factor(group))) +
  ggtitle("geom_linerange") + scale_colour_discrete("Distribution") + xlab("Distribution")
g7
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7))

g8 <- ggplot() + 
  geom_histogram(data=subset(boxplotdata, group=="Gamma(2,1/3)"), aes(x=y, fill=..count..), binwidth=1) + 
  ggtitle("geom_histogram")
g8
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7, g8=g8))

g9 <- ggplot() + 
  geom_violin(data=boxplotdata, aes(x=group, y=y, fill=group, group=group)) +
  ggtitle("geom_violin")+ scale_fill_discrete("Distribution") + xlab("Distribution")
g9
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7, g8=g8, g9=g9))

#' Step Plot
#' Must specify group and then use colour=factor(group) to get desired effect.
g10 <- ggplot() + geom_step(data=boxplotdata, aes(x=x, y=y, colour=factor(group), group=group)) +
  scale_colour_discrete("Distribution") +
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

contourdata2 <- floor(contourdata/3)*3
g12 <- ggplot() + 
  geom_tile(data=contourdata2, aes(x=x, y=y, fill=z, colour=z)) + 
  geom_contour(data=contourdata, aes(x=x, y=y, z=z), colour="black", size=.5) +
  scale_fill_continuous("height", low="#56B1F7", high="#132B43", guide="legend") +
  scale_colour_continuous("height", low="#56B1F7", high="#132B43", guide="legend") +
  ggtitle("geom_tile + geom_contour") 
g12
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7, g8=g8, g9=g9, g10=g10, g11=g11, g12=g12)) 

library("MASS")
data(geyser,package="MASS")
g13 <- ggplot() +  
  geom_point(data=geyser, aes(x = duration, y = waiting)) + 
  geom_contour(data=geyser, aes(x = duration, y = waiting), colour="blue", size=.5, stat="density2d") + 
  xlim(0.5, 6) + scale_y_log10(limits=c(40,110)) +
  ggtitle("geom_contour 2d density")
g13
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7, g8=g8, g9=g9, g10=g10, g11=g11, g12=g12, g13=g13))
# geom_point disappears because it does not get transformed.

g14 <- ggplot() +  
  geom_polygon(data=geyser,aes(x=duration, y=waiting, fill=..level.., 
                               group=..piece..), 
               stat="density2d", alpha=.5) +
  geom_point(data=geyser, aes(x = duration, y = waiting)) + 
  scale_fill_continuous("Density Level", low="#56B1F7", high="#132B43") + 
  guides(colour = guide_legend(override.aes = list(alpha = 1)), 
         fill = guide_legend(override.aes = list(alpha = 1))) + 
  scale_y_continuous(limits=c(40,110), trans="log10") +
  scale_x_continuous(limits=c(.5, 6)) +
  ggtitle("geom_density2d polygon")
g14
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7, g8=g8, g9=g9, g10=g10, g11=g11, g12=g12, g13=g13, g14=g14))


data(diamonds)
dsmall <- diamonds[sample(nrow(diamonds), 1000), ]
g15 <- ggplot() + 
  geom_tile(data=dsmall, aes(x=carat, y=price, fill=..density.., colour=..density..), stat="density2d", contour=FALSE, n=30) +
  scale_fill_gradient(limits=c(1e-5,8e-4), na.value="white") + 
  scale_colour_gradient(limits=c(1e-5,8e-4), na.value="white") +
  ggtitle("geom_density2d tile") + ylim(c(0, 19000))
g15
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7, g8=g8, g9=g9, g10=g10, g11=g11, g12=g12, g13=g13, g14=g14, g15=g15))
  
g16 <- ggplot() + 
  geom_point(data=dsmall, aes(x=carat, y=price, alpha=..density..), 
             stat="density2d", contour=FALSE, n=10, size=I(1)) +
  scale_alpha_continuous("Density") +
  ggtitle("geom_density2d points")
g16
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7, g8=g8, g9=g9, g10=g10, g11=g11, g12=g12, g13=g13, g14=g14, g15=g15, g16=g16))


#' geom_map using geom_polygon and merge
crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
library(reshape2) # for melt
crimesm <- melt(crimes, id = 1)
library(maps)
states_map <- map_data("state")
assault.map <- merge(states_map, subset(crimesm, variable=="Assault"), by.x="region", by.y="state")
assault.map <- assault.map[order(assault.map$group, assault.map$order),]
g17 <- ggplot() + 
  geom_polygon(data=assault.map, aes(x=long, y=lat, group=group, fill=value, colour=value)) +
  expand_limits(x = states_map$long, y = states_map$lat) + 
  ggtitle("geom_polygon map") + ylim(c(12, 63)) + 
  geom_text(data=data.frame(x=-95.84, y=55, label="Arrests for Assault"), hjust=.5, aes(x=x, y=y, label=label))
g17  
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7, g8=g8, 
#                 g9=g9, g10=g10, g11=g11, g12=g12, g13=g13, g14=g14, g15=g15, 
#                 g16 = g16, g17=g17))

#' geom_bar stacked
data(mtcars)
g18 <- ggplot() + geom_bar(data=mtcars, aes(x=factor(cyl), fill=factor(vs))) + ggtitle("geom_bar stacked")
g18
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7, g8=g8, 
#                 g9=g9, g10=g10, g11=g11, g12=g12, g13=g13, g14=g14, g15=g15, 
#                 g16 = g16, g17=g17, g18=g18))

#' geom_area
data(diamonds)
g19 <- ggplot() + 
  geom_area(data=diamonds, aes(x=clarity, y=..count.., group=cut, colour=cut, fill=cut), stat="density") +
  ggtitle("geom_area")
g19
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7, g8=g8, 
#                 g9=g9, g10=g10, g11=g11, g12=g12, g13=g13, g14=g14, g15=g15, 
#                 g16 = g16, g17=g17, g18=g18, g19=g19))

g20 <- ggplot() + 
  geom_freqpoly(data=diamonds, aes(x=clarity, group=cut, colour=cut)) +
  ggtitle("geom_freqpoly")
g20
gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7, g8=g8, 
                g9=g9, g10=g10, g11=g11, g12=g12, g13=g13, g14=g14, g15=g15, 
                g16 = g16, g17=g17, g18=g18, g19=g19, g20=g20))

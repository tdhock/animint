library(animint)
library(ggplot2)
library(gridExtra)

#' Demonstrates axis specification, serves as a tutorial to introduce animint (eventually?)
# Randomly generate some data
scatterdata <- data.frame(x=rnorm(100, 50, 15))
scatterdata$y <- with(scatterdata, runif(100, x-5, x+5))
scatterdata$xnew <- round(scatterdata$x/20)*20
scatterdata$xnew <- as.factor(scatterdata$xnew)
scatterdata$class <- factor(round(scatterdata$x/10)%%2, labels=c("high", "low"))
scatterdata$class4 <- factor(round(scatterdata$x/10)%%4, labels=c("high", "medhigh", "medlow", "low"))


p <- qplot(data=scatterdata, x=x, y=y, geom="point")
p
gg2animint(list(p=p))
# This throws an error.
## TODO: qplot specification should be feasible... how to implement?

#' Must use empty ggplot() statement because of structure of ggplot/qplot object
#' Must provide a named list of ggplots.
s1 <- ggplot() + geom_point(data=scatterdata, aes(x=x,y=y))
s1
# gg2animint(list(s1=s1))

#' Colors, Demonstrates axis -- works with factor data
#' Specify colors using R color names works
s2 <- ggplot() + geom_point(data=scatterdata, aes(x=xnew, y=y), colour="blue")
s2
# gg2animint(list(s1=s1, s2=s2))

#' Specify colors manually using hex values works
s3 <- ggplot() + geom_point(data=scatterdata, aes(x=xnew, y=y, colour=class, fill=class)) + scale_colour_manual(values=c("#FF0000", "#0000FF")) + scale_fill_manual(values=c("#FF0000", "#0000FF"))
s3
# gg2animint(list(s1=s1, s2=s2, s3=s3))

#' Color by x axis
s4 <- ggplot() + geom_point(data=scatterdata, aes(x=xnew, y=y, colour=xnew, fill=xnew))
s4
# gg2animint(list(s1=s1, s2=s2, s3=s3, s4=s4))

#' Use geom_jitter and color by another variable
s5 <- ggplot() + geom_jitter(data=scatterdata, aes(x=xnew, y=y, colour=class4, fill=class4))
s5
# gg2animint(list(s1=s1, s2=s2, s3=s3, s4=s4, s5=s5))

#' Color by x*y axis (no binning)
s6 <- ggplot() + geom_point(data=scatterdata, aes(x=x, y=y, color=x*y, fill=x*y))
s6
# gg2animint(list(s1=s1, s2=s2, s3=s3, s4=s4, s5=s5, s6=s6))

#' Overplotting data for testing alpha
library(plyr)
scatterdata2 <- data.frame(x=rnorm(1000, 0, .5), y=rnorm(1000, 0, .5))
scatterdata2$quad <- with(scatterdata2, (3+sign(x)+2*sign(y))/2+1)
scatterdata2 <- ddply(scatterdata2, .(quad), transform, str=sqrt(x^2+y^2)/4)
qplot(data=scatterdata2, x=x, y=y, geom="point", colour=factor(quad), alpha=I(.2))

#' Single alpha value
s7 <- ggplot() + 
  geom_point(data=scatterdata2, aes(x=x, y=y, colour=factor(quad), fill=factor(quad)), alpha=.1)+ 
  geom_point(data=scatterdata2, aes(x=x, y=y, colour=factor(quad), fill=factor(quad), 
                                    clickSelects=quad, showSelected=quad), alpha=.2)
s7
# gg2animint(list(s1=s1, s2=s2, s3=s3, s4=s4, s5=s5, s6=s6, s7=s7))

#' Continuous alpha
s8 <- ggplot() + 
  geom_point(data=scatterdata2, aes(x=x, y=y, colour=factor(quad), fill=factor(quad)), alpha=.05) +
  geom_point(data=scatterdata2, aes(x=x, y=y, colour=factor(quad), fill=factor(quad), 
                                    alpha=str, clickSelects=quad, showSelected=quad))
s8
# gg2animint(list(s1=s1, s2=s2, s3=s3, s4=s4, s5=s5, s6=s6, s7=s7, s8=s8))

#' Categorical alpha and scale_alpha_discrete()
#' Note, to get unselected points to show up, need to have two copies of geom_point: One for anything that isn't selected, one for only the selected points.
s9 <- ggplot() + 
  geom_point(data=scatterdata2, aes(x=x, y=y, colour=factor(quad), fill=factor(quad)), alpha=.1) +
  geom_point(data=scatterdata2, aes(x=x, y=y, colour=factor(quad), fill=factor(quad), 
                                    alpha=factor(quad), clickSelects=quad, showSelected=quad)) + 
  scale_alpha_discrete(range=c(.05, .25))
s9
# gg2animint(list(s1=s1, s2=s2, s3=s3, s4=s4, s5=s5, s6=s6, s7=s7, s8=s8, s9=s9))

#' Interactive plots...
scatterdata2.summary <- ddply(scatterdata2, .(quad), summarise, xmin=min(x), xmax=max(x), ymin=min(y), ymax=max(y), xmean=mean(x), ymean=mean(y))
s10 <- ggplot() + 
  geom_rect(data=scatterdata2.summary, aes(xmax=xmax, xmin=xmin, ymax=ymax, ymin=ymin,
                                           colour=factor(quad)), fill="white", alpha=.5) +
  geom_rect(data=scatterdata2.summary, aes(xmax=xmax, xmin=xmin, ymax=ymax, ymin=ymin, 
                                           colour=factor(quad), fill=factor(quad),
                                           clickSelects = quad), alpha=.5) +
  geom_point(data=scatterdata2, aes(x=x, y=y, fill=factor(quad), colour=factor(quad)), alpha=.2) + 
  geom_point(data=scatterdata2.summary, aes(x=xmean, y=ymean, fill=factor(quad), showSelected = quad), colour="black", size=5)
s10

gg2animint(list(s1=s1, s2=s2, s3=s3, s4=s4, s5=s5, s6=s6, s7=s7, s8=s8, s9=s9, s10=s10))


#' Kmeans Implementation
#' 

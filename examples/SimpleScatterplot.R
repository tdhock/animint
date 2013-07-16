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

#' qplot-style specification works for simple examples, 
#' but may not continue to work for more complicated 
#' combinations... 
#' TODO: Test further.
p <- qplot(data=scatterdata, x=x, y=y, geom="point", colour=floor(x))
gg2animint(list(p=p))

#' Should use empty ggplot() statement because of structure of ggplot/qplot object
#' Must provide a named list of ggplots.
s1 <- ggplot() + geom_point(data=scatterdata, aes(x=x, y=y)) +
  xlab("very long x axis label") + 
  ylab("very long y axis label") +
  ggtitle("Titles are awesome")
s1
# gg2animint(list(s1=s1))

#' Colors, Demonstrates axis -- works with factor data
#' Specify colors using R color names
s2 <- ggplot() + geom_point(data=scatterdata, aes(x=xnew, y=y), colour="blue") +
  ggtitle("Colors are cool")
s2
# gg2animint(list(s1=s1, s2=s2))

#' Specify colors manually using hex values
s3 <- ggplot() + 
  geom_point(data=scatterdata, aes(x=xnew, y=y, colour=class, fill=class)) + 
  scale_colour_manual(values=c("#FF0000", "#0000FF")) + 
  scale_fill_manual(values=c("#FF0000", "#0000FF")) +
  ggtitle("Manual color/fill scales")
s3
# gg2animint(list(s1=s1, s2=s2, s3=s3))

#' Categorical color scales 
s4 <- ggplot() + geom_point(data=scatterdata, aes(x=xnew, y=y, colour=xnew, fill=xnew)) +
  ggtitle("Categorical color/fill scales")
s4
# gg2animint(list(s1=s1, s2=s2, s3=s3, s4=s4))

#' Use geom_jitter and color by another variable
s5 <- ggplot() + geom_jitter(data=scatterdata, aes(x=xnew, y=y, colour=class4, fill=class4)) +
  ggtitle("geom_jitter")
s5
# gg2animint(list(s1=s1, s2=s2, s3=s3, s4=s4, s5=s5))

#' Color by x*y axis (no binning)
s6 <- ggplot() + geom_point(data=scatterdata, aes(x=x, y=y, color=x*y, fill=x*y)) +
  ggtitle("Continuous color scales")
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
                                    clickSelects=quad, showSelected=quad), alpha=.2) +
  ggtitle("Constant alpha")
s7
# gg2animint(list(s1=s1, s2=s2, s3=s3, s4=s4, s5=s5, s6=s6, s7=s7))

#' Continuous alpha
s8 <- ggplot() + 
  geom_point(data=scatterdata2, aes(x=x, y=y, colour=factor(quad), fill=factor(quad)), alpha=.05) +
  geom_point(data=scatterdata2, aes(x=x, y=y, colour=factor(quad), fill=factor(quad), 
                                    alpha=str, clickSelects=quad, showSelected=quad)) +
  ggtitle("Continuous alpha")
s8
# gg2animint(list(s1=s1, s2=s2, s3=s3, s4=s4, s5=s5, s6=s6, s7=s7, s8=s8))

#' Categorical alpha and scale_alpha_discrete()
#' Note, to get unselected points to show up, need to have two copies of geom_point: One for anything that isn't selected, one for only the selected points.
s9 <- ggplot() + 
  geom_point(data=scatterdata2, aes(x=x, y=y, colour=factor(quad), fill=factor(quad)), alpha=.1) +
  geom_point(data=scatterdata2, aes(x=x, y=y, colour=factor(quad), fill=factor(quad), 
                                    alpha=factor(quad), clickSelects=quad, showSelected=quad)) + 
  scale_alpha_discrete(range=c(.05, .25)) +
  ggtitle("Discrete alpha")
s9
# gg2animint(list(s1=s1, s2=s2, s3=s3, s4=s4, s5=s5, s6=s6, s7=s7, s8=s8, s9=s9))

#' Interactive plots...
scatterdata2.summary <- ddply(scatterdata2, .(quad), summarise, xmin=min(x), xmax=max(x), ymin=min(y), ymax=max(y), xmean=mean(x), ymean=mean(y))
s10 <- ggplot() + 
  geom_rect(data=scatterdata2.summary, aes(xmax=xmax, xmin=xmin, ymax=ymax, ymin=ymin,
                                           colour=factor(quad)), fill="white", alpha=.2) +
  geom_rect(data=scatterdata2.summary, aes(xmax=xmax, xmin=xmin, ymax=ymax, ymin=ymin, 
                                           colour=factor(quad), fill=factor(quad),
                                           clickSelects = quad), alpha=.2) +
  geom_point(data=scatterdata2, aes(x=x, y=y, fill=factor(quad), colour=factor(quad)), alpha=.2) + 
  geom_point(data=scatterdata2.summary, aes(x=xmean, y=ymean, fill=factor(quad), showSelected = quad), colour="black", size=5) +
  ggtitle("Selects & Means")
s10

# gg2animint(list(s1=s1, s2=s2, s3=s3, s4=s4, s5=s5, s6=s6, s7=s7, s8=s8, s9=s9, s10=s10))


#' Point Size Scaling
#' Scale defaults to radius, but area is more easily interpreted by the brain (Tufte).
s11 <- ggplot() + 
  geom_point(data=scatterdata2, aes(x=x, y=y, colour=factor(quad), fill=factor(quad), size=str), alpha=.5) +
  geom_point(data=scatterdata2, aes(x=x, y=y, colour=factor(quad), fill=factor(quad), 
                                    size=str, clickSelects=quad, showSelected=quad), alpha=.3) +
  ggtitle("Scale Size")
s11

# gg2animint(list(s1=s1, s2=s2, s3=s3, s4=s4, s5=s5, s6=s6, s7=s7, s8=s8, s9=s9, s10=s10, s11=s11))

s12 <- ggplot() + 
  geom_point(data=scatterdata2, aes(x=x, y=y, colour=factor(quad), fill=factor(quad), size=str), alpha=.5) + 
  scale_size_area() +
  ggtitle("Scale Area")
s12

gg2animint(list(s1=s1, s2=s2, s3=s3, s4=s4, s5=s5, s6=s6, s7=s7, s8=s8, s9=s9, s10=s10, s11=s11, s12=s12))


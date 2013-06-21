library(animint)
library(ggplot2)

#' Demonstrates axis specification, serves as a tutorial to introduce animint (eventually?)
# Randomly generate some data
data <- data.frame(x=rnorm(100, 50, 15))
data$y <- with(data, runif(100, x-5, x+5))
data$xnew <- round(data$x/20)*20
data$xnew <- as.factor(data$xnew)
data$class <- factor(round(data$x/10)%%2, labels=c("high", "low"))
data$class4 <- factor(round(data$x/10)%%4, labels=c("high", "medhigh", "medlow", "low"))


p <- qplot(data=data, x=x, y=y, geom="point")
p
gg2animint(list(p=p))
# This throws an error.
## TODO: qplot specification should be feasible... how to implement?

#' Must use empty ggplot() statement because of structure of ggplot/qplot object
#' Must provide a named list of ggplots.
p1 <- ggplot() + geom_point(data=data, aes(x=x,y=y))
p1
# gg2animint(list(p1=p1))

#' Demonstrates axis -- works with factor data
p2 <- ggplot() + geom_point(data=data, aes(x=xnew, y=y))
p2
# gg2animint(list(p1=p1, p2=p2))


#' Colors
#' Specify colors using R color names works
p3 <- ggplot() + geom_point(data=data, aes(x=xnew, y=y), colour="blue")
p3
# gg2animint(list(p1=p1, p2=p2, p3=p3))

#' Specify colors manually using hex values works
p4 <- ggplot() + geom_point(data=data, aes(x=xnew, y=y, colour=class, fill=class)) + scale_colour_manual(values=c("#FF0000", "#0000FF")) + scale_fill_manual(values=c("#FF0000", "#0000FF"))
p4
# gg2animint(list(p1=p1, p2=p2, p3=p3, p4=p4))

#' Color by x axis
p5 <- ggplot() + geom_point(data=data, aes(x=xnew, y=y, colour=xnew, fill=xnew))
p5
# gg2animint(list(p1=p1, p2=p2, p3=p3, p4=p4, p5=p5))

#' Use geom_jitter and color by another variable
p6 <- ggplot() + geom_jitter(data=data, aes(x=xnew, y=y, colour=class4, fill=class4))
p6
# gg2animint(list(p1=p1, p2=p2, p3=p3, p4=p4, p5=p5, p6=p6))

#' Color by x*y axis (no binning)
p7 <- ggplot() + geom_point(data=data, aes(x=x, y=y, color=x*y, fill=x*y))
p7
gg2animint(list(p1=p1, p2=p2, p3=p3, p4=p4, p5=p5, p6=p6, p7=p7))

#' Overplotting data for testing alpha
data2 <- data.frame(x=rnorm(1000, 0, .5), y=rnorm(1000, 0, .5))
data2$str <- with(data2, x*y)
data2$quad <- with(data2, (3+sign(x)+2*sign(y))/2+1)
qplot(data=data2, x=x, y=y, geom="point", colour=factor(quad), alpha=I(.2))

#' Single alpha value
p8 <- ggplot() + geom_point(data=data2, aes(x=x, y=y, colour=factor(quad), fill=factor(quad)), alpha=.1)
p8
gg2animint(list(p1=p1, p2=p2, p3=p3, p4=p4, p5=p5, p6=p6, p7=p7, p8=p8))

#' Continuous alpha
p9 <- ggplot() + geom_point(data=data2, aes(x=x, y=y, colour=factor(quad), fill=factor(quad), alpha=abs(str)/abs(max(str))))
p9
gg2animint(list(p1=p1, p2=p2, p3=p3, p4=p4, p5=p5, p6=p6, p7=p7, p8=p8, p9=p9))

#' Categorical alpha and scale_alpha_discrete()
p10 <- ggplot() + geom_point(data=data2, aes(x=x, y=y, colour=factor(quad), fill=factor(quad), alpha=factor(quad))) + scale_alpha_discrete(range=c(.1, .5))
p10
gg2animint(list(p1=p1, p2=p2, p3=p3, p4=p4, p5=p5, p6=p6, p7=p7, p8=p8, p9=p9, p10=p10))
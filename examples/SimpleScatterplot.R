library(animint)
library(ggplot2)

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

#' Demonstrates axis -- works with factor data
s2 <- ggplot() + geom_point(data=scatterdata, aes(x=xnew, y=y))
s2
# gg2animint(list(s1=s1, s2=s2))


#' Colors
#' Specify colors using R color names works
s3 <- ggplot() + geom_point(data=scatterdata, aes(x=xnew, y=y), colour="blue")
s3
# gg2animint(list(s1=s1, s2=s2, s3=s3))

#' Specify colors manually using hex values works
s4 <- ggplot() + geom_point(data=scatterdata, aes(x=xnew, y=y, colour=class, fill=class)) + scale_colour_manual(values=c("#FF0000", "#0000FF")) + scale_fill_manual(values=c("#FF0000", "#0000FF"))
s4
# gg2animint(list(s1=s1, s2=s2, s3=s3, s4=s4))

#' Color by x axis
s5 <- ggplot() + geom_point(data=scatterdata, aes(x=xnew, y=y, colour=xnew, fill=xnew))
s5
# gg2animint(list(s1=s1, s2=s2, s3=s3, s4=s4, s5=s5))

#' Use geom_jitter and color by another variable
s6 <- ggplot() + geom_jitter(data=scatterdata, aes(x=xnew, y=y, colour=class4, fill=class4))
s6
# gg2animint(list(s1=s1, s2=s2, s3=s3, s4=s4, s5=s5, s6=s6))

#' Color by x*y axis (no binning)
s7 <- ggplot() + geom_point(data=scatterdata, aes(x=x, y=y, color=x*y, fill=x*y))
s7
# gg2animint(list(s1=s1, s2=s2, s3=s3, s4=s4, s5=s5, s6=s6, s7=s7))

#' Overplotting data for testing alpha
scatterdata2 <- data.frame(x=rnorm(1000, 0, .5), y=rnorm(1000, 0, .5))
scatterdata2$str <- with(scatterdata2, x*y)
scatterdata2$quad <- with(scatterdata2, (3+sign(x)+2*sign(y))/2+1)
qplot(data=scatterdata2, x=x, y=y, geom="point", colour=factor(quad), alpha=I(.2))

#' Single alpha value
s8 <- ggplot() + geom_point(data=scatterdata2, aes(x=x, y=y, colour=factor(quad), fill=factor(quad)), alpha=.1)
s8
# gg2animint(list(s1=s1, s2=s2, s3=s3, s4=s4, s5=s5, s6=s6, s7=s7, s8=s8))

#' Continuous alpha
s9 <- ggplot() + geom_point(data=scatterdata2, aes(x=x, y=y, colour=factor(quad), fill=factor(quad), alpha=abs(str)/abs(max(str))))
s9
# gg2animint(list(s1=s1, s2=s2, s3=s3, s4=s4, s5=s5, s6=s6, s7=s7, s8=s8, s9=s9))

#' Categorical alpha and scale_alpha_discrete()
s10 <- ggplot() + geom_point(data=scatterdata2, aes(x=x, y=y, colour=factor(quad), fill=factor(quad), alpha=factor(quad))) + scale_alpha_discrete(range=c(.1, .5))
s10
gg2animint(list(s1=s1, s2=s2, s3=s3, s4=s4, s5=s5, s6=s6, s7=s7, s8=s8, s9=s9, s10=s10))
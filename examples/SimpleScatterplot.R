library(animint)
library(ggplot2)

#' Demonstrates axis specification, serves as a tutorial to introduce animint (eventually?)
# Randomly generate some data
data <- data.frame(x=rnorm(100, 50, 15))
data$y <- with(data, runif(100, x-5, x+5))

p <- qplot(data=data, x=x, y=y, geom="point")
p
# This throws an error.
## TODO: qplot specification should be feasible... how to implement?

# Must use empty ggplot() statement because of structure of ggplot/qplot object
splot <- ggplot() + geom_point(data=data, aes(x=x, y=y)) #+ geom_smooth(data=data, aes(x=x, y=y))

# Must provide a named list of ggplots.
gg2animint(list(p1 = ggplot() + geom_point(data=data, aes(x=x, y=y))), out.dir="./junk", open.browser=FALSE)

#' Demonstrates axis -- works with factor data
data$xnew <- round(data$x/20)*20
data$xnew <- as.factor(data$xnew)

p <- ggplot() + geom_point(data=data, aes(x=xnew, y=y))
p
gg2animint(list(p1 = ggplot() + geom_point(data=data, aes(x=xnew, y=y))), out.dir="./junk", open.browser=FALSE)


#' Colors?
data$class <- factor(round(data$x/10)%%2, labels=c("high", "low"))
data$class4 <- factor(round(data$x/10)%%4, labels=c("high", "medhigh", "medlow", "low"))

p <- ggplot() + geom_point(data=data, aes(x=xnew, y=y), colour="blue")
p
gg2animint(list(p1 = ggplot() + geom_point(data=data, aes(x=xnew, y=y), colour="blue", fill="blue")), out.dir="./junk", open.browser=FALSE)

p <- ggplot() + geom_point(data=data, aes(x=xnew, y=y, colour=class, fill=class)) + scale_colour_manual(values=c("#FF0000", "#0000FF")) + scale_fill_manual(values=c("#FF0000", "#0000FF"))
p
gg2animint(list(p1 = p), out.dir="./junk", open.browser=FALSE)

p <- ggplot() + geom_point(data=data, aes(x=xnew, y=y, colour=xnew, fill=xnew))
p
gg2animint(list(p1 = p), out.dir="./junk", open.browser=FALSE)

p <- ggplot() + geom_jitter(data=data, aes(x=xnew, y=y, colour=class4, fill=class4))
p
gg2animint(list(p1 = p), out.dir="./junk", open.browser=FALSE)
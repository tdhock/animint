library(animint)
library(ggplot2)

#' Randomly generate some data
data <- data.frame(x=rnorm(100, 0, 1))
data$y <- with(data, runif(100, x-.5, x+.5))

qplot(data=data, x=x, y=y, geom="point")
#' Must use empty ggplot() statement because of structure of ggplot/qplot object
p <- ggplot() + geom_point(data=data, aes(x=x, y=y))

#' Must provide a named list of ggplots.
gg2animint(list(p1 = ggplot() + geom_point(data=data, aes(x=x, y=y))), out.dir="./junk/", open.browser=FALSE)
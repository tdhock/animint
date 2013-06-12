library(animint)
library(ggplot2)

#' Demonstrates line plots in animint, debugging, serves as an example

# Generate data
data <- data.frame(x=rep(1:10, times=5), group = rep(1:5, each=10))
data$group <- as.factor(data$group)
data$y <- rnorm(length(data$x), data$x, .5) + rep(rnorm(5, 0, 2), each=10)

# Simple line plot
p <- ggplot() + geom_line(data=data, aes(x=x, y=y, group=group))
p
gg2animint(list(p1 = p), out.dir="./junk", open.browser=FALSE)


# Simple line plot with colors...
p <- ggplot() + geom_line(data=data, aes(x=x, y=y, colour=group, group=group))
p
gg2animint(list(p1 = p), out.dir="./junk", open.browser=FALSE)
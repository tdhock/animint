# two.selectors.animated <- {
#   list(ts=ggplot()+
#          geom_vline(aes(xintercept=generation,
#                         clickSelects=generation),
#                     data=generations, alpha=1/2, lwd=4)+
#          geom_line(aes(generation, frequency, group=population,
#                        showSelected=locus), data=generation.loci),
#        predictions=ggplot()+
#          geom_point(aes(ancestral, estimated, showSelected=generation,
#                         clickSelects=locus),
#                     data=generation.pop, size=4, alpha=3/4),
#        loci=ggplot()+
#          geom_vline(aes(xintercept=locus, clickSelects=locus),
#                     data=loci, alpha=1/2, lwd=4)+
#          geom_point(aes(locus, frequency, showSelected=generation),
#                     data=generation.loci),
#        duration=list(generation=1000),
#        time=list(variable="generation",ms=2000))
# }
# gg2animint(two.selectors.animated)

data <- expand.grid(a=seq(0, 1, .005), b=seq(-10, 0, 1))
data$anew <- rnorm(2211, data$a, .001)
data$c <- with(data, rnorm(2211, 0, .1) + a*rep(rnorm(11, 5, .1), each=201) + b)
data$d <- with(data, rnorm(2211, 0, .1) - rep(rnorm(11, seq(-10, 0, 1), .1), each=201)*a + rep(runif(11, -1, 1), each=201))
qplot(data=data, x=a, y=c, group=factor(b), geom="line")
qplot(data=data, x=a, y=d, group=factor(b), geom="line")

attemptanimation <- {
  list(ts=ggplot() + geom_line(data=data, aes(x=a, y=c, group=b, clickSelects=b)),
       bs=ggplot() + geom_line(data=data, aes(x=anew, y=d, group=b, clickSelects=b)),
       is=ggplot() + geom_hline(data=data, aes(x=a, yintercept=b, showSelected=b), alpha=1/2)
       )
}
gg2animint(attemptanimation, out.dir="./junk/", open.browser=FALSE)


data <- expand.grid(width=seq(1, 3, 1)+rnorm(3, 0, .5), height=seq(1, 3, 1)+rnorm(3, 0, .5))

data$idx <- 1:nrow(data)
data$xstart <- runif(nrow(data), -10, 10)
data$ystart <- runif(nrow(data), -10, 10)
data$xend <- data$width+data$xstart
data$yend <- data$height+data$ystart
data$area <- data$width*data$height

bigdata <- expand.grid(width=abs(seq(0, 3, .1) + rnorm(31, 0, .5)), height=abs(seq(0, 3, .1) + rnorm(31, 0, .5)))
bigdata$xstart <- runif(nrow(bigdata), -10, 10)
bigdata$ystart <- runif(nrow(bigdata), -10, 10)
bigdata$xend <- bigdata$width+bigdata$xstart
bigdata$yend <- bigdata$height+bigdata$ystart
bigdata$area <- bigdata$width*bigdata$height

# This version works (no interactivity)

# areadens <- data.frame(area=seq(0, 15, 1))
# areadens$p <- predict(loess(sapply(areadens$area, function(i) sum(data$area<=i)/length(data$area))~areadens$area))
# ggplot() + geom_rect(data=data, aes(xmin=xstart, ymin=ystart, xmax=xend, ymax=yend), alpha=.5, colour="blue", fill="white")
# ggplot() + geom_line(data=areadens, aes(x=area, y=p)) + geom_vline(data=data, aes(xintercept=area))
# attemptanimation <- {
#   list(ts=ggplot() + geom_rect(data=data, aes(xmin=xstart, ymin=ystart, xmax=xend, ymax=yend), alpha=.2, color="blue", fill="lightblue"),
#        cdf = ggplot() + geom_line(data=areadens, aes(x=area, y=p)) + geom_vline(data=data, aes(xintercept=area))
#   )
# }
# gg2animint(attemptanimation, out.dir="./junk/", open.browser=FALSE)

# This version doesn't work (attempt at interactivity)

areadens <- data.frame(area=seq(0, 15, .1))
areamodel <- loess(sapply(areadens$area, function(i) sum(bigdata$area<=i)/length(bigdata$area))~areadens$area)
data$areaprob <- sapply(predict(areamodel, newdata=data$area), function(x) min(1, x)) # prevent cdf from going over 1
areadens$p <- sapply(predict(areamodel), function(x) min(1, x))
ggplot() + geom_rect(data=data, aes(xmin=xstart, ymin=ystart, xmax=xend, ymax=yend, group=area), alpha=.5, colour="blue", fill="white")
ggplot() + geom_line(data=areadens, aes(x=area, y=p)) + geom_vline(data=data, aes(xintercept=area))

attemptanimation <- {
  )
}
gg2animint(attemptanimation, out.dir="./junk/", open.browser=FALSE)

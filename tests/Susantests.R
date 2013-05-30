library(ggplot2)
library(animint)
library(animation)
library(plyr)

# -------- Line Intercepts - demonstrates geom_hline() implementation --------
data <- expand.grid(a=seq(0, 1, .005), b=seq(-10, 0, 1))
data$anew <- rnorm(2211, data$a, .001)
data$c <- with(data, rnorm(2211, 0, .1) + a*rep(rnorm(11, 5, .1), each=201) + b)
data$d <- with(data, rnorm(2211, 0, .1) - rep(rnorm(11, seq(-10, 0, 1), .1), each=201)*a + rep(runif(11, -1, 1), each=201))
qplot(data=data, x=a, y=c, group=factor(b), geom="line")
qplot(data=data, x=a, y=d, group=factor(b), geom="line")

attemptanimation <- {
  list(ts=ggplot() + geom_line(data=data, aes(x=a, y=c, group=b, clickSelects=b)),
       is=ggplot() + geom_hline(data=data, aes(x=a, yintercept=b, showSelected=b), alpha=1/2)
       )
}
gg2animint(attemptanimation, out.dir="./junk/", open.browser=FALSE)

# -------- Area of rectangles - demonstrates geom_rect, geom_vline, --------
#          and geom_segment implementation.

# generate rectangles
data <- expand.grid(width=seq(1, 3, 1)+rnorm(3, 0, .5), height=seq(1, 3, 1)+rnorm(3, 0, .5))

data$idx <- 1:nrow(data)
data$xstart <- runif(nrow(data), -10, 10)
data$ystart <- runif(nrow(data), -10, 10)
data$xend <- data$width+data$xstart
data$yend <- data$height+data$ystart
data$area <- data$width*data$height

# generate a bunch of rectangles - used for more continuous CDF curve...
bigdata <- expand.grid(width=abs(seq(0, 3, .1) + rnorm(31, 0, .5)), height=abs(seq(0, 3, .1) + rnorm(31, 0, .5)))
bigdata$xstart <- runif(nrow(bigdata), -10, 10)
bigdata$ystart <- runif(nrow(bigdata), -10, 10)
bigdata$xend <- bigdata$width+bigdata$xstart
bigdata$yend <- bigdata$height+bigdata$ystart
bigdata$area <- bigdata$width*bigdata$height

areadens <- data.frame(area=seq(0, 15, .1))
areamodel <- loess(sapply(areadens$area, function(i) sum(bigdata$area<=i)/length(bigdata$area))~areadens$area)
data$areaprob <- sapply(predict(areamodel, newdata=data$area), function(x) min(1, x)) # prevent cdf from going over 1
areadens$p <- sapply(predict(areamodel), function(x) min(1, x))
areadens$p[which(areadens$p>=1)[1]:nrow(areadens)] <- 1 # make cdf nondecreasing...
ggplot() + geom_rect(data=data, aes(xmin=xstart, ymin=ystart, xmax=xend, ymax=yend, group=area), color="blue", fill="lightblue")
ggplot() + geom_line(data=areadens, aes(x=area, y=p)) + geom_vline(data=data, aes(xintercept=area))

attemptanimation <- {
  list(ts=ggplot() + geom_rect(data=data, aes(xmin=xstart, ymin=ystart, xmax=xend, ymax=yend, clickSelects=area, onSelected=area)),
       cdf = ggplot() + geom_line(data=areadens, aes(x=area, y=p)) + geom_vline(data=data, aes(xintercept=area, onSelected=area, clickSelects=area)),
       cdf2 = ggplot() + geom_line(data=areadens, aes(x=area, y=p)) + 
         geom_segment(data=data, aes(x=area, xend=area, y=0*areaprob, yend=areaprob, clickSelects=area, onSelected=area))+
         geom_segment(data=data, aes(x=0*area, xend=area, y=areaprob, yend=areaprob, clickSelects=area, onSelected=area))
  )
}
gg2animint(attemptanimation, out.dir="./junk/", open.browser=FALSE)

# -------- Animation Example --------
# right now, color is passed in to the json file as a scale but 
# somehow the individual points don't get a color attribute. Still working
# on debugging that little problem...
#
# Once colors are available, then I can animate the sequence - right now
# animation would be somewhat pointless, since the colors are what would
# ideally change!

sd <- .35
data <- rbind(data.frame(x=rnorm(100, 0, sd), y=rnorm(100, 0, sd)),
              data.frame(x=rnorm(100, 1, sd), y=rnorm(100, 1, sd)),
              data.frame(x=rnorm(100, -1, sd), y=rnorm(100, -1, sd)),
              data.frame(x=rnorm(100, -1, sd), y=rnorm(100, 1, sd)),
              data.frame(x=rnorm(100, 1, sd), y=rnorm(100, -1, sd)))
qplot(data=data, x=x, y=y) # 5 clusters

centers <- 5
ocluster <- sample(nrow(data), centers, replace=TRUE)
centers <- data[ocluster,]

get.groups <- function(centers, data){
  as.numeric(apply(data, 1, function(i) which.min(apply(centers, 1, function(j) sum((i-j)^2)))))
}

datastep <- NULL
for(i in 1:10){
  datatmp <- cbind(data, step=i, group=as.character(get.groups(centers, data)))
  datatmp$group.x <- centers[datatmp$group,1]
  datatmp$group.y <- centers[datatmp$group,2]
  centers <- ddply(datatmp, .(group), summarise, x=mean(x), y=mean(y))[,2:3]
  
  datastep <- rbind(datastep, datatmp)
}

qplot(data=datastep, x=x, y=y, geom="point", colour=group) + 
  scale_colour_manual(values=rainbow(5)) + 
  facet_wrap(~step) # facets are what would ordinarily be animated...

ssq <- ddply(datastep, .(group,step), summarise, sswithin = sum((x-group.x)^2+(y-group.y)^2))
ssq <- merge(ssq, ddply(datastep, .(step), summarise, ssbetween = sum((group.x-mean(group.x))^2+(group.y-mean(group.y))^2)))
ssq <- ddply(ssq, .(step), summarise, sswithin=sum(sswithin), ssbetween=unique(ssbetween))

# qplot(data=ssq, x=step, y=ssbetween, geom="line") + geom_line(data=ssq, aes(x=step, y=sswithin), linetype=2) 

colscale <- c("1" = rainbow(5)[1], "2" = rainbow(5)[2], "3" = rainbow(5)[3], "4" = rainbow(5)[4], "5" = rainbow(5)[5])

kmeans.animation <- {
  list(p1=ggplot()+geom_point(aes(x, y, colour=group, fill=group), #, showSelected=group, clickSelects=group),
                              data=subset(datastep, step==1), size=4) +
         scale_colour_manual(values=colscale, na.value="#000000") + 
         scale_fill_manual(values=colscale, na.value="#000000")
  )
}

gg2animint(kmeans.animation, out.dir="./junk/", open.browser=FALSE)

# -------- This is the working example from the package... --------
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
# --------

# --------
# This section is a duplication of the previous section - I was trying to see
# if geom_segment worked better than geom_point since the circle javascript
# implementation means that color (in R) should be mapped to fill (in JS) - 
# I was trying to remove that ambiguity...
sd <- .2
n <- 100
data <- data.frame(x=rnorm(n, 1:5, sd), y=rnorm(n, 1:5, sd))
data$yend <- data$y+rnorm(n, 1, .05)

qplot(data=data, x=x, xend=x, y=y, yend=yend, geom="segment", alpha=I(.5)) # 5 clusters

centers <- 5
ocluster <- sample(nrow(data), centers, replace=TRUE)
centers <- data[ocluster,]

get.groups <- function(centers, data){
  as.numeric(apply(data, 1, function(i) which.min(apply(centers, 1, function(j) sum((i-j)^2)))))
}

datastep <- NULL
for(i in 1:30){
  datatmp <- cbind(data, step=i, group=get.groups(centers, data))
  datatmp$group.x <- centers[datatmp$group,1]
  datatmp$group.y <- centers[datatmp$group,2]
  datatmp$group.yend <- centers[datatmp$group,3]
  centers <- ddply(datatmp, .(group), summarise, x=mean(x), y=mean(y), yend=mean(yend))[,2:4]
  datastep <- rbind(datastep, datatmp)
}

qplot(data=datastep, x=x, xend=x, y=y, yend=yend, geom="segment", colour=factor(group)) + 
  scale_colour_manual(values=rainbow(5)) + 
  facet_wrap(~step) # facets are what would ordinarily be animated...

ssq <- ddply(datastep, .(group,step), summarise, sswithin = sum((x-group.x)^2+(y-group.y)^2+(yend-group.yend)^2))
ssq <- merge(ssq, ddply(datastep, .(step), summarise, ssbetween = sum((group.x-mean(group.x))^2+(group.y-mean(group.y))^2 + (group.yend-mean(group.yend))^2)))
ssq <- ddply(ssq, .(step), summarise, sswithin=sum(sswithin), ssbetween=unique(ssbetween))

qplot(data=ssq, x=step, y=sswithin, geom="line") + geom_line(data=ssq, aes(x=step, y=ssbetween), linetype=2) + geom_vline(data=ssq, aes(xintercept=step))

kmeans.animation <- {
  list(p1=ggplot() + geom_segment(data=datastep, aes(x=x, xend=x, y=y, yend=yend, colour=factor(group))) + 
         scale_colour_manual(values=rainbow(5)))
  )
}

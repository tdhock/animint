#' Tests for each geom

xydata <- data.frame(x=sort(runif(50, 0, 10)))
xydata$y <- 3+2*xydata$x + rnorm(50, 0, 1)
p1 <- ggplot() + geom_point(data=xydata, aes(x=x, y=y)) + 
  geom_abline(data=data.frame(intercept=c(3, 0), slope=c(2,1)), aes(intercept=intercept, slope=slope))
p1
# gg2animint(list(p1=p1))

ribbondata <- data.frame(x=seq(0, 1, .1), ymin=runif(11, 0, 1), ymax=runif(11, 1, 2))
ribbondata <- rbind(cbind(ribbondata, group=1), cbind(ribbondata, group=2))
ribbondata[12:22,2:3] <- ribbondata[12:22,2:3]+1
p2 <- ggplot() + geom_ribbon(data=ribbondata, aes(x=x, ymin=ymin, ymax=ymax, group=group, fill=group), alpha=.5)
p2
gg2animint(list(p1=p1, p2=p2))
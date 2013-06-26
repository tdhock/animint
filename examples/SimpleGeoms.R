#' Tests for each geom

xydata <- data.frame(x=sort(runif(50, 0, 10)))
xydata$y <- 3+2*xydata$x + rnorm(50, 0, 1)
g1 <- ggplot() + geom_point(data=xydata, aes(x=x, y=y)) + 
  geom_abline(data=data.frame(intercept=c(3, 0), slope=c(2,1)), aes(intercept=intercept, slope=slope))
g1
# gg2animint(list(g1=g1))

ribbondata <- data.frame(x=seq(0, 1, .1), ymin=runif(11, 0, 1), ymax=runif(11, 1, 2))
ribbondata <- rbind(cbind(ribbondata, group=1), cbind(ribbondata, group=2))
ribbondata[12:22,2:3] <- ribbondata[12:22,2:3]+1
g2 <- ggplot() + geom_ribbon(data=ribbondata, aes(x=x, ymin=ymin, ymax=ymax, group=group, fill=group), alpha=.5)
g2
# gg2animint(list(g1=g1, g2=g2))

densdata <- data.frame(x=c(rnorm(100), rexp(100)), group=rep(1:2, each=100))
g3 <- ggplot() + geom_density(data=densdata, aes(x=x, group=group, fill=factor(group)), alpha=.5)
g3
gg2animint(list(g1=g1, g2=g2, g3=g3))

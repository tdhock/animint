createSine <- function(n=200, len=1, f=f, fprime=fprime, a=0, b=2*pi) {
  x <- seq(a, b, length=n+2)[(2:(n+1))]
  ell <- rep(len, length=length(x))
  fx <- f(x)
  ystart <- fx - .5*ell
  yend <- fx + .5*ell
  
  # now correct for line illusion in vertical direction
  dy <- diff(range(fx))
  dx <- diff(range(x))
  
  # fprime works in framework of dx and dy, but we represent it in framework of dx and dy+len
  # needs to be fixed by factor a:  
  a <- dy/(dy + len) 
  
  # ellx is based on the "trig" correction
  # ellx <- ell / cos(atan(abs(a*fprime(x))))
  ellx <- ell * sqrt(1 + a^2*fprime(x)^2)

  dframe <- data.frame(x=x, xstart=x, xend=x, y=fx, ystart=ystart, yend=yend, ell=ell, ellx = ellx)

  
  dframe
}


data <- createSine(n=60, f=sin, fprime=cos)

ggplot() + geom_segment(data=data, aes(x=xstart, y=ystart, xend=xend, yend=yend)) + 
  geom_text(data=data, aes(x=xstart, y=1.75, label=paste("l = ", ell)))
ggplot() + geom_segment(data=data, aes(x=xstart, y=y+ellx/2, xend=xend, yend=y-ellx/2)) + 
  geom_text(data=data, aes(x=xstart, y=1.75, label=paste("l = ", round(ellx, 2))))


data$labheight <- .25+max(c(data$ystart, data$y+data$ellx/2))
data$ell.label <- paste("l = ", data$ell)
data$ellx.label <- paste("l = ", round(data$ellx,2))
attemptanimation <- {
  list(orig = ggplot() + geom_segment(data=data, aes(x=xstart, y=ystart, xend=xend, yend=yend), alpha=.3) + 
         geom_segment(data=data, aes(x=xstart, y=ystart, xend=xend, yend=yend, clickSelects=ell, showSelected=ell)) + 
         geom_text(data=data, aes(x=xstart, y=labheight, label=ell.label, showSelected=ell)),
       corrected = ggplot() + geom_segment(data=data, aes(x=xstart, y=y+ellx/2, xend=xend, yend=y-ellx/2), alpha=.3) +
         geom_segment(data=data, aes(x=xstart, y=y+ellx/2, xend=xend, yend=y-ellx/2, clickSelects=ell, showSelected=ell)) + 
         geom_text(data=data, aes(x=xstart, y=labheight, label=ellx.label, showSelected=ellx))
  )
}
gg2animint(attemptanimation, out.dir="./junk/", open.browser=FALSE)

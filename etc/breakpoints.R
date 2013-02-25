## Make some simulated signals that show the breakpointError.
works_with_R("2.15.2",breakpointError="1.0")

kmax <- 20
sample.signal <- function(d){
  position <- round(seq(1,length(mu),l=d))
  last <- as.integer(position[length(position)])
  this.signal <- signal[position]
  result <- run.cghseg(this.signal,position,kmax)
  result$bases.per.probe <- factor(round(last/d))
  result$position <- position
  result$signal <- this.signal
  result$components <- errorComponents(result$breaks, mu.break.after, last)
  result$details <- lapply(result$breaks, errorDetails, mu.break.after, last)
  result
}
  
set.seed(1)
seg.size <- c(1,2,1,1,2,1)*1e5
means <- c(-1,0,1,0,1,0)
seg.df <- data.frame()
mu <- c()
first.base <- 1
for(i in seq_along(means)){
  N <- seg.size[i]
  seg.mu <- means[i]
  mu <- c(mu,rep(seg.mu,N))
  last.base <- first.base+N-1
  seg.df <- rbind(seg.df,data.frame(first.base,last.base,seg.mu))
  first.base <- last.base+1
}
mu.break.after <- which(diff(mu)!=0)
signal <- rnorm(length(mu), mu, 1)
## here we define the size of the signals.
variable.density.signals <- list()
signal.size <- c(1000,50)*length(means)
n.signals <- length(signal.size)

## we need to recover these data for each signal:
breakpoints <- list()
for(sig.i in 1:n.signals){
  cat(sprintf("simulating signal %4d / %4d\n",sig.i,n.signals))
  d <- signal.size[sig.i]

  sig <- sample.signal(d)
  bases.per.probe <- factor(round(max(sig$position)/length(sig$signal)))

  these <- list(error=sig$components,
                signals=with(sig,data.frame(position,signal)),
                breaks=sig$break.df,
                segments=sig$segments)
  for(N in names(these)){
    breakpoints[[N]] <- rbind(breakpoints[[N]],{
      data.frame(these[[N]],bases.per.probe)
    })
  }
}

yrange <- range(breakpoints$signals$signal)
details <- sig$details[[1]]
get.imp <- function(position,signal){
  data.frame(position,signal)
}
imp.df <- with(details,{
  rbind(get.imp(left,yrange[2]),
        get.imp(right,yrange[2]),
        get.imp(breaks,yrange[1]))
})
breakpoints$imprecision <- imp.df[order(imp.df$pos),]

save(breakpoints,file=file.path("..","data","breakpoints.RData"),compress="xz")

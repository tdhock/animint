library(animint)
data(generation.loci)

## Calculate vline data.frames.
generations <- data.frame(generation=unique(generation.loci$generation))
loci <- data.frame(locus=unique(generation.loci$locus))

## Calculate ancestral and estimated allele frequencies.
first <- subset(generation.loci,generation==1)
ancestral <- do.call(rbind,lapply(split(first,first$locus),with,{
  stopifnot(all(frequency==frequency[1]))
  data.frame(locus=locus[1],ancestral=frequency[1])
}))
gl.list <- split(generation.loci,with(generation.loci,list(generation,locus)))
generation.pop <- do.call(rbind,lapply(gl.list,with,{
  data.frame(generation=generation[1],locus=locus[1],estimated=mean(frequency))
}))
generation.pop$ancestral <- ancestral$ancestral[generation.pop$locus]

## Last generation data.
generation.loci.last <- subset(generation.loci,generation==max(generation))
generation.pop.last <- subset(generation.pop,generation==max(generation))

## only breakpointError.
data(breakpoints)
only.error <- subset(breakpoints$error,type=="E")
only.segments <- subset(only.error,bases.per.probe==bases.per.probe[1])

examples <-
  list(two.selectors.not.animated={
    list(ts=ggplot()+
         geom_vline(aes(xintercept=generation, clickSelects=generation),
                    data=generations, alpha=1/2, lwd=4)+
         geom_line(aes(generation, frequency, group=population,
                       showSelected=locus), data=generation.loci),
         loci=ggplot()+
         geom_vline(aes(xintercept=locus, clickSelects=locus, duration=1000),
                    data=loci, alpha=1/2, size=4)+
         geom_point(aes(locus, frequency, showSelected=generation),
                    data=generation.loci))
  },one.selector.not.animated={
    list(ts=ggplot()+
         geom_line(aes(generation, frequency, group=population,
                       showSelected=locus), data=generation.loci),
         predictions=ggplot()+
         geom_point(aes(ancestral, estimated, clickSelects=locus),
                    data=generation.pop.last, size=4, alpha=3/4),
         loci=ggplot()+
         geom_vline(aes(xintercept=locus, clickSelects=locus),
                    data=loci, alpha=1/2, lwd=4)+
         geom_point(aes(locus, frequency), data=generation.loci.last))
  },breakpointError={
    list(signal=ggplot()+
         geom_point(aes(position, signal, showSelected=bases.per.probe),
                    data=breakpoints$signals)+
         geom_line(aes(position, signal),
                   data=breakpoints$imprecision)+
         geom_segment(aes(first.base, mean, xend=last.base, yend=mean,
                          showSelected=segments,
                          showSelected2=bases.per.probe),
                      data=breakpoints$segments)+
         geom_vline(aes(xintercept=base, showSelected=segments),
                    data=breakpoints$breaks),
         error=ggplot()+
         geom_line(aes(segments, error, group=bases.per.probe),
                   data=only.error)+
         geom_vline(aes(xintercept=segments, clickSelects=segments),
                    data=only.segments, lwd=10))
  })

for(plot.list in examples){
  for(p in plot.list){
    ## we should be able to print these as regular, overplotted
    ## ggplots.
    print(p)
  }
  ## Attempt conversion.
  gg2animint(plot.list)
}

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

examples <-
  list(two.selectors.not.animated={
    list(ts=ggplot()+
         geom_vline(aes(xintercept=generation,clickSelects=generation),
                    data=generations,alpha=1/2)+
         geom_line(aes(generation,frequency,group=population,
                       showSelected=locus),data=generation.loci),
         loci=ggplot()+
         geom_vline(aes(xintercept=locus,clickSelects=locus),
                    data=loci,alpha=1/2)+
         geom_point(aes(locus,frequency,group=population,
                        showSelected=generation),data=generation.loci))
  },one.selector.not.animated={
    list(ts=ggplot()+
         geom_line(aes(generation,frequency,group=population,
                       showSelected=locus),data=generation.loci),
         predictions=ggplot()+
         geom_point(aes(ancestral,estimated,clickSelects=locus),
                    data=generation.pop.last),
         loci=ggplot()+
         geom_vline(aes(xintercept=locus,clickSelects=locus),
                    data=loci,alpha=1/2)+
         geom_point(aes(locus,frequency),data=generation.loci.last))
  })

## we should be able to print these as regular, overplotted ggplots.
for(plot.list in examples){
  for(p in plot.list){
    print(p)
  }
}

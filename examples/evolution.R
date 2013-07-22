library(animint)

data(generation.loci)
## Example: 2 plots, 2 selectors.
generations <- data.frame(generation=unique(generation.loci$generation))
loci <- data.frame(locus=unique(generation.loci$locus))
two.selectors.not.animated <- 
  list(ts=ggplot()+
         geom_vline(aes(xintercept=generation, clickSelects=generation),
                    data=generations, alpha=1/2, lwd=4)+
         geom_line(aes(generation, frequency, group=population,
                       showSelected=locus), data=generation.loci),
       loci=ggplot()+
         geom_vline(aes(xintercept=locus, clickSelects=locus),
                    data=loci, alpha=1/2, size=4)+
         geom_point(aes(locus, frequency, showSelected=generation),
                    data=generation.loci),
       duration=list(generation=1000)
  )
gg2animint(two.selectors.not.animated)

## Example: 2 plots, 2 selectors, with color legends.
colormap <- c(blue="blue",red="red",ancestral="black",neutral="grey30")
ancestral <- subset(generation.loci,population==1 & generation==1)
ancestral$color <- "ancestral"
two.selectors.color <- 
  list(ts=ggplot()+
       make_tallrect("generation", generation.loci)+
       geom_text(aes(generation,frequency,showSelected=locus,
                     label=sprintf("locus %d",locus)),
                 data=data.frame(loci,generation=50,frequency=1.05))+
       scale_colour_manual(values=colormap)+
       geom_line(aes(generation, frequency, group=population,
                     colour=color, showSelected=locus),
                 data=generation.loci)+
       geom_point(aes(generation, frequency, showSelected=locus),
                  data=ancestral),
       loci=ggplot()+
       make_tallrect("locus", generation.loci)+
       ## TODO: why do we have to specify color AND fill to get the
       ## points and legend to look right?
       scale_fill_manual(values=colormap)+
       scale_colour_manual(values=colormap)+
       geom_point(aes(locus, frequency, colour=color, fill=color,
                      showSelected=generation),
                  data=generation.loci, pch=21)+
       geom_point(aes(locus, frequency, colour=color, fill=color),
                  data=ancestral, pch=21)+
       geom_text(aes(locus,frequency,showSelected=generation,
                     label=sprintf("generation %d",generation)),
                 data=data.frame(generations,locus=35,frequency=1)),
       duration=list(generation=1000))
gg2animint(two.selectors.color)

## Example: 3 plots, 1 selector.
first <- subset(generation.loci,generation==1)
ancestral <- do.call(rbind,lapply(split(first,first$locus),with,{
  stopifnot(all(frequency==frequency[1]))
  data.frame(locus=locus[1],ancestral=frequency[1])
}))
gl.list <- split(generation.loci,
                 with(generation.loci,list(generation,locus)))
generation.pop <- do.call(rbind,lapply(gl.list,with,{
  data.frame(generation=generation[1], locus=locus[1],
             estimated=mean(frequency))
}))
generation.pop$ancestral <- ancestral$ancestral[generation.pop$locus]

## Calculate the subset for just the last generation, to plot.
generation.loci.last <- subset(generation.loci,generation==max(generation))
generation.pop.last <- subset(generation.pop,generation==max(generation))
one.selector.not.animated <- 
  list(ts=ggplot()+
         geom_line(aes(generation, frequency, group=population,
                       showSelected=locus), data=generation.loci),
       predictions=ggplot()+
         geom_point(aes(ancestral, estimated, clickSelects=locus),
                    data=generation.pop.last, size=4, alpha=3/4),
       loci=ggplot()+
         geom_vline(aes(xintercept=locus, clickSelects=locus),
                    data=loci, alpha=1/2, lwd=4)+
         geom_point(aes(locus, frequency), data=generation.loci.last)
  )
gg2animint(one.selector.not.animated)

## Example: animated time series with 3 plots and 2 selectors.
two.selectors.animated <- 
  list(ts=ggplot()+
         geom_vline(aes(xintercept=generation,
                        clickSelects=generation),
                    data=generations, alpha=1/2, lwd=4)+
         geom_line(aes(generation, frequency, group=population,
                       showSelected=locus), data=generation.loci),
       predictions=ggplot()+
         geom_point(aes(ancestral, estimated, showSelected=generation,
                        clickSelects=locus),
                    data=generation.pop, size=4, alpha=3/4),
       loci=ggplot()+
         geom_vline(aes(xintercept=locus, clickSelects=locus),
                    data=loci, alpha=1/2, lwd=4)+
         geom_point(aes(locus, frequency, showSelected=generation),
                    data=generation.loci),
       duration=list(generation=1000),
       time=list(variable="generation",ms=2000))
gg2animint(two.selectors.animated)


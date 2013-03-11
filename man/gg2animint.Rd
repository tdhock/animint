\name{gg2animint}
\alias{gg2animint}
\title{gg2animint}
\description{Convert a list of ggplots to an interactive animation.}
\usage{gg2animint(plot.list, out.dir = tempfile(), open.browser = interactive())}
\arguments{
  \item{plot.list}{List of named ggplots with showSelected and clickSelects
aesthetics.}
  \item{out.dir}{Directory to store html/js/csv files.}
  \item{open.browser}{Open a web browser?}
}

\value{An invisible copy of the R list that was exported to JSON.}

\author{Toby Dylan Hocking}




\examples{
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

## Example: 2 plots, 2 selectors, but only interacting with 1 plot.
data(breakpoints)
only.error <- subset(breakpoints$error,type=="E")
only.segments <- subset(only.error,bases.per.probe==bases.per.probe[1])
signal.colors <- c(estimate="#0adb0a",
                   latent="#0098ef")
breakpointError <- 
  list(signal=ggplot()+
       geom_point(aes(position, signal, showSelected=bases.per.probe),
                  data=breakpoints$signals)+
       geom_line(aes(position, signal), colour=signal.colors[["latent"]],
                 data=breakpoints$imprecision)+
       geom_segment(aes(first.base, mean, xend=last.base, yend=mean,
                        showSelected=segments,
                        showSelected2=bases.per.probe),
                    colour=signal.colors[["estimate"]],
                    data=breakpoints$segments)+
       geom_vline(aes(xintercept=base,
                      showSelected=segments,
                      showSelected2=bases.per.probe),
                  colour=signal.colors[["estimate"]],
                  linetype="dashed",
                  data=breakpoints$breaks),
       error=ggplot()+
       geom_vline(aes(xintercept=segments, clickSelects=segments),
                  data=only.segments, lwd=17, alpha=1/2)+
       geom_line(aes(segments, error, group=bases.per.probe,
                     clickSelects=bases.per.probe),
                 data=only.error, lwd=4))
gg2animint(breakpointError)

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
}

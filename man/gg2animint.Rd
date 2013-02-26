\name{gg2animint}
\alias{gg2animint}
\title{gg2animint}
\description{Convert a list of ggplots to an interactive animation.}
\usage{gg2animint(plot.list, out.dir = tempfile(), open.browser = interactive())}
\arguments{
  \item{plot.list}{List of ggplots with showSelected, time, and/or clickSelects aes.}
  \item{out.dir}{Directory to store html/js/csv files.}
  \item{open.browser}{Open a web browser?}
}

\value{The R representation of the exported JSON, so we can easily do
checks.}

\author{Toby Dylan Hocking}




\examples{
data(generation.loci)
## Example: 2 plots, 2 selectors.
generations <- data.frame(generation=unique(generation.loci$generation))
loci <- data.frame(locus=unique(generation.loci$locus))
two.selectors.not.animated <- {
  list(ts=ggplot()+
       geom_vline(aes(xintercept=generation, clickSelects=generation),
                  data=generations, alpha=1/2, lwd=4)+
       geom_line(aes(generation, frequency, group=population,
                     showSelected=locus), data=generation.loci),
       loci=ggplot()+
       geom_vline(aes(xintercept=locus, clickSelects=locus),
                  data=loci, alpha=1/2, size=4)+
       geom_point(aes(locus, frequency, showSelected=generation,
                      duration=1000),
                  data=generation.loci)
       )}
gg2animint(two.selectors.not.animated)

## Example: 3 plots, 1 selector.
first <- subset(generation.loci,generation==1)
ancestral <- do.call(rbind,lapply(split(first,first$locus),with,{
  stopifnot(all(frequency==frequency[1]))
  data.frame(locus=locus[1],ancestral=frequency[1])
}))
gl.list <- split(generation.loci,with(generation.loci,list(generation,locus)))
generation.pop <- do.call(rbind,lapply(gl.list,with,{
  data.frame(generation=generation[1], locus=locus[1],
             estimated=mean(frequency))
}))
generation.pop$ancestral <- ancestral$ancestral[generation.pop$locus]

## Calculate the subset for just the last generation, to plot.
generation.loci.last <- subset(generation.loci,generation==max(generation))
generation.pop.last <- subset(generation.pop,generation==max(generation))
one.selector.not.animated <- {
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
       )}
gg2animint(one.selector.not.animated)
}

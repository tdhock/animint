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
## Calculate vline data.frames.
generations <- data.frame(generation=unique(generation.loci$generation))
loci <- data.frame(locus=unique(generation.loci$locus))
two.selectors.not.animated <- {
  list(ts=ggplot()+
       geom_vline(aes(xintercept=generation,clickSelects=generation),
                  data=generations,alpha=1/2,lwd=4)+
       geom_line(aes(generation,frequency,group=population,
                     showSelected=locus),data=generation.loci),
       loci=ggplot()+
       geom_vline(aes(xintercept=locus,clickSelects=locus),
                  data=loci,alpha=1/2,size=4)+
       geom_point(aes(locus,frequency,showSelected=generation),
                  data=generation.loci)
)}
json <- gg2animint(two.selectors.not.animated)
}

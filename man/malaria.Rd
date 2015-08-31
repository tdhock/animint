\name{malaria}
\alias{malaria}
\docType{data}
\title{
  Malaria parasite genome variants
}
\description{
  Two sequencing methods were used to measure genetic variants in
  malaria parasite yoelii yoelii, strain N67. High quality gold standard
  Sanger sequences were obtained for 45 amplicons, a small subset of the
  14-chromosome genome. Next generation sequencing (NGS) was used to
  find genome-wide variants, but there is a lot more noise and so some
  false positive variants need to be filtered via thresholding the MQ score.
}
\usage{data("malaria")}
\format{
List of 8 data.frames:
 $ error.variants  :'data.frame':	18800 obs. of  19 variables:
 $ regions         :'data.frame':	14 obs. of  6 variables:
 $ amplicons       :'data.frame':	45 obs. of  8 variables:
 $ chroms          :'data.frame':	14 obs. of  2 variables:
 $ error.amplicons :'data.frame':	1800 obs. of  11 variables:
 $ filterVar.labels:'data.frame':	40 obs. of  3 variables:
 $ error.curves    :'data.frame':	120 obs. of  4 variables:
 $ filterVar       :'data.frame':	1 obs. of  2 variables:
}
\source{
  https://github.com/tdhock/malaria-evolution-viz
}
\keyword{datasets}

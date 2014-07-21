library(animint)
library(reshape2)

data("uci621raw", package = "benchmark")
molt <- melt(uci621raw)
perfs <- dcast(molt,samp+alg+ds~perf)
perfMeans <- dcast(molt,alg+ds~perf,mean,na.rm=TRUE)
x <- 0.3
y <- 500
algos <-
  list(selectAlg=ggplot()+
       geom_point(aes(Misclassification, Time, colour=alg,
                      clickSelects=alg, showSelected=ds),
                  data=perfs, alpha=6/10)+
       make_text(perfs,x,y,"ds"),
       selectDS=ggplot()+
       ## geom_text(aes(Misclassification, Time, 
       ##                label=ds, showSelected=alg),
       ##            data=perfMeans)+
       geom_point(aes(Misclassification, Time, colour=alg,
                      clickSelects=ds, showSelected=alg),
                  data=perfMeans)+
       make_text(perfs,x,y,"alg"),
       selectAlgLog=ggplot()+
       geom_point(aes(Misclassification, log10(Time), colour=alg,
                      clickSelects=alg, showSelected=ds),
                  data=perfs, alpha=6/10)+
       make_text(perfs,x,log10(y),"ds"),
       selectDSLog=ggplot()+
       geom_text(aes(Misclassification, log10(Time), 
                      label=ds, showSelected=alg),
                  data=perfMeans, alpha=1/4)+
       geom_point(aes(Misclassification, log10(Time), colour=alg,
                      clickSelects=ds, showSelected=alg),
                  data=perfMeans, size=5)+
       make_text(perfs,x,log10(y),"alg"))       
gg2animint(algos, "benchmark")

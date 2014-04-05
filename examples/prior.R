library(animint)
data(prior)

prior$accuracy$percent <- prior$accuracy$accuracy.mean * 100
prior$accuracy$percent.se <- prior$accuracy$accuracy.se * 100
sqLab <- "squared error of the prior estimate"
priorBands <-
  list(set=ggplot()+
       geom_abline()+
       geom_text(aes(positive, negative, label=set), data=prior$data)+
       geom_point(aes(positive, negative, size=dimension, clickSelects=set),
                  data=prior$data)+
       scale_size_continuous(range=c(3,20),breaks=prior$data$dim),
       error=ggplot()+
       make_text(prior$accuracy, 86, 0.3, "prior")+
       make_text(prior$accuracy, 86, 0.32, "samples")+
       geom_point(aes(percent, sqErr.mean, fill=method, colour=classifier,
                      showSelected=prior, showSelected2=samples,
                      clickSelects=set),
                  data=prior$accuracy, size=4)+
       scale_colour_manual(values=c("Kernel logistic regression"="black",
                             "Least squares probabalistic classifier"="white"))+
       ylab(sqLab)+
       xlab("percent classification accuracy"),
       samples=ggplot()+
       make_tallrect(prior$accuracy, "samples")+
       make_text(prior$accuracy, 175, 97.5, "prior")+
       make_text(prior$accuracy, 175, 95, "set")+
       geom_ribbon(aes(samples,
                       ymin=percent-percent.se,
                       ymax=percent+percent.se,
                       group=interaction(method, classifier),
                       fill=method, showSelected=prior, showSelected2=set),
                   data=prior$accuracy, alpha=1/4)+
       geom_line(aes(samples, percent, group=interaction(method, classifier),
                     colour=method, linetype=classifier,
                     showSelected=prior, showSelected2=set),
                 data=prior$accuracy)+
       guides(colour="none",linetype="none",fill="none")+
       xlab("number of points sampled")+
       ylab("percent classification accuracy"),
       prior=ggplot()+
       make_tallrect(prior$accuracy, "prior")+
       make_text(prior$accuracy, 0.5, 97.5, "samples")+
       make_text(prior$accuracy, 0.5, 95, "set")+
       geom_ribbon(aes(prior, ymin=percent-percent.se, ymax=percent+percent.se,
                       group=interaction(method, classifier),
                       fill=method,
                       showSelected=samples, showSelected2=set),
                   data=prior$accuracy, alpha=1/4)+
       geom_line(aes(prior, percent, group=interaction(method, classifier),
                     colour=method, linetype=classifier,
                     showSelected=samples, showSelected2=set),
                 data=prior$accuracy)+
       xlab("class prior")+
       ylab("percent classification accuracy"),
       samplessqErr=ggplot()+
       make_tallrect(prior$accuracy, "samples")+
       geom_ribbon(aes(samples,
                       ymin=sqErr.mean-sqErr.se,
                       ymax=sqErr.mean+sqErr.se,
                       group=interaction(method, classifier),
                       fill=method, showSelected=prior, showSelected2=set),
                   data=prior$accuracy, alpha=1/4)+
       geom_line(aes(samples, sqErr.mean, group=interaction(method, classifier),
                     colour=method, linetype=classifier,
                     showSelected=prior, showSelected2=set),
                 data=prior$accuracy)+
       guides(colour="none",linetype="none",fill="none")+
       xlab("number of points sampled")+
       ylab(sqLab),
       priorsqErr=ggplot()+
       make_tallrect(prior$accuracy, "prior")+
       geom_ribbon(aes(prior,
                       ymin=sqErr.mean-sqErr.se,
                       ymax=sqErr.mean+sqErr.se,
                       group=interaction(method, classifier),
                       fill=method,
                       showSelected=samples, showSelected2=set),
                   data=prior$accuracy, alpha=1/4)+
       geom_line(aes(prior, sqErr.mean, group=interaction(method, classifier),
                     colour=method, linetype=classifier,
                     showSelected=samples, showSelected2=set),
                 data=prior$accuracy)+
       xlab("class prior")+
       ylab(sqLab))
gg2animint(priorBands, "prior")

## are the exported files the same?

## csv.files <- Sys.glob("/tmp/RtmpVIt99h/filee8b6b741ce7/*.csv")
## for(i in 1:(length(csv.files)-1)){
##   for(j in (i+1):length(csv.files)){
##     cmd <- sprintf("diff %s %s|head -1",csv.files[i],csv.files[j])
##     out <- system(cmd, intern=TRUE)
##     if(length(out)==0){
##       print(cmd)
##     }
##   }
## }

## Answer: 3 pairs are the same: (12,20), (14,9), (17,7). So actually
## there is not so much repetition that can be easily avoided.

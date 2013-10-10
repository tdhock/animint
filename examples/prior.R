library(animint)
library(plyr)

priorAccuracy <-
  read.table("../data/priorAccuracy.csv",sep=",",strip.white=TRUE)
names(priorAccuracy) <-
  c("set","samples","prior","algorithm",
    "sqErr.mean","sqErr.se","accuracy.mean","accuracy.se")
priorAccuracy$percent <- priorAccuracy$accuracy.mean * 100
priorAccuracy$percent.se <- priorAccuracy$accuracy.se * 100
algoStats <- sort(daply(priorAccuracy, .(algorithm), with, mean(percent)))
ord <- names(algoStats)
data.info <- ddply(priorAccuracy, .(set, samples, prior),
                   summarize, mean=mean(percent))
priorAccuracy$algorithm <- factor(priorAccuracy$algo, ord, ord)
priorLines <-
  list(samples=ggplot()+
       make_tallrect(priorAccuracy, "samples")+
       make_text(priorAccuracy, 175, 92.5, "prior")+
       make_text(priorAccuracy, 175, 95, "set")+
       geom_line(aes(samples, percent, group=algorithm, colour=algorithm,
                     showSelected=prior, showSelected2=set),
                 data=priorAccuracy)+
       guides(colour="none")+
       xlab("number of points sampled")+
       ylab("percent classification accuracy"),
       prior=ggplot()+
       make_tallrect(priorAccuracy, "prior")+
       make_text(priorAccuracy, 0.5, 92.5, "samples")+
       make_text(priorAccuracy, 0.5, 95, "set")+
       geom_line(aes(prior, percent, group=algorithm, colour=algorithm,
                     showSelected=samples, showSelected2=set),
                 data=priorAccuracy)+
       xlab("class prior")+
       ylab("percent classification accuracy"),
       data=ggplot()+
       geom_bar(aes(set, mean, clickSelects=set,
                    showSelected=samples, showSelected2=prior),
                stat="identity", position="identity", data=data.info)+
       coord_flip()+
       ylab("percent classification accuracy")+
       xlab("data set"))
gg2animint(priorLines)

decode <- c(PE="Pearson divergence",
            KLR="Kernel logistic regression",
            LSPC="Least squares probabalistic classifier",
            "EL-KLR"="Elkan's kernel logistic regression",
            "EL-LSPC"="Elkan's least squares probabalistic classifier")
priorData <- read.table("../data/priorData.csv")
names(priorData) <- c("set","dimension","positive","negative","total")
## Parse the first occurance of pattern from each of several strings
## using (named) capturing regular expressions, returning a matrix
## (with column names).
str_match_perl <- function(string,pattern){
  stopifnot(is.character(string))
  stopifnot(is.character(pattern))
  stopifnot(length(pattern)==1)
  parsed <- regexpr(pattern,string,perl=TRUE)
  captured.text <- substr(string,parsed,parsed+attr(parsed,"match.length")-1)
  captured.text[captured.text==""] <- NA
  captured.groups <- do.call(rbind,lapply(seq_along(string),function(i){
    st <- attr(parsed,"capture.start")[i,]
    if(is.na(parsed[i]) || parsed[i]==-1)return(rep(NA,length(st)))
    substring(string[i],st,st+attr(parsed,"capture.length")[i,]-1)
  }))
  result <- cbind(captured.text,captured.groups)
  colnames(result) <- c("",attr(parsed,"capture.names"))
  result
}
pattern <- "(?<method>.*)-CA-(?<classifier>.*)"
method.classifier <-
  str_match_perl(as.character(priorAccuracy$algorithm), pattern)
for(var.name in c("method", "classifier")){
  extract <- method.classifier[,var.name]
  priorAccuracy[,var.name] <- decode[extract]
}
sqLab <- "squared error of the prior estimate"
priorBands <-
  list(set=ggplot()+
       geom_abline()+
       geom_text(aes(positive, negative, label=set), data=priorData)+
       geom_point(aes(positive, negative, size=dimension, clickSelects=set),
                  data=priorData)+
       scale_size_continuous(range=c(3,20),breaks=priorData$dim),
       error=ggplot()+
       make_text(priorAccuracy, 86, 0.3, "prior")+
       make_text(priorAccuracy, 86, 0.32, "samples")+
       geom_point(aes(percent, sqErr.mean, fill=method, colour=classifier,
                      showSelected=prior, showSelected2=samples, clickSelects=set),
                  data=priorAccuracy, size=4)+
       scale_colour_manual(values=c("Kernel logistic regression"="black",
                             "Least squares probabalistic classifier"="white"))+
       ylab(sqLab)+
       xlab("percent classification accuracy"),
       samples=ggplot()+
       make_tallrect(priorAccuracy, "samples")+
       make_text(priorAccuracy, 175, 97.5, "prior")+
       make_text(priorAccuracy, 175, 95, "set")+
       geom_ribbon(aes(samples, ymin=percent-percent.se, ymax=percent+percent.se,
                       group=interaction(method, classifier),
                       fill=method, showSelected=prior, showSelected2=set),
                   data=priorAccuracy, alpha=1/4)+
       geom_line(aes(samples, percent, group=interaction(method, classifier),
                     colour=method, linetype=classifier,
                     showSelected=prior, showSelected2=set),
                 data=priorAccuracy)+
       guides(colour="none",linetype="none",fill="none")+
       xlab("number of points sampled")+
       ylab("percent classification accuracy"),
       prior=ggplot()+
       make_tallrect(priorAccuracy, "prior")+
       make_text(priorAccuracy, 0.5, 97.5, "samples")+
       make_text(priorAccuracy, 0.5, 95, "set")+
       geom_ribbon(aes(prior, ymin=percent-percent.se, ymax=percent+percent.se,
                       group=interaction(method, classifier),
                       fill=method,
                       showSelected=samples, showSelected2=set),
                   data=priorAccuracy, alpha=1/4)+
       geom_line(aes(prior, percent, group=interaction(method, classifier),
                     colour=method, linetype=classifier,
                     showSelected=samples, showSelected2=set), data=priorAccuracy)+
       xlab("class prior")+
       ylab("percent classification accuracy"),
       samplessqErr=ggplot()+
       make_tallrect(priorAccuracy, "samples")+
       ## make_text(priorAccuracy, 175, 97.5, "prior")+
       ## make_text(priorAccuracy, 175, 95, "set")+
       geom_ribbon(aes(samples, ymin=sqErr.mean-sqErr.se, ymax=sqErr.mean+sqErr.se,
                       group=interaction(method, classifier),
                       fill=method, showSelected=prior, showSelected2=set),
                   data=priorAccuracy, alpha=1/4)+
       geom_line(aes(samples, sqErr.mean, group=interaction(method, classifier),
                     colour=method, linetype=classifier,
                     showSelected=prior, showSelected2=set),
                 data=priorAccuracy)+
       guides(colour="none",linetype="none",fill="none")+
       xlab("number of points sampled")+
       ylab(sqLab),
       priorsqErr=ggplot()+
       make_tallrect(priorAccuracy, "prior")+
       ## make_text(priorAccuracy, 0.5, 97.5, "samples")+
       ## make_text(priorAccuracy, 0.5, 95, "set")+
       geom_ribbon(aes(prior, ymin=sqErr.mean-sqErr.se, ymax=sqErr.mean+sqErr.se,
                       group=interaction(method, classifier),
                       fill=method,
                       showSelected=samples, showSelected2=set),
                   data=priorAccuracy, alpha=1/4)+
       geom_line(aes(prior, sqErr.mean, group=interaction(method, classifier),
                     colour=method, linetype=classifier,
                     showSelected=samples, showSelected2=set), data=priorAccuracy)+
       xlab("class prior")+
       ylab(sqLab))
gg2animint(priorBands)

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

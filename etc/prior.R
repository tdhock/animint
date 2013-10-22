priorAccuracy <-
  read.table("../data/priorAccuracy.csv",sep=",",strip.white=TRUE)
names(priorAccuracy) <-
  c("set","samples","prior","algorithm",
    "sqErr.mean","sqErr.se","accuracy.mean","accuracy.se")
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
  priorAccuracy[,var.name] <- factor(decode[extract])
}

prior <- list(accuracy=priorAccuracy, data.set.info=priorData)

save(prior, file="../data/prior.RData")

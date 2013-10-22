library(reshape2)

changeLik <- read.table("../data/changeLik.csv",sep=",")
names(changeLik) <- c("lambda","train","test")
complexity <- function(lambda)-log(lambda)
changeCurves <- melt(changeLik,id="lambda")
changeCurves$complexity <- complexity(changeCurves$lambda)
names(changeCurves)[2:3] <- c("set","log.likelihood")

changeCoef <- read.table("../data/changeCoef.csv.gz",sep=",")
names(changeCoef) <- c("lambda","variables","coefficient")
changeCoef$complexity <- complexity(changeCoef$lambda)
changeCoef$change <- "guess"

changeTruth <- read.table("../data/changeTruth.csv", sep=",")
names(changeTruth) <- c("variables", "changed")
changeTruth$truth <- ifelse(changeTruth$changed, "change", "no change")
changeTruth$change <- "true"

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
## Add truth to coefs.
rownames(changeTruth) <- as.character(changeTruth$var)
changeCoef$truth <- changeTruth[as.character(changeCoef$var), "truth"]

## Convert variables in Song's string format to integer columns.
pattern <- "(?<v1>[0-9]+)  (?<v2>[0-9]+)"
change <- list(coefs=changeCoef, truth=changeTruth)
for(df.name in names(change)){
  df <- change[[df.name]]
  var.mat <- str_match_perl(as.character(df$var), pattern)
  for(v.name in c("v1", "v2")){
    df[,v.name] <- as.integer(var.mat[,v.name])
  }
  change[[df.name]] <- df
}
## Define positions for each variable.
var.ids <- unique(with(change$coefs, c(v1, v2)))
angle <- seq(0, 2*pi, l=length(var.ids)+1)[-1]
changePos <- data.frame(variable=var.ids, x=cos(angle), y=sin(angle))
## append the positions for each.
for(df.name in names(change)){
  df <- change[[df.name]]
  for(v.name in c("v1","v2")){
    i <- df[,v.name]
    for(xy in c("x","y")){
      newCol <- sprintf("%s.%s",v.name,xy)
      df[,newCol] <- changePos[i, xy]
    }
  }
  change[[df.name]] <- df
}

change$likelihood <- changeCurves

save(change, file="../data/change.RData")

library(animint)
library(reshape2)

changeLik <- read.table("../data/changeLik.csv",sep=",")
names(changeLik) <- c("lambda","train","test")
complexity <- function(lambda)-log(lambda)
changeCurves <- melt(changeLik,id="lambda")
changeCurves$complexity <- complexity(changeCurves$lambda)
names(changeCurves)[2:3] <- c("set","log.likelihood")
train.test <- data.frame(x=6, y=c(-3, -5), set=c("train","test"))
likPlot <- ggplot()+
  make_tallrect(changeCurves, "complexity")+
  geom_line(aes(complexity, log(log.likelihood+.002), group=set, colour=set),
            data=changeCurves, size=5)+
  xlab("model complexity -log(lambda)")+
  guides(colour="none")+
  geom_text(aes(x, y, label=set, colour=set), data=train.test)+
  ggtitle("Train and test likelihood")
print(likPlot)

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
var.dfs <- list(coefs=changeCoef, truth=changeTruth)
for(df.name in names(var.dfs)){
  df <- var.dfs[[df.name]]
  var.mat <- str_match_perl(as.character(df$var), pattern)
  for(v.name in c("v1", "v2")){
    df[,v.name] <- as.integer(var.mat[,v.name])
  }
  var.dfs[[df.name]] <- df
}
## Define positions for each variable.
var.ids <- unique(with(var.dfs$coefs, c(v1, v2)))
angle <- seq(0, 2*pi, l=length(var.ids)+1)[-1]
changePos <- data.frame(variable=var.ids, x=cos(angle), y=sin(angle))
## append the positions for each.
for(df.name in names(var.dfs)){
  df <- var.dfs[[df.name]]
  for(v.name in c("v1","v2")){
    i <- df[,v.name]
    for(xy in c("x","y")){
      newCol <- sprintf("%s.%s",v.name,xy)
      df[,newCol] <- changePos[i, xy]
    }
  }
  var.dfs[[df.name]] <- df
}

## Just the variables which have really changed.
changed <- subset(var.dfs$truth, changed)
varPlot <- ggplot()+
  geom_text(aes(x,y,label=variable), data=changePos)+
  geom_segment(aes(v1.x, v1.y, xend=v2.x, yend=v2.y,
                   size=change, colour=change,
                   showSelected=complexity, clickSelects=variables),
               data=subset(var.dfs$coefs, coefficient != 0), alpha=3/4)+
  geom_segment(aes(v1.x, v1.y, xend=v2.x, yend=v2.y,
                   size=change, colour=change),
               data=changed)+
  scale_size_manual(values=c(guess=10, true=1))+
  scale_colour_manual(values=c(true="black",guess="violet"))+
  theme(axis.line=element_blank(), axis.text=element_blank(), 
        axis.ticks=element_blank(), axis.title=element_blank())+
  ggtitle("Graphical model structure")
##print(varPlot)

## The path of coefficients.
pathPlot <- ggplot()+
  make_tallrect(var.dfs$coefs, "complexity")+
  geom_line(aes(complexity, coefficient, group=variables, colour=truth,
                clickSelects=variables),
            data=var.dfs$coefs, alpha=3/4, size=3)+
  xlab("model complexity -log(lambda)")+
  ylab("norm of the difference between two variables")+
  scale_colour_manual(values=c("no change"="red", "change"="black"))+
  ggtitle("Regularization path")
print(pathPlot)

viz <- list(path=pathPlot, var=varPlot, lik=likPlot)
gg2animint(viz, "~/public_html/change")


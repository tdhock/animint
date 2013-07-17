library(ggplot2)
library(animint)
library(plyr) # to access round_any
movies$decade <- round_any(movies$year, 10)
m <- ggplot(movies, aes(x=rating, colour=decade, group=decade)) + 
  geom_density(fill=NA) + scale_colour_continuous(guide="legend") 

m <- ggplot(movies, aes(x=rating, colour=decade, group=decade)) + geom_density(fill=NA) + scale_colour_continuous(guide="none")
m 

mb <- ggplot_build(m)
aes.scales <- which(sapply(mb$plot$scales$scales, function(i) sum(i$aesthetics%in%c("colour", "size", "fill", "linetype", "alpha"))>0))

getLegendList <- function(mb){
  aes.scales <- which(sapply(mb$plot$scales$scales, function(i) sum(i$aesthetics%in%c("colour", "size", "fill", "linetype", "alpha"))>0))
  lapply(aes.scales, getLegend, mb)
}


getLegend <- function(mb, i){
  sc <- mb$plot$scales$scales[[i]]
  guidetype <- sc$guide
  sc.aes <- sc$aesthetics
  bk <- ggplot2:::scale_breaks(sc)
  val <- ggplot2:::scale_map(sc, bk)
  labels <- ggplot2:::scale_labels(sc)
  if(sc.aes %in% c("colour", "fill")){
    val <- toRGB(val)
  }
  df <- data.frame(breaks = bk, value = val, label = labels)
  df <- df[which(rowSums(is.na(df))==0),] # return only those entries that have breaks, values, and labels.
  if(guidetype=="none"){
    NULL
  } else{
    list(guide = guidetype, 
         aesthetic = sc.aes, 
         title = as.character(as.expression(mb$plot$mapping[[sc.aes]])), 
         legend = df)
  }
}
legends <- lapply(aes.scales, getLegend, mb=mb)

m <- ggplot(movies, aes(x=length, y=rating, size=votes, colour=factor(Comedy))) + scale_colour_manual(values=c("black", "green")) + geom_jitter(alpha=.5) + scale_size_area() + xlim(c(20, 300))
m
mb <- ggplot_build(m)
aes.scales <- which(sapply(mb$plot$scales$scales, function(i) sum(i$aesthetics%in%c("colour", "size", "fill", "linetype", "alpha"))>0))

legends <- lapply(aes.scales, getLegend, mb=mb)
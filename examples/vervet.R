library(animint)
library(dplyr)
library(reshape2)
library(grid)

data(vervet)

vervetCounts <- vervet$counts
nonzero <- subset(vervetCounts, reads > 0)
hist(vervet$samples[["Total Reads"]])
with(vervet$samples, table(experiment, contamination))
with(vervet$samples, table(monkey, contamination))
ord <- order(nonzero$loc)
counts <- nonzero[ord,]
experiment.colors <- c(reseqApril="green", sequenced="black")
contamination.colors <- c(hostDNA="pink", none="blue")
paths <- counts %.%
  group_by(monkey, location, id55mer) %.%
  summarise(mean=mean(reads), count=n(), min=min(reads), max=max(reads)) %.%
  arrange(location)
bars <- filter(paths, count > 1)
vervetCounts$observed <- ifelse(vervetCounts$reads == 0, "absent", "present")
vervetCounts$total.words.55mers <- vervetCounts[["Total Words(55-mers)"]]
vervetCounts$total.freq.55mers <-
  vervetCounts[["Total Frequency of Words 55-mers"]]
## Make pairwise scatterplots of all kmers for every monkey. X and Y
## variables are different locations in the digestive tract.
monkey.list <- with(vervet, split(counts, counts$monkey))

## This doesn't work because of C stack overflow:

##monkey.cast <- dcast(vervetCounts, monkey + id55mer ~ location, mean, value.var="reads")

## So instead lets do it by hand:
monkey.cast <- list()
levs <- levels(vervet$samp$loc)#[1:5]
angle <- seq(0, 2*pi, l=length(levs)+1)[-1]
circle.text <- data.frame(location=levs, x=cos(angle), y=sin(angle),
                          row.names=levs)
for(monkey.name in names(monkey.list)){
  m1 <- monkey.list[[monkey.name]]
  d <- dcast(m1, monkey + id55mer ~ location, mean, value.var="reads")
  ## for(v.name in levs){
  ##   if(is.null(d[[v.name]])){
  ##     d[[v.name]] <- Inf
  ##   }
  ## }
  monkey.cast[[monkey.name]] <- d
}
scatter <- list()
axis.labels <- data.frame()
mytrans <- function(x)log10(x) #to plot the percent reads.
v.name.list <- list()
circle.segs <- data.frame()
for(pair.i in 1:(length(levs)-1)){
  for(pair.j in (pair.i+1):length(levs)){
    x <- levs[pair.i]
    y <- levs[pair.j]
    pair <- sprintf("y=%s vs x=%s", y, x)
    v.name.list[[length(v.name.list)+1]] <- 
      list(distance=pair.j-pair.i, x=x, y=y, pair=pair)
    from <- circle.text[x,]
    to <- circle.text[y,]
    circle.segs <- rbind(circle.segs, {
      data.frame(pair, x=from$x, xend=to$x, y=from$y, yend=to$y)
    })
  }
}
rownames(circle.segs) <- circle.segs$pair

observed.segs <- data.frame()
pair.rects <- data.frame()
for(pair.i in seq_along(v.name.list)){ #pairs of locations -> scatterplots.
  v.names <- v.name.list[[pair.i]]
  pair <- v.names$pair
  pair.dist <- v.names$dist
  v.names <- v.names[c("x", "y")]
  axis.labels <- rbind(axis.labels, {
    data.frame(x=c(-1/2, -2.5), y=c(-2, -1/2), label=c(v.names$y, v.names$x),
               pair, angle=c(90, 0))
  })
  for(monkey.name in names(monkey.list)){
    d <- monkey.cast[[monkey.name]]
    has.both <- all(c(v.names$x, v.names$y) %in% names(d))
    if(has.both){
      ## Create a circle for selecting all location pairs.
      monkey.segs <- data.frame(circle.segs[pair,],
                                monkey=as.integer(monkey.name))
      observed.segs <- rbind(observed.segs, monkey.segs)
      ## Create a rect for selecting nearby location pairs.
      if(pair.dist == 1){
        pair.rects <- rbind(pair.rects, {
          data.frame(pair, monkey=as.integer(monkey.name),
                     xmin=-4, xmax=0,
                     ymin=v.names$x, #x
                     ymax=v.names$y) #y
        })
      }
      is.zero <- lapply(v.names, function(v.name)d[[v.name]] == 0)
      ##is.inf <- lapply(v.names, function
      sets <- list(one.point=is.zero$x & is.zero$y,
                   vertical.seg=is.zero$x & !is.zero$y,
                   horizontal.seg=!is.zero$x & is.zero$y,
                   is.point=!is.zero$x & !is.zero$y)
      stopifnot(sapply(sets, function(x)sum(is.na(x)))==0)
      dfs <- lapply(sets, function(is)d[is,])
      all.pts <- do.call(rbind, dfs)
      these <- list(zero=data.frame(kmers=nrow(dfs$one),
                      monkey=as.integer(monkey.name),
                      x=0, y=0, hjust=0, vjust=0, pair),
                    all=data.frame(x=all.pts[,v.names$x],
                      y=all.pts[,v.names$y], id55mer=all.pts$id55mer,
                      pair, monkey=as.integer(monkey.name)),
                    point=data.frame(),
                    seg=data.frame())
      these$label <- rbind(these$zero,{
        data.frame(kmers=nrow(dfs$is), monkey=as.integer(monkey.name),
                   x=Inf, y=Inf, hjust=1, vjust=1, pair)
      })
      y <- dfs$vert[,v.names$y]
      if(nrow(dfs$vert)){
        df <- data.frame(xmin=0, xmax=0, ymin=min(y), ymax=max(y),
                         kmers=length(y), monkey=as.integer(monkey.name), pair)
        these$seg <- rbind(these$seg, df)
        these$label <- rbind(these$label, with(df, {
          data.frame(kmers, monkey, x=0, y=ymax,
                     hjust=0, vjust=0, pair)
        }))
      }
      x <- dfs$horiz[,v.names$x]
      if(nrow(dfs$hor)){
        df <- data.frame(xmin=min(x), xmax=max(x), ymin=0, ymax=0,
                         kmers=length(x), monkey=as.integer(monkey.name), pair)
        these$seg <- rbind(these$seg, df)
        these$label <- rbind(these$label, with(df, {
          data.frame(kmers, monkey, x=xmax, y=0,
                     hjust=0, vjust=0, pair)
        }))
      }
      if(nrow(dfs$is)){
        these$point <- rbind(these$point, {
          data.frame(id55mer=dfs$is[,"id55mer"], monkey=as.integer(monkey.name),
                     x=dfs$is[,v.names$x], y=dfs$is[,v.names$y], pair)
        })
      }
      p <- ggplot()+
        geom_point(aes(mytrans(x), mytrans(y)), data=these$zero, size=5)+
          xlab(v.names$x)+ylab(v.names$y)        
      if(nrow(these$point)){
        p <- p+
          geom_point(aes(mytrans(x), mytrans(y),
                         clickSelects=id55mer, showSelected=monkey),
                     data=these$point)
      }
      if(nrow(these$label)){
        p <- p+
          geom_text(aes(mytrans(x), mytrans(y),
                        label=sprintf("%d kmers", kmers),
                        hjust=hjust, vjust=vjust),
                    data=these$label)
      }
      if(nrow(these$seg)){
        p <- p+
          geom_segment(aes(mytrans(xmin), mytrans(ymin),
                           xend=mytrans(xmax), yend=mytrans(ymax)),
                       data=these$seg, size=2)
      }
      for(N in names(these)){
        scatter[[N]] <- rbind(scatter[[N]], these[[N]])
      }
      ##print(p)
    }
  }
}

ablines <- data.frame(slope=1, intercept=0)
lab <- "log10(percent reads)"
p <- ggplot()+
  geom_abline(aes(slope=slope, intercept=intercept), data=ablines)+
  geom_point(aes(mytrans(x), mytrans(y)), data=scatter$zero, size=5)+
  geom_point(aes(mytrans(x), mytrans(y),
           clickSelects=id55mer, showSelected=monkey),
         data=scatter$point)+
  geom_segment(aes(mytrans(xmin), mytrans(ymin),
           xend=mytrans(xmax), yend=mytrans(ymax)),
         data=scatter$seg, size=2)+
  geom_text(aes(mytrans(x), mytrans(y), label=sprintf("%d", kmers),
          hjust=hjust, vjust=vjust), colour="red",
        data=scatter$label)+
  ggtitle("log10(percent reads) is correlated in adjacent locations")+
  xlab(lab)+ylab(lab)+
  facet_grid(pair~monkey,labeller=function(var, val){
    if(var=="monkey"){
      sprintf("monkey %s", val)
    }else{
      as.character(val)
    }
  })+
  theme_bw()+
  theme(panel.margin=unit(0, "cm"))+
  coord_equal()

## Create geom_rects for selecting nearby location pairs.
nearby <- v.name.list[sapply(v.name.list, "[[", "distance") == 1]
pair.levs <- sapply(nearby, "[[", "pair")
pair.select <-
  geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,
                clickSelects=pair, showSelected=monkey),
            data=pair.rects, alpha=1/2)
ggplot()+pair.select+
  ylim(levs)+
  facet_wrap("monkey")+
  theme_bw()+
  theme(panel.margin=unit(0, "cm"))

## Table with 1 row for each monkey.
new.names <- c()
old.names <- names(vervet$mon)[5:11]
table.rects <- vervet$monkeys
for(col.name in old.names){
  new.name <- substr(col.name, 1, 3)
  table.rects[[new.name]] <- table.rects[[col.name]]
  new.names <- c(new.names, new.name)
}
table.cols <- c("monkey", "sex", "years", new.names)
header.df <- data.frame(variable=table.cols, x=seq_along(table.cols))
rownames(header.df) <- header.df$var
old.x <- structure(header.df[new.names,"x"], names=old.names)
table.text <- data.frame(header.df, value=header.df$var, y=0)
table.rects$y <- -(1:nrow(table.rects))
for(x in seq_along(table.cols)){
  table.col <- table.cols[x]
  table.text <- rbind(table.text, {
    data.frame(header.df[x,], value=as.character(table.rects[,table.col]),
               y=table.rects$y,
               row.names=NULL)
  })
}
table.rects$xmin <- min(table.text$x)-1/2
table.rects$xmax <- max(table.text$x)+1/2
rownames(table.rects) <- table.rects$monkey

vervetCounts$x <- old.x[as.character(vervetCounts$location)]
vervetCounts$y <- table.rects[as.character(vervetCounts$monkey), "y"]
## Several plots.
space <- 5/10
viz <-
  list(kmer=ggplot()+ylim(levs)+
       ggtitle("All monkey data for one kmer")+
       xlab(lab)+
       geom_path(aes(log10(mean), location, 
                     showSelected=monkey, showSelected2=id55mer),
                 data=paths, size=5, colour="red")+
       geom_path(aes(log10(mean), location, group=monkey,
                     clickSelects=monkey, showSelected=id55mer),
                 data=paths, alpha=3/4)+
       geom_segment(aes(log10(min), location,
                        xend=log10(max), yend=location,
                        showSelected=monkey, showSelected2=id55mer),
                   data=bars)+
       geom_point(aes(log10(reads), location,
                      fill=contamination, colour=experiment,
                      clickSelects=monkey, showSelected=id55mer),
                  data=counts, size=5, alpha=3/4)+
       scale_colour_manual(values=experiment.colors)+
       scale_fill_manual(values=contamination.colors)+
       make_text(nonzero, -2, "duodenum", "id55mer"),
       samples=ggplot()+
       ggtitle("All samples")+
       make_text(vervetCounts, 750, 1000, "monkey")+
       make_text(vervetCounts, 750, 3000, "id55mer")+
       geom_point(aes(total.words.55mers, total.freq.55mers,
                      clickSelects=monkey,
                      showSelected=id55mer, size=observed,
                      fill=contamination, colour=experiment),
                  data=vervetCounts, alpha=3/4)+
       scale_size_manual(values=c(absent=2, present=5))+
       scale_colour_manual(values=experiment.colors)+
       scale_fill_manual(values=contamination.colors),
### TODO: plot a picture of the intestines in the background.
       circle=ggplot()+
       ggtitle("Location pairs")+
       make_text(vervetCounts, 0, 0, "monkey")+
       theme(axis.line=element_blank(), axis.text=element_blank(), 
             axis.ticks=element_blank(), axis.title=element_blank())+
       geom_text(aes(x, y, label=location), data=circle.text)+
       geom_segment(aes(x, y, xend=xend, yend=yend,
                        showSelected=monkey, clickSelects=pair),
                    data=observed.segs, size=5, alpha=6/10),
       monkey=ggplot()+pair.select+ylim(levs)+
       ggtitle("All 55mer data for one monkey")+
       xlab(lab)+
       geom_path(aes(log10(mean), location, 
                     showSelected=monkey, showSelected2=id55mer),
                 data=paths, size=5, colour="red")+
       geom_path(aes(log10(mean), location, group=id55mer,
                     clickSelects=id55mer, showSelected=monkey),
                 data=paths, alpha=3/4)+
       geom_segment(aes(log10(min), location,
                        xend=log10(max), yend=location,
                        showSelected=monkey, showSelected2=id55mer),
                   data=bars)+
       scale_colour_manual(values=experiment.colors)+
       scale_fill_manual(values=contamination.colors)+
       geom_point(aes(log10(reads), location,
                      fill=contamination, colour=experiment,
                      clickSelects=id55mer, showSelected=monkey),
                  data=counts, size=5, alpha=3/4,
### For the plot of all kmers for 1 monkey there are a lot of points so
### let us jitter them up and down to avoid overlaps.
                  position=position_jitter(width=0))+
       make_text(nonzero, -2, "duodenum", "monkey"),
       scatter=ggplot()+
       geom_point(aes(mytrans(x), mytrans(y), showSelected=id55mer,
                      showSelected2=monkey, showSelected3=pair),
                  data=scatter$point, colour="red", size=5)+
       geom_point(aes(mytrans(x), mytrans(y), clickSelects=id55mer, 
                      showSelected2=monkey, showSelected3=pair),
                  data=scatter$point, position="jitter")+
       geom_text(aes(x, y, label=label, angle=angle, showSelected=pair),
                 data=axis.labels)+
       geom_abline(aes(slope=slope, intercept=intercept), data=ablines)+
       geom_point(aes(mytrans(x), mytrans(y),
                      showSelected=pair, showSelected2=monkey),
                  data=scatter$zero)+
       geom_segment(aes(mytrans(xmin), mytrans(ymin),
                        showSelected=pair, showSelected2=monkey,
                        xend=mytrans(xmax), yend=mytrans(ymax)),
                    data=scatter$seg, size=2)+
       geom_text(aes(mytrans(x), mytrans(y), label=sprintf("%d", kmers),
                     hjust=hjust, vjust=vjust, 
                     showSelected=pair, showSelected2=monkey), colour="red",
                 data=scatter$label)+
       make_text(scatter$point, -2.5, -1/4, "monkey")+
       xlab(lab)+ylab(lab)+
       ggtitle("nearby location correlation"),
       table=ggplot()+
       ggtitle("Select monkey")+
       theme(axis.line=element_blank(), axis.text=element_blank(), 
             axis.ticks=element_blank(), axis.title=element_blank())+
       make_text(vervetCounts, mean(range(table.text$x)), 1, "id55mer")+
       geom_text(aes(x, y, label=ifelse(is.na(value), "", as.character(value))),
                 data=table.text)+
       scale_fill_manual(values=contamination.colors)+
### TODO: clickSelects=pair rects on the header!
       geom_rect(aes(ymin=y-space, ymax=y+space,
                     xmin=x-space, xmax=x+space,
                     showSelected=id55mer,
                     colour=observed, fill=contamination),
                 ##data=subset(vervetCounts, id55mer %in% id55mer[1:100]),
                 data=vervetCounts,
                 alpha=1/2, size=2)+
       scale_colour_manual(values=c(present="black", absent=NA))+
       facet_wrap("id55mer")+
       geom_rect(aes(ymin=y-1/2, ymax=y+1/2, 
                     xmin=xmin, xmax=xmax,
                     clickSelects=monkey),
                 alpha=1/2, fill="yellow", colour=NA, data=table.rects),
       width=list(kmer=800, samples=300, circle=300, monkey=800),
       height=list(kmer=300, samples=300, circle=300, monkey=600))
##viz$sca+facet_grid(pair~monkey)+theme_bw()+theme(panel.margin=unit(0,"cm"))
###print(viz$table)
gg2animint(viz, "vervet")


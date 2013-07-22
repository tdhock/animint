## Make some data that represent an evolution simulation.
works_with_R("3.0.1",nicholsonppp="1.0",reshape2="1.2.2",ggplot2="0.9.3.1")

set.seed(1)
a <- sim.drift.selection(s=0.1,generations=100)
dimnames(a$simulated.freqs) <-
  list("locus"=NULL,"population"=NULL,"generation"=NULL)
freq <- melt(a$sim)


sorted.s <- a$s[with(a$s,order(S,ancestral)),]
sorted.s$sorted.id <- 1:nrow(sorted.s)
sorted.s <- sorted.s[order(sorted.s$locus),]

loc.pop.colors <- as.matrix(sorted.s[,grepl("color",names(sorted.s))])
dimnames(loc.pop.colors) <- list(locus=NULL,population=NULL)
colors.df <- melt(loc.pop.colors)

## quick merge and check
test <- data.frame(freq,colors.df)
with(test,sum(locus!=locus.1))
with(test,sum(population!=population.1))
merged <- with(test,{
  data.frame(locus,population,generation,value,
             color=value.1,
             sorted.id=sorted.s$sorted.id)
})

type.info <- sorted.s[,c("locus","type")]
with.type <- merge(merged, type.info)
generation.loci <- with(with.type,{
  data.frame(locus=sorted.id,population,generation,frequency=value,color,type)
})
generation.loci <-
  generation.loci[with(generation.loci,order(locus,population,generation)),]
save(generation.loci,file=file.path("..","data","generation.loci.RData"),
     compress="xz")

## time series for 1 locus.
l.id <- subset(a$s,type=="positive")$locus[1]
l1 <- subset(merged,locus==l.id)
color.key <- c(blue="blue",red="red",neutral="turquoise")
ggplot(l1,aes(generation,value))+
  geom_line(aes(group=population,colour=color))+
  scale_colour_manual(values=color.key)+
  ggtitle(with(a$s[l.id,],sprintf("locus %d s=%f selection=%s",
         l.id,s,type)))

## freq dotplot for 1 generation.
g1 <- subset(merged,generation==50)
ggplot(g1,aes(sorted.id,value))+
  geom_point(aes(y=ancestral),data=sorted.s,pch=21)+
  geom_point(aes(colour=color))+
  scale_colour_manual(values=color.key)+
  ylim(0,1)+
  ylab("blue allele frequency")


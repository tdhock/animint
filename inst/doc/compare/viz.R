library(reshape2)
library(plyr)
data(compare)
## difference between rank and compare models.
sets <- dcast(compare$err, N + seed + norm ~ fit.name, value.var="percent")
sets$diff <- sets$compare-sets$rank
sets$set.id <- 1:nrow(sets)
diff.df <- ddply(sets, .(N, norm), summarize,
                 N=N[1], norm=norm[1],
                 mean=mean(diff), sd=sd(diff))
compare$bayes$N <- as.integer(as.character(compare$bayes$N))
## Axes labels.
xl <- xlab("feature 1")
yl <- ylab("feature 2")
x.lab <- "number of labeled pairs in the training set"
model.colors <- c(compare="#00bfc4", #bluish
                  rank="#f8766d",
                  latent="grey")
ord <- c("latent","compare","rank")
lab.df <- subset(diff.df, N==max(N))
viz <-
  list(data=ggplot()+
       geom_segment(aes(Xt.1, Xt.2, xend=Xtp.1, yend=Xtp.2, colour=factor(yt),
                        showSelected=set.id),
                    data=compare$train)+
       geom_point(aes(Xtp.1, Xtp.2, colour=factor(yt),
                      showSelected=set.id),
                  data=subset(compare$train, yt==1))+
       scale_colour_manual("label",values=c("1"="red","-1"="black"))+
       xl+yl+
       ggtitle("training data"),
       error=ggplot()+
       make_text(compare$err, 200, 35, "norm")+
       geom_point(aes(N, percent, colour=model,
                      showSelected=norm, showSelected2=set.id),
                  data=compare$bayes)+
       geom_point(aes(N, percent, colour=fit.name,
                      showSelected=norm,
                      clickSelects=set.id),
                 lwd=3,alpha=3/4,data=compare$err)+
       ylab("percent incorrectly predicted test pairs")+
       scale_colour_manual("model", values=c(model.colors[1:2],latent="black"),
                           breaks=ord)+
       xlab(x.lab)+
       ggtitle("test error, select data set"),
       diff=ggplot()+
       geom_ribbon(aes(N, ymin=mean-sd, ymax=mean+sd, group=norm,
                       clickSelects=norm), alpha=1/2,
                   data=diff.df)+
       geom_line(aes(N, mean, group=norm, clickSelects=norm), 
                   data=diff.df)+
       geom_hline(yintercept=0, color="red")+
       geom_text(aes(x,y,label=label),color="red",
                 data=data.frame(x=150,y=1,label="no difference"))+
       geom_text(aes(N, mean, label=norm), data=lab.df, hjust=0)+
       ggtitle("test error difference, select norm")+
       xlab(x.lab)+
       ylab("<- compare better (test error percent difference) rank better->"))
for(model in c("compare", "rank")){
  sub.df <- subset(compare$rank, what %in% c(model, "latent"))
  L <- list(ggplot()+
    geom_contour(aes(x1, x2, z=rank, group=interaction(what, norm, seed, N),
                     colour=what, showSelected=set.id), data=sub.df)+
    scale_colour_manual("model",values=model.colors)+
            xl+yl+
    ggtitle(sprintf("learned SVM%s model",model)))
  names(L) <- model
  viz <- c(viz, L)
}

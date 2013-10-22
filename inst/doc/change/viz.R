data(change)
train.test <- data.frame(x=6, y=c(-3, -5), set=c("train","test"))
changed <- subset(change$truth, changed)
viz <-
  list(likelihood=ggplot()+
       make_tallrect(change$lik, "complexity")+
       geom_line(aes(complexity, log(log.likelihood+.002),
                     group=set, colour=set),
                 data=change$lik, size=5)+
       xlab("model complexity -log(lambda)")+
       guides(colour="none")+
       geom_text(aes(x, y, label=set, colour=set), data=train.test)+
       ggtitle("Train and test likelihood"),
       variables=ggplot()+
       geom_text(aes(x,y,label=variable), data=change$pos)+
       geom_segment(aes(v1.x, v1.y, xend=v2.x, yend=v2.y,
                        size=change, colour=change,
                        showSelected=complexity, clickSelects=variables),
                    data=subset(change$coefs, coefficient != 0), alpha=3/4)+
       geom_segment(aes(v1.x, v1.y, xend=v2.x, yend=v2.y,
                        size=change, colour=change),
                    data=changed)+
       scale_size_manual(values=c(guess=10, true=1))+
       scale_colour_manual(values=c(true="black",guess="violet"))+
       theme(axis.line=element_blank(), axis.text=element_blank(), 
             axis.ticks=element_blank(), axis.title=element_blank())+
       ggtitle("Graphical model structure"),
       path=ggplot()+
       make_tallrect(change$coefs, "complexity")+
       geom_line(aes(complexity, coefficient, group=variables, colour=truth,
                     clickSelects=variables),
                 data=change$coefs, alpha=3/4, size=3)+
       xlab("model complexity -log(lambda)")+
       ylab("norm of the difference between two variables")+
       scale_colour_manual(values=c("no change"="red", "change"="black"))+
       ggtitle("Regularization path"))


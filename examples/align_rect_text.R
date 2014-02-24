library(animint)
centers <- expand.grid(x=1:10, y=1:20)
aligned <- ggplot(centers)+
  geom_rect(aes(xmin=x-1/2, ymin=y-1/2,
                xmax=x+1/2, ymax=y+1/2),
            fill="white", colour="black")+
  geom_text(aes(x, y, label=sprintf("x=%d,y=%d", x, y)),
            vjust=1/2)
print(aligned)
gg2animint(list(aligned=aligned), "aligned")

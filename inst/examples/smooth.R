library(animint)
viz <-
  list(smooth=ggplot(mtcars, aes(qsec, wt))+
       stat_smooth(method = "loess")+
       geom_point())
animint2dir(viz, "smooth")

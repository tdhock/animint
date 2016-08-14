library(animint)
data(prostateLasso)
variable.colors <- c(
  "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", 
  "#A65628", "#F781BF", "#999999")
hline.df <- data.frame(residual=0)
addY <- function(dt, y){
  data.frame(dt, y.var=factor(y, c("error", "weights")))
}
viz <- list(
  title="Lasso on the prostate cancer data set",
  path=ggplot()+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(y.var ~ ., scales="free")+
    ylab("")+
    scale_color_manual(values=variable.colors)+
    geom_line(aes(arclength, standardized.coef, color=variable, group=variable),
              data=addY(prostateLasso$path, "weights"))+
    geom_line(aes(arclength, mse, linetype=set, group=set),
              data=addY(prostateLasso$error, "error"))+
    make_tallrect(prostateLasso$error, "arclength"),
  res=ggplot()+
    geom_hline(aes(yintercept=residual),
               data=hline.df,
               color="grey")+
    guides(linetype="none")+
    geom_point(aes(response, residual, 
                   key=observation.i,
                   showSelected2=set,
                   showSelected=arclength),
               shape=21,
               fill=NA,
               color="black",
               data=prostateLasso$residuals)+
    geom_text(aes(3, 2.5, label=sprintf("L1 arclength = %.1f", arclength),
                  key=1,
                  showSelected=arclength),
              data=prostateLasso$models)+
    geom_text(aes(0, -2, label=sprintf("train error = %.3f", mse),
                  key=1,
                  showSelected2=set,
                  showSelected=arclength),
              hjust=0,
              data=subset(prostateLasso$error, set=="train"))+
    geom_text(aes(0, -2.5, label=sprintf("validation error = %.3f", mse),
                  key=1,
                  showSelected2=set,
                  showSelected=arclength),
              hjust=0,
              data=subset(prostateLasso$error, set=="validation"))+
    geom_segment(aes(response, residual,
                     xend=response, yend=0,
                     linetype=set,
                     key=observation.i,
                     showSelected2=set,
                     showSelected=arclength),
                 data=prostateLasso$residuals),
  duration=list(arclength=2000))
animint2dir(viz, "figure-prostateLasso")


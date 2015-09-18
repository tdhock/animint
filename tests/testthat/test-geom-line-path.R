library(testthat)
context("geom line")
library(animint)
library(data.table)
library(dplyr)

data(intreg)
signal.colors <- c(estimate="#0adb0a", latent="#0098ef")
breakpoint.colors <- c("1breakpoint"="#ff7d7d", "0breakpoints"='#f6f4bf')
segs <- data.table(intreg$segments)
sigs <- data.table(intreg$signals) %>%
  mutate(base.after=base+1)
setkey(segs, signal, first.base, last.base)
setkey(sigs, signal, base, base.after)
ov <- foverlaps(sigs, segs)
model.selection <- ov %>%
  group_by(signal, segments) %>%
  summarise(error=sum((logratio-mean)^2),
            data=n()) %>%
  mutate(log.data=log(data),
         penalized.error=error + log.data * segments)
sig.labels <- model.selection %>%
  group_by(signal) %>%
  filter(segments==1)
sig.seg.names <- ov %>%
  group_by(signal, segments) %>%
  summarise(min.base=min(base),
            max.base=max(base)) %>%
  mutate(base=(min.base+max.base)/2)
sig.names <- sig.seg.names %>%
  group_by(signal, base) %>%
  summarise(logratio=max(sigs$logratio))
seg.names <- sig.seg.names %>%
  group_by(signal, segments, base) %>%
  summarise(logratio=min(sigs$logratio)-0.2)
tallrects <- make_tallrect(model.selection, "segments")
tallrects$geom_params$colour <- signal.colors[["estimate"]]

## Plot segments rather than penalty.
mmir.selection <- 
  list(error=ggplot()+
       ggtitle("Select profile and number of segments")+
       tallrects+
       theme_bw()+
       theme(panel.margin=grid::unit(0, "lines"))+
       facet_grid(. ~ geom)+
       geom_text(aes(0, error, label=signal, clickSelects=signal),
                 data=sig.labels, hjust=1)+
       scale_x_continuous("segments", breaks=c(1, 5, 10, 20),
                          limits=c(-2, 20))+
       xlab("squared error")+
       geom_line(aes(segments, error,
                     group=signal,
                     clickSelects=signal),
                 data=data.frame(model.selection, geom="line"),
                 alpha=0.6, size=8)+
       geom_path(aes(segments, error,
                     group=signal,
                     clickSelects=signal),
                 data=data.frame(model.selection, geom="path"),
                 alpha=0.6, size=8),

       signal=ggplot()+
       theme_animint(width=800)+       
       scale_x_continuous("position on chromosome (mega base pairs)",
                          breaks=c(100,200))+
       scale_fill_manual(values=breakpoint.colors,guide="none")+
       geom_blank(aes(first.base/1e6, logratio+2/8), data=intreg$ann)+
       ggtitle("Copy number profile and maximum likelihood segmentation")+
       ylab("logratio")+
       geom_point(aes(base/1e6, logratio,
                      showSelected=signal),
                  data=intreg$sig)+
       geom_segment(aes(first.base/1e6, mean, xend=last.base/1e6, yend=mean,
                        showSelected=signal,
                        showSelected2=segments),
                    data=intreg$seg, colour=signal.colors[["estimate"]])+
       geom_segment(aes(base/1e6, min(sigs$logratio),
                        xend=base/1e6, yend=max(sigs$logratio),
                        showSelected=signal,
                        showSelected2=segments),
                  colour=signal.colors[["estimate"]],
                  linetype="dashed",
                  data=intreg$breaks)+
       geom_text(aes(base/1e6, logratio, label=paste("signal", signal),
                     showSelected=signal),
                 data=sig.names)+
       geom_text(aes(base/1e6, logratio,
                     label=sprintf("%d segment%s", segments,
                       ifelse(segments==1, "", "s")),
                     showSelected=signal,
                     showSelected2=segments),
                 data=seg.names, color=signal.colors[["estimate"]]),

       first=list(signal="4.2", segments=4))

animint2dir(mmir.selection, "intreg-selection")

all.increasing <- function(num.vec){
  stopifnot(is.numeric(num.vec))
  all(0 < diff(num.vec))
}

expected.list <-
  list(geom3_line_error=function(x)all.increasing(x),
       geom4_path_error=function(x)!all.increasing(x))

for(g.class in names(expected.list)){
  should.return.true <- expected.list[[g.class]]
  tsv.path <- Sys.glob(file.path("intreg-selection", paste0(g.class, "*")))
  g.data <- read.table(tsv.path, header=TRUE)
  tsv.by.signal <- split(g.data, g.data$clickSelects)
  for(signal.name in names(tsv.by.signal)){
    one.signal <- tsv.by.signal[[signal.name]]
    expect_true(should.return.true(one.signal$x))
  }
}

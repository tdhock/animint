context("PeakConsistency")

data(PeakConsistency)

color.code <-
  c(truth="#1B9E77", #teal
    PeakSeg="#D95F02", #orange
    PeakSegJoint="#7570B3", #violet
    "#E7298A", #pink
    "#66A61E", #green
    "#E6AB02", #tan
    "#A6761D", #brown
    "#666666") #grey

second.small <-
  list(signals=ggplot()+
         theme_bw()+
         theme_animint(width=1000, height=800)+
         theme(panel.margin=grid::unit(0, "cm"))+
         facet_grid(sample.id ~ ., labeller=function(var, val){
           paste("sample", val)
         })+
         guides(size="none")+
         geom_segment(aes(chromStart+0.5, mean,
                          xend=chromEnd+0.5, yend=mean,
                          showSelected=seed, showSelected2=sample.size,
                          color=model, size=model),
                      data=PeakConsistency$model)+
         scale_size_manual(values=c(PeakSegJoint=0.5, PeakSeg=1))+
         scale_color_manual(values=color.code),
       first=list(sample.size=5))

info <- animint2HTML(second.small)

getStroke <- function(element.list){
  style.strs <- sapply(element.list, function(x) xmlAttrs(x)["style"])
  pattern <-
    paste0("(?<name>\\S+?)",
           ": *",
           "(?<value>.+?)",
           ";")
  style.matrices <- str_match_all_perl(style.strs, pattern)
  sapply(style.matrices, function(m)m["stroke", "value"])
}

test_that("15 segments of both colors", {
  line.list <-
    getNodeSet(info$html, '//g[@class="geom1_segment_signals"]//line')
  computed.vec <- getStroke(line.list)
  color.counts <- as.numeric(table(computed.vec))
  expect_equal(color.counts, c(15, 15))
})

viz <-
  list(errors=ggplot()+
         ylab("distance from true peaks to estimated peaks")+
         scale_color_manual(values=color.code)+
         make_tallrect(PeakConsistency$error, "sample.size")+
         geom_line(aes(sample.size, errors,
                       clickSelects=seed,
                       group=interaction(model, seed),
                       color=model),
                   size=5,
                   alpha=0.7,
                   data=PeakConsistency$error),
       signals=ggplot()+
         theme_bw()+
         theme_animint(width=1000, height=800)+
         theme(panel.margin=grid::unit(0, "cm"))+
         facet_grid(sample.id ~ ., labeller=function(var, val){
           paste("sample", val)
         })+
         geom_point(aes(chromEnd, count, showSelected=seed),
                    color="grey50",
                    data=PeakConsistency$signal)+
         geom_vline(aes(xintercept=chromStart+0.5, color=model,
                        showSelected=seed),
                    show_guide=TRUE,
                    linetype="dashed",
                    data=PeakConsistency$truth)+
         guides(size="none")+
         geom_segment(aes(chromStart+0.5, mean,
                          xend=chromEnd+0.5, yend=mean,
                          showSelected=seed, showSelected2=sample.size,
                          color=model, size=model),
                      data=PeakConsistency$model)+
         geom_vline(aes(xintercept=chromStart+0.5,
                        showSelected=seed, showSelected2=sample.size,
                        color=model, size=model),
                    show_guide=TRUE,
                    linetype="dashed",
                    data=PeakConsistency$guess)+
         scale_size_manual(values=c(PeakSegJoint=0.5, PeakSeg=1))+
         scale_color_manual(values=color.code),
       first=list(sample.size=5))

info <- animint2HTML(viz)

test_that("4 paths of both colors in first plot", {
  path.list <- 
    getNodeSet(info$html, '//g[@class="geom2_line_errors"]//path')
  computed.vec <- getStroke(path.list)
  color.counts <- as.numeric(table(computed.vec))
  expect_equal(color.counts, c(4, 4))
})

test_that("15 segments of both colors in second plot", {
  line.list <-
    getNodeSet(info$html, '//g[@class="geom5_segment_signals"]//line')
  computed.vec <- getStroke(line.list)
  color.counts <- as.numeric(table(computed.vec))
  ##expect_equal(color.counts, c(15, 15)) #firefox bug.
  expect_equal(sum(color.counts), 30)
})


context("PredictedPeaks data set")

data(PredictedPeaks)

hgTracks <- "http://genome.ucsc.edu/cgi-bin/hgTracks"
viz <- list(
  oneChrom=ggplot()+
    ggtitle("PeakSegJoint detections on selected chromosome")+
    theme_bw()+
    coord_cartesian(xlim=c(0, 1))+
    theme_animint(width=1500, height=100)+
    theme(axis.line.x=element_blank(), axis.text.x=element_blank(), 
          axis.ticks.x=element_blank(), axis.title.x=element_blank())+
    scale_y_discrete("cell type", drop=FALSE)+
    geom_text(aes(relative.middle, type.fac, label=samples.up,
                  href=sprintf("%s?db=hg19&position=%s:%d-%d",
                    hgTracks, chrom, zoomStart, zoomEnd),
                  showSelected2=chrom,
                  showSelected=dotID),
              size=11,
              data=PredictedPeaks$chromCounts),
  chroms=ggplot()+
    theme_bw()+
    theme_animint(width=1500, height=330)+
    scale_y_discrete("chromosome", drop=FALSE)+ 
    scale_x_continuous("position on chromosome (mega bases)")+
    geom_text(aes(0, chrom, label=paste0(peaks, "_"),
                  clickSelects=chrom,
                  showSelected=dotID),
              hjust=1,
              size=11,
              data=PredictedPeaks$countsByChrom)+
    geom_segment(aes(chromStart/1e6, chrom,
                     clickSelects=chrom,
                     xend=chromEnd/1e6, yend=chrom),
              size=9,
              data=PredictedPeaks$chrom.ranges)+
    geom_point(aes(chromEnd/1e6, chrom,
                   id=chrom,
                   clickSelects=chrom),
              size=5,
              data=PredictedPeaks$chrom.ranges)+
    geom_text(aes(max(PredictedPeaks$chrom.ranges$chromEnd)/2e6, chrom,
                  showSelected=dotID,
                  label=totals),
             data=PredictedPeaks$scatter.text),
  scatter=ggplot()+
    geom_hline(aes(yintercept=N),
               color="grey",
               data=PredictedPeaks$counts.Input)+
    scale_x_continuous("number of samples with a peak")+
    facet_grid(nonInputType ~ .)+
    theme_bw()+
    scale_fill_gradient(low="grey", high="red")+
    theme_animint(width=1500)+
    theme(panel.margin=grid::unit(0, "cm"))+
    geom_vline(aes(xintercept=N),
               color="grey",
               data=PredictedPeaks$counts.not.Input)+
    geom_rect(aes(xmin=up-size, xmax=up+size,
                  ymin=Input-size, ymax=Input+size,
                  tooltip=totals,
                  clickSelects=dotID,
                  showSelected=chrom,
                  fill=log10(count)),
              color="transparent",
              data=PredictedPeaks$bg.rect),
  first=list(dotID="38 neutro samples, 1 Input samples", chrom="chr16"))

## TODO:hoverselects!

info <- animint2HTML(viz)

getSorted <- function(){
  text.list <- getNodeSet(getHTML(), '//g[@class="geom1_text_oneChrom"]//text')
  value.vec <- sapply(text.list, xmlValue)
  sort(as.numeric(value.vec))
}

test_that("initially 2 text elements rendered", {
  num.vec <- getSorted()
  expect_equal(num.vec, c(1, 38))
})

clickID("chrM")

test_that("3 elements rendered (first time)", {
  num.vec <- getSorted()
  expect_equal(num.vec, c(1, 14, 38))
})

clickID("chrY")

clickID("chrM")

test_that("3 elements rendered (second time)", {
  num.vec <- getSorted()
  expect_equal(num.vec, c(1, 14, 38))
})


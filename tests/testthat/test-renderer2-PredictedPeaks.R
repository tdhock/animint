context("PredictedPeaks data set")

require(httr)
PredictedPeaks.RData <- file.path(tempdir(), "PredictedPeaks.RData")
request <- GET("http://github.com/tdhock/animint-examples/blob/master/data/PredictedPeaks.RData?raw=true")
stop_for_status(request)
writeBin(content(request), PredictedPeaks.RData)
load(PredictedPeaks.RData)

hover.dots <- subset(PredictedPeaks$chromCounts, nonInputType==type)

viz <- list(
  oneChrom=ggplot()+
    ggtitle("PeakSegJoint detections on selected chromosome")+
    theme_bw()+
    coord_cartesian(xlim=c(0, 1))+
    theme_animint(width=1500, height=100)+
    theme(axis.line.x=element_blank(), axis.text.x=element_blank(), 
          axis.ticks.x=element_blank(), axis.title.x=element_blank())+
    ## geom_text(aes(relative.middle, type.fac, label=samples.up,
    ##               clickSelects=peak.name,
    ##               showSelected2=chrom,
    ##               showSelected=dotID),
    ##           size=11,
    ##           data=PredictedPeaks$chromCounts)+
    geom_text(aes(relative.middle, type.fac, label=samples.up,
                  href=paste0(
                    "http://genome.ucsc.edu/cgi-bin/hgTracks?db=hg19&position=",
                    chrom, ":", zoomStart, "-", zoomEnd),
                  showSelected2=chrom,
                  showSelected=dotID),
              size=11,
              data=PredictedPeaks$chromCounts)+
    scale_y_discrete("cell type", drop=FALSE),
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

info <- animint2HTML(viz)

## Simulate mouseover using javascript?

## myScript <- 'myObj = document.getElementById("chrM");
## myArray = [];
## for(var b in myObj) { 
##   myArray.push(b);
## }
## return myArray;'
## remDr$executeScript(myScript)
## remDr$executeScript('return document.getElementById("chrM").onmouseover();')

## Simulate mouseover using RSelenium?

## e <- remDr$findElement("id", "chrM")
## remDr$mouseMoveToLocation(webElement=e)

## e <- remDr$findElement("id", "chrY")
## remDr$mouseMoveToLocation(webElement=e)

## getStyleValue(getHTML(), '//g[@class="geom4_point_chroms"]//circle', "opacity")

## getNodeSet(getHTML(), '//g[@class="geom4_point_chroms"]//circle')

test_that("without selectize option, only render chrom widget", {
  widget.vec <- getSelectorWidgets(info$html)
  expect_identical(widget.vec, "chrom")
})

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
Sys.sleep(1)

exp.vec <- c(1, 14, 38)

test_that("3 elements rendered (first time)", {
  num.vec <- getSorted()
  expect_equal(num.vec, exp.vec)
})

clickID("chrY")
Sys.sleep(1)

clickID("chrM")
Sys.sleep(1)

test_that("3 elements rendered (second time)", {
  num.vec <- getSorted()
  expect_equal(num.vec, exp.vec)
})

viz <- list(
  oneChrom=ggplot()+
    ggtitle("PeakSegJoint detections on selected chromosome")+
    theme_bw()+
    coord_cartesian(xlim=c(0, 1))+
    theme_animint(width=1500, height=100)+
    theme(axis.line.x=element_blank(), axis.text.x=element_blank(), 
          axis.ticks.x=element_blank(), axis.title.x=element_blank())+
    geom_text(aes(relative.middle, type.fac, label=samples.up,
                  clickSelects=peak.name,
                  showSelected2=chrom,
                  showSelected=dotID),
              size=11,
              data=PredictedPeaks$chromCounts)+
    scale_y_discrete("cell type", drop=FALSE),
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
              data=PredictedPeaks$bg.rect)+
   geom_point(aes(up, Input,
                  showSelected=peak.name),
              data=hover.dots),
  selectize=list(dotID=TRUE, chrom=FALSE),
  first=list(dotID="38 neutro samples, 1 Input samples", chrom="chr16"))

## TODO:href + hoverselects!

info <- animint2HTML(viz)

test_that("selectize option respected", {
  widget.vec <- getSelectorWidgets(info$html)
  expect_identical(widget.vec, "dotID")
})

## e <- remDr$findElement("class name", "show_hide_selector_widgets")
## e$clickElement()

## remDr$findElements("class name", "selectize-input")

## It takes a long time to render the selectize widget with many
## levels, why?

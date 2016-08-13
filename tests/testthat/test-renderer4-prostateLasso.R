acontext("prostateLasso data set")

data(prostateLasso)

variable.colors <- c(
  "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", 
  "#A65628", "#F781BF", "#999999")
hline.df <- data.frame(residual=0)
arclength <- prostateLasso$models$arclength
rect.width <- diff(arclength[1:2])/2
tallrect.all <- expand.grid(
  arclength.click=arclength,
  arclength.show=arclength)
addY <- function(dt, y){
  data.frame(dt, y.var=factor(y, c("error", "weights")))
}
viz.no.time <- list(
  title="both .variable .value aesthetics",
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
    geom_tallrect(aes(
      xmin=arclength.click-rect.width,
      xmax=arclength.click+rect.width,
      id=paste0("arclength", arclength.click),
      clickSelects.variable="arclength",
      clickSelects.value=arclength.click,
      showSelected.variable="arclength",
      showSelected.value=arclength.show,
      key=ifelse(
        arclength.click==arclength.show, 1,
        paste(arclength.click, arclength.show))),
      alpha=0.5,
      data=addY(tallrect.all, "error")),
  res=ggplot()+
    geom_hline(aes(yintercept=residual),
               data=hline.df,
               color="grey")+
    guides(linetype="none")+
    geom_point(aes(response, residual, 
                   key=observation.i,
                   showSelected=arclength),
               shape=21,
               fill=NA,
               color="black",
               data=prostateLasso$residuals)+
    geom_segment(aes(response, residual,
                     xend=response, yend=0,
                     linetype=set,
                     key=observation.i,
                     showSelected=arclength),
                 data=prostateLasso$residuals),
  first=list(arclength=max(arclength)),
  duration=list(arclength=5000))
info <- animint2HTML(viz.no.time)

clickID("arclength0")
Sys.sleep(1)
html.during <- getHTML()
Sys.sleep(5)
html.after <- getHTML()

getGreyX <- function(html){
  xpath <- '//g[@class="geom3_tallrect_path"]//rect'
  node.list <- getNodeSet(html, xpath)
  opacity.str <- getStyleValue(html, xpath, "opacity")
  opacity.num <- as.numeric(opacity.str)
  grey.i <- which(opacity.num == 0.5)
  grey.rect <- node.list[[grey.i]]
  attr.vec <- xmlAttrs(grey.rect)
  as.numeric(attr.vec[["x"]])
}

test_that("selected tallrect moves to the left", {
  x.before <- getGreyX(info$html)
  x.during <- getGreyX(html.during)
  expect_lt(x.during, x.before)
  x.after <- getGreyX(html.after)
  expect_lt(x.after, x.during)
})

viz.time <- viz.no.time
viz.time$time <- list(variable="arclength", ms=5000)

test_that("viz with time option compiles", {
  info <- animint2HTML(viz.time)
})


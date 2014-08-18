Context("Interactive facets")

## Example: 4 plots, 2 selectors.
data(intreg)
signal.colors <- c(estimate="#0adb0a", latent="#0098ef")
breakpoint.colors <- c("1breakpoint"="#ff7d7d", "0breakpoints"='#f6f4bf')
model.linetypes <- c(margin="dotted",limit="dashed",regression="solid")
intreg$annotations$logratio <- max(intreg$sig$log)

mmir.facets <- 
  list(signal=ggplot()+
       theme_animint(height=300, width=800)+       
       scale_x_continuous("position on chromosome (mega base pairs)",
                          breaks=c(100,200))+
       geom_tallrect(aes(xmin=first.base/1e6, xmax=last.base/1e6,
                         fill=annotation,
                         showSelected=signal),
                     data=intreg$annotations)+
       scale_fill_manual(values=breakpoint.colors,guide="none")+
       geom_text(aes((first.base+last.base)/2e6, logratio+1/8,
                     label=annotation,
                     showSelected=signal),
                 data=intreg$annotations)+
       geom_blank(aes(first.base/1e6, logratio+2/8),
                  data=intreg$annotations)+
       geom_point(aes(base/1e6, logratio,
                      showSelected=signal),
                  data=intreg$signals)+
       geom_segment(aes(first.base/1e6, mean, xend=last.base/1e6, yend=mean,
                        showSelected=signal,
                        showSelected2=segments),
                    data=intreg$segments,
                    colour=signal.colors[["estimate"]])+
       geom_vline(aes(xintercept=base/1e6,
                      showSelected=signal,
                      showSelected2=segments),
                  colour=signal.colors[["estimate"]],
                  linetype="dashed",
                  data=intreg$breaks),
       penalty=ggplot()+
       geom_tallrect(aes(xmin=min.L, xmax=max.L,
                         showSelected=signal,
                         clickSelects=segments),
                     data=data.frame(intreg$selection, what="segments"),
                     alpha=1/2)+
       ylab("")+
       theme_animint(height=500, width=800)+
       geom_segment(aes(min.L, feature, xend=max.L, yend=feature,
                        clickSelects=signal),
                    size=5,
                    data=data.frame(intreg$intervals, what="regression"))+
       geom_segment(aes(min.L, min.feature, xend=max.L, yend=max.feature,
                        linetype=line),
                    colour="red",
                    size=3,
                    data=data.frame(intreg$model, what="regression"))+
       scale_linetype_manual(values=model.linetypes)+
       geom_segment(aes(min.L, cost, xend=max.L, yend=cost,
                        showSelected=signal),
                    data=data.frame(intreg$selection, what="error"))+
       geom_segment(aes(min.L, segments, xend=max.L, yend=segments,
                        showSelected=signal),
                    data=data.frame(intreg$selection, what="segments"))+
       xlab("penalty value $L=f(x)$")+ # TODO: mathjax.
       facet_grid(what~.,scales="free"))

info <- animint2HTML(mmir.facets)

getTransform <- function(tick)xmlAttrs(tick)[["transform"]]

test_that("y axes align in facet_grid(variable~.)", {
  g.list <- getNodeSet(info$html, "//svg[@id='penalty']//g[@id='yaxis']")
  expect_equal(length(g.list), 3)
  transform.txt <- sapply(g.list, getTransform)
  x.txt <- sub("translate[(](.*?),.*", "\\1", transform.txt)
  for(x.val in x.txt){
    expect_equal(x.val, x.txt[[1]])
  }
})

test_that("red lines are only drawn in panel 2", {
  panelPath <- function(panel.i){
    paste0("//svg[@id='penalty']",
           "//g[@class='geom9_segment_penalty']",
           "//g[@class='PANEL", panel.i, "']",
           "//line")
  }
  PANEL1lines <- getNodeSet(info$html, panelPath(1))
  expect_equal(length(PANEL1lines), 0)
  PANEL2lines <- getNodeSet(info$html, panelPath(2))
  expect_equal(length(PANEL2lines), 6)
  PANEL3lines <- getNodeSet(info$html, panelPath(3))
  expect_equal(length(PANEL3lines), 0)
})

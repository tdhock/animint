acontext("mixtureKNN data set")

data(mixtureKNN)

mixtureKNN$Bayes.error$text.V1.prop <- 0
mixtureKNN$Bayes.error$text.V2.bottom <- -2
mixtureKNN$other.error$text.V1.prop <- 0
mixtureKNN$Bayes.error$text.V1.error <- -2.6
mixtureKNN$other.error$text.V1.error <- -2.6
classifier.linetypes <- c(
  Bayes="dashed",
  KNN="solid")
label.colors <- c(
  "0"="#377EB8",
  "1"="#FF7F00")
set.colors <-
  c(test="#984EA3",#purple
    validation="#4DAF4A",#green
    Bayes="#984EA3",#purple
    train="black")

errorPlot <- ggplot()+
  ggtitle("Select number of neighbors")+
  theme_bw()+
  theme_animint(height=500)+
  geom_text(aes(min.neighbors, error.prop,
                color=set, label="Bayes",
                showSelected=classifier),
            hjust=1,
            data=mixtureKNN$Bayes.segment)+
  geom_segment(aes(min.neighbors, error.prop, 
                   xend=max.neighbors, yend=error.prop,
                   color=set,
                   showSelected=classifier, linetype=classifier),
               data=mixtureKNN$Bayes.segment)+
  scale_color_manual(values=set.colors, breaks=names(set.colors))+
  scale_fill_manual(values=set.colors)+
  guides(fill="none", linetype="none")+
  scale_linetype_manual(values=classifier.linetypes)+
  ylab("Misclassification Errors")+
  scale_x_continuous(
    "Number of Neighbors",
    limits=c(-1, 30),
    breaks=c(1, 10, 20, 29))+
  geom_ribbon(aes(neighbors, ymin=mean-sd, ymax=mean+sd,
                  fill=set,
                  showSelected=classifier,
                  showSelected2=set),
              alpha=0.5,
              data=mixtureKNN$validation.error)+
  geom_line(aes(neighbors, mean, color=set,
                showSelected=classifier, linetype=classifier),
            data=mixtureKNN$validation.error)+
  geom_line(aes(neighbors, error.prop, group=set, color=set,
                showSelected=classifier, linetype=classifier),
            data=mixtureKNN$other.error)+
  geom_tallrect(aes(xmin=neighbors-1, xmax=neighbors+1,
                    clickSelects=neighbors),
                alpha=0.5,
                data=mixtureKNN$validation.error)
errorPlot

scatterPlot <- ggplot()+
  ggtitle("Mis-classification errors in train set")+
  theme_bw()+
  theme_animint(width=500, height=500)+
  xlab("Input feature 1")+
  ylab("Input feature 2")+
  coord_equal()+
  scale_color_manual(values=label.colors)+
  scale_linetype_manual(values=classifier.linetypes)+
  geom_point(aes(V1, V2, color=label,
                 showSelected=neighbors),
             size=0.2,
             data=mixtureKNN$show.grid)+
  geom_path(aes(V1, V2, group=path.i, linetype=classifier,
                showSelected=neighbors),
            size=1,
            data=mixtureKNN$pred.boundary)+
  geom_path(aes(V1, V2, group=path.i, linetype=classifier),
            color=set.colors[["test"]],
            size=1,
            data=mixtureKNN$Bayes.boundary)+
  geom_point(aes(V1, V2, color=label,
                 fill=prediction,
                 showSelected=neighbors),
             size=3,
             shape=21,
             data=mixtureKNN$show.points)+
  scale_fill_manual(values=c(error="black", correct="transparent"))+
  geom_text(aes(text.V1.error, text.V2.bottom, label=paste(set, "Error:")),
            data=mixtureKNN$Bayes.error,
            hjust=0)+
  geom_text(aes(text.V1.prop, text.V2.bottom, label=sprintf("%.3f", error.prop)),
            data=mixtureKNN$Bayes.error,
            hjust=1)+
  geom_text(aes(text.V1.error, V2.bottom, label=paste(set, "Error:"),
                showSelected=neighbors),
            data=mixtureKNN$other.error,
            hjust=0)+
  geom_text(aes(text.V1.prop, V2.bottom, label=sprintf("%.3f", error.prop),
                showSelected=neighbors),
            data=mixtureKNN$other.error,
            hjust=1)+
  geom_text(aes(V1, V2,
                showSelected=neighbors,
                label=paste0(
                  neighbors,
                  " nearest neighbor",
                  ifelse(neighbors==1, "", "s"),
                  " classifier")),
            data=mixtureKNN$show.text)
scatterPlot+
  facet_wrap("neighbors")+
  theme(panel.margin=grid::unit(0, "lines"))

viz.neighbors <- list(
  error=errorPlot,
  data=scatterPlot,
  first=list(neighbors=7)
  )
info <- animint2HTML(viz.neighbors)

test_that("1 <path> rendered for validation error band", {
  path.list <- getNodeSet(info$html, "//g[@class='geom3_ribbon_error']//path")
  expect_equal(length(path.list), 1)
})

test_that("1 <path> rendered for validation error mean", {
  path.list <- getNodeSet(info$html, "//g[@class='geom4_line_error']//path")
  expect_equal(length(path.list), 1)
})

test_that("2 <path> rendered for train/test error", {
  path.list <- getNodeSet(info$html, "//g[@class='geom5_line_error']//path")
  expect_equal(length(path.list), 2)
})

test_that("1 <line> rendered for Bayes error", {
  line.list <- getNodeSet(info$html, "//g[@class='geom2_segment_error']//line")
  expect_equal(length(line.list), 1)
  rect.list <- getNodeSet(
    info$html, "//svg[@id='plot_error']//rect[@class='border_rect']")
  expect_equal(length(rect.list), 1)
  rect.attr.vec <- xmlAttrs(rect.list[[1]])
  rect.x <- as.numeric(rect.attr.vec[["x"]])
  rect.width <- as.numeric(rect.attr.vec[["width"]])
  rect.right <- rect.x + rect.width
  line.attr.vec <- xmlAttrs(line.list[[1]])
  line.x2 <- as.numeric(line.attr.vec[["x2"]])
  expect_less_than(line.x2, rect.right)
})

test_that("6 <path> rendered for KNN boundary", {
  path.list <- getNodeSet(info$html, "//g[@class='geom8_path_data']//path")
  expect_equal(length(path.list), 6)
})

test_that("2 <path> rendered for Bayes boundary", {
  path.list <- getNodeSet(info$html, "//g[@class='geom9_path_data']//path")
  expect_equal(length(path.list), 2)
})

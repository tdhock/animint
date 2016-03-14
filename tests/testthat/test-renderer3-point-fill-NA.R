acontext("point fill NA")

##dput(RColorBrewer::brewer.pal(Inf, "Set1"))
species.colors <-
  c(versicolor="#E41A1C",
    setosa="#377EB8",
    virginica="#4DAF4A", "#984EA3",
    "#FF7F00", "#FFFF33", 
    "#A65628", "#F781BF", "#999999")
viz <- list(
  petals=ggplot()+
    scale_color_manual(values=species.colors)+
    geom_point(aes(Petal.Length, Petal.Width, color=Species),
               fill=NA,
               shape=21,
               data=iris)
  )

test_that("geom_point(aes(color), fill=NA) renders fill transparent", {
  info <- animint2HTML(viz)
  style.mat <- getStyleValue(
    info$html, '//g[@class="geom1_point_petals"]//circle', c("fill", "stroke"))
  expected.stroke <- species.colors[paste(iris$Species)]
  expect_color(style.mat["stroke", ], expected.stroke)
  expected.fill <- rep("transparent", nrow(iris))
  computed.fill <- style.mat["fill", ]
  ##print(rbind(computed.fill, expected.fill))
  expect_color(computed.fill, expected.fill)
})

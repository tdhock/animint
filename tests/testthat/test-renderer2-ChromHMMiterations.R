acontext("ChromHMMiterations data set")

data(ChromHMMiterations, package = "animint")

emission <- data.frame(ChromHMMiterations$emission, parameters="emission")
transition <- data.frame(ChromHMMiterations$transition, parameters="transition")

viz <- list(
  parameters=ggplot()+
    ggtitle("parameters at selected iteration")+
    scale_fill_gradient(low="white", high="blue")+
    geom_tile(aes(state, experiment, fill=frequency,
                  key=paste(state, experiment),
                  showSelected=iteration),
              data=emission)+
    scale_color_gradient(low="white", high="red")+
    theme_bw()+
    theme_animint(height=600, width=350)+
    theme(panel.margin=grid::unit(0, "cm"))+
    facet_grid(parameters ~ .,
               space="free",
               scales="free_y")+
    scale_y_discrete(drop=FALSE)+
    geom_point(aes(state.to, state.from, color=probability,
                  key=paste(state.from, state.to),
                  showSelected=iteration),
               size=10,
               data=transition),
  metrics=ggplot()+
    ggtitle("convergence metrics, select iteration")+
    make_tallrect(ChromHMMiterations$metrics, "iteration")+
    geom_line(aes(iteration, metric.value),
              data=ChromHMMiterations$metrics)+
    theme_bw()+
    theme_animint(height=500)+
    theme(panel.margin=grid::unit(0, "cm"))+
    facet_grid(metric.name ~ ., scales="free_y"),
  duration=list(iteration=500),
  first=list(iteration=100),
  title="ChromHMM parameter fitting for one iPS sample")

expect_no_warning({
  info <- animint2HTML(viz)
})

test_that("no vertical space between border_rects", {
  rect.list <- getNodeSet(
    info$html, '//svg[@id="plot_parameters"]//rect[@class="border_rect"]')
  expect_equal(length(rect.list), 2)
  first <- xmlAttrs(rect.list[[1]])
  first.bottom <- as.numeric(first[["y"]])+as.numeric(first[["height"]])
  second <- xmlAttrs(rect.list[[2]])
  second.top <- as.numeric(second[["y"]])
  expect_equal(first.bottom, second.top)
})

test_that("fill not constant in probability legend and circles", {
  fill.vec <- getStyleValue(
    info$html, '//svg[@id="plot_parameters"]//circle', "fill")
  expect_true(1 < length(table(fill.vec)))
  fill.vec <- getStyleValue(
    info$html, '//tr[@class="probability_variable"]//circle', "fill")
  expect_true(1 < length(table(fill.vec)))
})




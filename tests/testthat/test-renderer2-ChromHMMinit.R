acontext("ChromHMMinit data set")

require(httr)
ChromHMMinit.RData <- file.path(tempdir(), "ChromHMMinit.RData")
request <- GET("http://github.com/tdhock/animint-examples/blob/master/data/ChromHMMinit.RData?raw=true")
stop_for_status(request)
writeBin(content(request), ChromHMMinit.RData)
## If we don't load this data set into the global environment, then we
## get Error in eval(expr, envir, enclos) (from helper-functions.R#5)
## : object 'ChromHMMinit' not found
load(ChromHMMinit.RData, .GlobalEnv) 

last.iteration <- subset(ChromHMMinit$metrics, iteration==100)

viz <- list(
  parameters=ggplot()+
    ggtitle("parameters at selected iteration")+
    scale_fill_gradient(low="white", high="blue")+
    geom_tile(aes(state, experiment, fill=frequency,
                  key=paste(state, experiment),
                  showSelected=repeat.fac,
                  showSelected2=iteration),
              ##chunk_vars=c("repeat.fac"),
              data=data.table(ChromHMMinit$emission, parameters="emission"))+
    scale_color_gradient(low="white", high="red")+
    theme_bw()+
    theme_animint(height=500, width=400)+
    theme(panel.margin=grid::unit(0, "cm"))+
    facet_grid(parameters ~ .,
               space="free",
               scales="free_y")+
    scale_y_discrete(drop=FALSE)+
    geom_point(aes(state.to, state.from, color=probability,
                   key=paste(state.from, state.to),
                   showSelected=repeat.fac,
                   showSelected2=iteration),
               size=8,
               ##chunk_vars=c("repeat.fac"),
               data=data.frame(ChromHMMinit$transition,
                 parameters="transition")),
  metrics=ggplot()+
    ggtitle("convergence metrics, select iteration")+
    make_tallrect(ChromHMMinit$metrics, "iteration")+
    geom_line(aes(iteration, metric.value,
                  clickSelects=repeat.fac,
                  group=repeat.fac),
              size=3,
              alpha=0.6,
              data=subset(ChromHMMinit$metrics, metric.name != "Change"))+
    theme_bw()+
    theme_animint(height=500)+
    theme(panel.margin=grid::unit(0, "cm"))+
    facet_grid(metric.name ~ ., scales="free_y"),
  last=ggplot()+
    ggtitle("last iteration, select initialization")+
    theme_bw()+
    theme_animint(height=500, width=400)+
    theme(panel.margin=grid::unit(0, "cm"))+
    facet_grid(metric.name ~ ., space="fixed", scales="free")+
    geom_point(aes(repeat.fac, metric.value,
                   clickSelects=repeat.fac),
               size=5,
               data=subset(last.iteration, metric.name != "Change"))+
    scale_x_discrete("random initialization")+
    scale_y_continuous(""),
  duration=list(iteration=500),
  first=list(iteration=100),
  time=list(variable="iteration", ms=500),
  title="10 ChromHMM fits for 6 experiments on one iPS sample")

system.time({
  info <- animint2HTML(viz) #TODO: why does this take so long?
})

getTileFill <- function(html=getHTML()){
  getStyleValue(html, '//g[@class="geom1_tile_parameters"]//rect', "fill")
}

test_that("animation starts by default", {
  initial.fill.vec <- getTileFill()
  Sys.sleep(5)
  updated.fill.vec <- getTileFill()
  n.same <- sum(initial.fill.vec == updated.fill.vec)
  expect_less_than(n.same, length(updated.fill.vec))
})

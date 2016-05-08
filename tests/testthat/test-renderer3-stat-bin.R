acontext("stat bin")

set.seed(1)
make <- function(count, stack, facet){
  data.frame(count, row=1:count, stack, facet)
}
df <- rbind(
  make(2, 1, 1),
  make(5, 1, 1),
  make(3, 2, 1),
  make(4, 2, 1),
  make(2, 2, 2),
  make(5, 2, 2),
  make(3, 1, 2),
  make(4, 1, 2)
)

test_that("warning for stat=bin and showSelected", {
  gg <- ggplot() +
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    geom_bar(
      aes(count, group=stack, fill=stack, showSelected=facet),
      binwidth=1,
      data = df,
      stat = "bin",
      position="identity"
    )
  gg+facet_grid(facet~.)
  complicated <- list(
    plot = gg
  )
  expect_warning({
    info <- animint2HTML(complicated)
  }, "showSelected only works with position=identity and stat=identity")
  xpath <- '//g[@class="geom1_bar_plot"]//rect'
  style.vec <- getStyleValue(info$html, xpath, "fill")
  fill.counts <- table(style.vec)
  expect_equal(length(fill.counts), 2)
  expect_true(all(fill.counts==2))
})

test_that("no warning for stat=bin without showSelected", {
  gg <- ggplot() +
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    geom_bar(
      aes(count, group=stack, fill=stack),
      binwidth=1,
      data = df,
      stat = "bin",
      position="identity"
    )+
    facet_grid(facet~.)
  complicated <- list(plot = gg)
  expect_no_warning({
    info <- animint2HTML(complicated)
  })
  for(panel in 1:2){
    xpath <- sprintf('//g[@class="PANEL%d"]//rect', panel)
    style.vec <- getStyleValue(info$html, xpath, "fill")
    fill.counts <- table(style.vec)
    expect_equal(length(fill.counts), 2)
    expect_true(all(fill.counts==2))
  }
})

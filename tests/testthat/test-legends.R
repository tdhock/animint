context("legends")

data(WorldBank, package="animint")
breaks <- 10^(4:9)
viz <-
  list(ts=ggplot()+
       make_tallrect(WorldBank, "year")+
       geom_line(aes(year, life.expectancy, group=country, colour=region,
                     clickSelects=country),
                 data=WorldBank, size=3, alpha=3/5),
       scatter=ggplot()+
       geom_point(aes(fertility.rate, life.expectancy, clickSelects=country,
                      showSelected=year, colour=region, size=population),
                  data=WorldBank)+
       geom_text(aes(fertility.rate, life.expectancy, label=country,
                     showSelected=country, showSelected2=year),
                 data=WorldBank)+
       make_text(WorldBank, 5, 80, "year")+
       scale_size_animint(breaks=breaks))

test_that('breaks are respected', {
  info <- animint2dir(viz, open.browser=FALSE)
  entries <- info$plots$scatter$legend$population$entries
  label.chr <- sapply(entries, "[[", "label")
  label.num <- as.numeric(label.chr)
  expect_equal(sort(label.num), sort(breaks))
})

test_that('hiding both legends works with geom_point(show_guide=FALSE)', {
  viz$scatter <- ggplot()+
    geom_point(aes(fertility.rate, life.expectancy, clickSelects=country,
                   showSelected=year, colour=region, size=population),
               data=WorldBank, show_guide=FALSE)+
    geom_text(aes(fertility.rate, life.expectancy, label=country,
                  showSelected=country, showSelected2=year),
              data=WorldBank)+
    make_text(WorldBank, 5, 80, "year")
  info <- animint2dir(viz, open.browser=FALSE)
  generated.names <- names(info$plots$scatter$legend)
  expect_identical(length(generated.names), 0L)
})

test_that('hiding the color legend works with scale_color(guide="none")',{
  viz$scatter <- viz$scatter+
    scale_color_discrete(guide="none")
  info <- animint2dir(viz, open.browser=FALSE)
  generated.names <- names(info$plots$scatter$legend)
  expect_identical(generated.names, "population")
})

test_that('hiding the color legend works with guides(color="none")',{
  viz$scatter <- viz$scatter+
    guides(color="none")
  info <- animint2dir(viz, open.browser=FALSE)
  generated.names <- names(info$plots$scatter$legend)
  expect_identical(generated.names, "population")
})

test_that('hiding all legends works with theme(legend.position="none")',{
  viz$scatter <- viz$scatter+
    theme(legend.position="none")
  info <- animint2dir(viz, open.browser=FALSE)
  generated.names <- names(info$plots$scatter$legend)
  expect_identical(generated.names, NULL)
})

gg <- 
  ggplot(df)+
    geom_point(aes(x, x))+
    geom_tallrect(aes(xmin=x, xmax=x+0.5, fill=x),
                  color="black")

expected.legend.list <- 
  list(increasing=1:3,
       default=seq(1, 3, by=0.5),
       decreasing=3:1)
    
test_that("renderer shows legend entries in correct order", {
  viz <-
    list(increasing=gg+
           scale_fill_continuous(breaks=1:3),
         decreasing=gg+
           scale_fill_continuous(breaks=3:1),
         default=gg)
  info <- animint2HTML(viz)
  ## NOTE: it is important to test the renderer here (not the
  ## compiler) since maybe the order specified in the plot.json file
  ## is not the same as the order of appearance on the web page.

  ## The expected behavior is smaller numeric entries on the bottom of
  ## the legend by default, and if they are manually specified via
  ## breaks, we have:
  breaks <-
    c("top",
      "middle",
      "bottom")
  for(plot.name in names(expected.legend.list)){
    xpath <-
      sprintf('//td[@id="%s_legend"]//td[@class="legend_entry_label"]',
              plot.name)
    expected.entries <- expected.legend.list[[plot.name]]
    node.set <- getNodeSet(info$html, xpath)
    value.str <- sapply(node.set, xmlValue)
    value.num <- as.numeric(value.str)
    expect_equal(value.num, expected.entries)
  }
})

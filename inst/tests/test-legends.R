context("legends")

data(WorldBank)
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
  info <- gg2animint(viz, open.browser=FALSE)
  entries <- info$plots$scatter$legend$population$entries
  label.chr <- sapply(entries, "[[", "label")
  label.num <- as.numeric(label.chr)
  expect_equal(sort(label.num), sort(breaks))
})

test_that('hiding both legends works with geom_point(show_guide=FALSE)', {
  viz$scatter=ggplot()+
       geom_point(aes(fertility.rate, life.expectancy, clickSelects=country,
                      showSelected=year, colour=region, size=population),
                  data=WorldBank, show_guide=FALSE)+
       geom_text(aes(fertility.rate, life.expectancy, label=country,
                     showSelected=country, showSelected2=year),
                 data=WorldBank)+
       make_text(WorldBank, 5, 80, "year")
  info <- gg2animint(viz, open.browser=FALSE)
  generated.names <- names(info$plots$scatter$legend)
  expect_identical(length(generated.names), 0L)
})

test_that('hiding the color legend works with scale_color(guide="none")',{
  viz$scatter <- viz$scatter+
    scale_color_discrete(guide="none")
  info <- gg2animint(viz, open.browser=FALSE)
  generated.names <- names(info$plots$scatter$legend)
  expect_identical(generated.names, "population")
})

test_that('hiding the color legend works with guides(color="none")',{
  viz$scatter <- viz$scatter+
    guides(color="none")
  info <- gg2animint(viz, open.browser=FALSE)
  generated.names <- names(info$plots$scatter$legend)
  expect_identical(generated.names, "population")
})

test_that('hiding all legends works with theme(legend.position="none")',{
  viz$scatter <- viz$scatter+
    theme(legend.position="none")
  info <- gg2animint(viz, open.browser=FALSE)
  generated.names <- names(info$plots$scatter$legend)
  expect_identical(generated.names, NULL)
})


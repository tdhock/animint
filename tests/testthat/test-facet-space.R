context('facet_grid(space="free")')

no.panels <- ggplot(mtcars, aes(mpg, wt)) + 
  geom_point(colour='grey50', size = 4) + 
  geom_point(aes(colour = cyl)) 

viz <-
  list(freeBoth = no.panels +
         facet_grid(.~am, space = "free", scales = "free", labeller=label_both),
       freeScale = no.panels +
         facet_grid(.~am, scales="free", labeller=label_both),
       fixed = no.panels +
         facet_grid(.~am, labeller=label_both))

info <- animint2HTML(viz)

getTextValue <- function(tick)xmlValue(getNodeSet(tick, "text")[[1]])
getTransform <- function(tick)xmlAttrs(tick)[["transform"]]
get1520diff <- function(xaxis){
  g.ticks <- getNodeSet(xaxis, "g[@class='tick major']")
  tick.labels <- sapply(g.ticks, getTextValue)
  names(g.ticks) <- tick.labels
  tick.transform <- sapply(g.ticks[c("15", "20")], getTransform)
  x.txt <- sub("translate[(](.*?),.*", "\\1", tick.transform)
  x.num <- as.numeric(x.txt)
  diff(x.num)
}
both.equal <- function(x){
  if(is.null(x) || !is.vector(x) || length(x) != 2){
    return(FALSE)
  }
  isTRUE(all.equal(x[[1]], x[[2]]))
}

test_that("each plot has two x axes and 1 y axis", {
  for(plot.name in names(viz)){
    svg.xpath <- sprintf("//svg[@id='%s']", plot.name)
    x.xpath <- paste0(svg.xpath, "//g[@id='xaxis']")
    x.axes <- getNodeSet(info$html, x.xpath)
    expect_equal(length(x.axes), 2)
    y.xpath <- paste0(svg.xpath, "//g[@id='yaxis']")
    y.axes <- getNodeSet(info$html, y.xpath)
    expect_equal(length(y.axes), 1)
  }
})

test_that("each plot has only one x axis label", {
  for(plot.name in names(viz)){
    svg.xpath <- sprintf("//svg[@id='%s']", plot.name)
    text.xpath <- paste0(svg.xpath, "//text[@id='xname']")
    x.labels <- getNodeSet(info$html, text.xpath)
    expect_equal(length(x.labels), 1)
  }
})

test_that("top strips present in each plot", {
  for(plot.name in names(viz)){
    xpath <- sprintf("//svg[@id='%s']//g[@id='top_strip']//text", plot.name)
    strip.text <- getNodeSet(info$html, xpath)
    expect_equal(length(strip.text), 2)
    text.values <- sapply(strip.text, xmlValue)
    expect_match(text.values, "am: 0", all=FALSE)
    expect_match(text.values, "am: 1", all=FALSE)
  }
})

test_that("pixels between 15 and 20 is constant or variable", {
  ## scale="fixed" means the distance between ticks 15 and 20 should
  ## be the same across the 2 panels.
  browser()
  x.axes <- getNodeSet(info$html, "//svg[@id='fixed']//g[@id='xaxis']")
  xdiff <- lapply(x.axes, get1520diff)
  expect_true(both.equal(xdiff))
  
  ## scale="free" means the distance between ticks 15 and 20 should
  ## be different across the 2 panels.
  x.axes <- getNodeSet(info$html, "//svg[@id='freeScale']//g[@id='xaxis']")
  xdiff <- lapply(x.axes, get1520diff)
  expect_true(!both.equal(xdiff))
  
  ## scale="free" and space="free" means the distance between ticks 15
  ## and 20 should be the same across the 2 panels.
  x.axes <- getNodeSet(info$html, "//svg[@id='freeBoth']//g[@id='xaxis']")
  xdiff <- lapply(x.axes, get1520diff)
  expect_true(both.equal(xdiff))
})


test_that("width_proportion is constant or variable", {
  expect_true(both.equal(info$plots$fixed$layout$width_proportion))
  expect_true(both.equal(info$plots$freeScale$layout$width_proportion))
  expect_true(!both.equal(info$plots$freeBoth$layout$width_proportion))
})

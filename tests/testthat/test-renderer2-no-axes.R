acontext("no axes")

viz <- list(
  gg=ggplot()+
    geom_point(aes(Petal.Length, Sepal.Length),
               data=iris)+
    theme_bw()+
    theme(axis.line=element_blank(), axis.text=element_blank(), 
          axis.ticks=element_blank(), axis.title=element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank())
  )

test_that("axes hidden", {
  info <- animint2HTML(viz)
  for(class.prefix in c("background", "border")){
    xpath <- sprintf('//rect[@class="%s_rect"]', class.prefix)
    rect.list <- getNodeSet(info$html, xpath)
    expect_equal(length(rect.list), 0)
  }
  for(contains.value in c("tick", "axis")){
    xpath <- sprintf('//g[contains(@class,"%s")]', contains.value)
    element.list <- getNodeSet(info$html, xpath)
    expect_equal(length(element.list), 0)
  }
  for(contains.value in c("xtitle", "ytitle")){
    xpath <- sprintf('//text[contains(@class,"%s")]', contains.value)
    element.list <- getNodeSet(info$html, xpath)
    expect_equal(length(element.list), 0)
  }
})

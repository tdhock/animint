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
  info <- animint2dir(viz)
  for(class.prefix in c("background", "border")){
    xpath <- sprintf('//svg[@id="gg"]//rect[@class="%s_rect"]', class.prefix)
    rect.list <- getNodeSet(info$html, xpath)
    expect_equal(length(rect.list), 0)
  }
})

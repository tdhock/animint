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
  ec <- function(element, class){
    data.frame(element, class)
  }
  elem.df <- rbind(
    ec("rect", paste0(c("background","border"), "_rect")),
    ec("g", "axis"),
    ec("path", "domain"),
    ec("text", paste0(c("x", "y"), "title")))
  for(elem.i in seq_along(elem.df$element)){
    xpath <- with(elem.df[elem.i, ], {
      sprintf('//%s[@class="%s"]', element, class)
    })
    element.list <- getNodeSet(info$html, xpath)
    expect_equal(length(element.list), 0)
  }
})

context("linetype")

error.types <-
  data.frame(x=1:3, status=c("correct", "false positive", "false negative"))

gg <- 
  ggplot(error.types)+
    geom_point(aes(x, x))+
    geom_tallrect(aes(xmin=x, xmax=x+0.5, linetype=status),
                  fill="grey",
                  color="black")

dasharrayPattern <-
  paste0("stroke-dasharray:",
         "(?<value>.*?)",
         ";")

rect.xpaths <- 
  c('//svg[@id="numeric"]//rect',
    '//svg[@id="character"]//rect',
    '//td[@id="numeric_legend"]//rect',
    '//td[@id="character_legend"]//rect')

test_that("linetypes render correctly", {
  viz <-
    list(numeric=gg+
           scale_linetype_manual(values=c(correct=0,
                                   "false positive"=1,
                                   "false negative"=3),
                                limits=c("correct", "false positive",
                                 "false negative")),

         character=gg+scale_linetype_manual(values=c(correct="blank",
                                              "false positive"="solid",
                                              "false negative"="dotted"),
                                limits=c("correct", "false positive",
                                 "false negative")))
  info <- animint2HTML(viz)

  for(xpath in rect.xpaths){
    node.set <- getNodeSet(info$html, xpath)
    expect_equal(length(node.set), 3)
    attr.mat <- sapply(node.set, xmlAttrs)
    match.mat <- str_match_perl(attr.mat["style",], dasharrayPattern)
    value.vec <- match.mat[, "value"]
    expect_match(value.vec[1], "0(px)?, *20(px)?")
    expect_identical(value.vec[2], NA_character_)
    expect_match(value.vec[3], "2(px)?, *4(px)?")
  }
})

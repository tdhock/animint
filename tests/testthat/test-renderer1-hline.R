library(testthat)
acontext("hline")

n.rows <- 100
df <- data.frame(x=rnorm(n.rows), y=rnorm(n.rows))
sc <- ggplot()+
  geom_point(aes(x, y), data=df)
two <- data.frame(x=c(0, 1))

viz <-
  list(one=sc+
         geom_hline(yintercept=0)+
         geom_vline(xintercept=0),

       two=sc+
         geom_hline(aes(yintercept=x), data=two)+
         geom_vline(aes(xintercept=x), data=two))

info <- animint2HTML(viz)

line.class.vec <-
  c("geom2_hline_one"=1, "geom3_vline_one"=1,
    "geom5_hline_two"=2, "geom6_vline_two"=2)

test_that("hlines and vlines appear", {
  for(line.class in names(line.class.vec)){
    xpath <- sprintf('//g[@class="%s"]//line', line.class)
    line.list <- getNodeSet(info$html, xpath)
    expected.length <- line.class.vec[[line.class]]
    expect_equal(length(line.list), expected.length)
    attr.mat <- sapply(line.list, xmlAttrs)
    xy.mat <- attr.mat[c("x1", "x2", "y1", "y2"), ]
    xy.vec <- as.numeric(xy.mat)
    expect_true(all(is.finite(xy.vec)))
  }
})

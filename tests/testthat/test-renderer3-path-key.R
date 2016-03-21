acontext("path key")

path.list <- list()
N <- 100
x <- 1:N
point <- data.frame(
  showSelected.i=1:2)
set.seed(1)
for(group.i in 1:2){
  for(showSelected.i in point$showSelected.i){
    path.list[[paste(group.i, showSelected.i)]] <-
      data.frame(group.i, showSelected.i, x, y=rnorm(N, group.i))
  }
}
path <- do.call(rbind, path.list)

viz <- list(
  point=ggplot()+
    geom_point(aes(showSelected.i, showSelected.i,
                   id=paste("point", showSelected.i),
                   clickSelects=showSelected.i),
               size=10,
               data=point),
  transition=ggplot()+
    ggtitle("should have animated transition")+
    geom_path(aes(x, y, group=group.i, color=group.i,
                  key=group.i,
                  showSelected=showSelected.i),
              data=path),
  noTransition=ggplot()+
    ggtitle("should NOT have animated transition")+
    geom_path(aes(x, y, group=group.i, color=group.i,
                  key=paste(group.i, showSelected.i),
                  showSelected=showSelected.i),
              data=path),
  first=list(showSelected.i="1"),
  duration=list(showSelected.i=3000))

info <- animint2HTML(viz)

getD <- function(html=getHTML()){
  node.list <- getNodeSet(html, '//g[@class="PANEL1"]//path')
  node.mat <- sapply(node.list, xmlAttrs)
  node.mat["d",]
}

test_that("transitions only for equivalent keys", {
  d.before <- getD()
  clickID("point2")
  Sys.sleep(1)
  d.during <- getD()
  Sys.sleep(3)
  d.after <- getD()
  expect_identical(d.before == d.during, c(FALSE, FALSE, FALSE, FALSE))
  expect_identical(d.during == d.after, c(FALSE, FALSE, TRUE, TRUE))
})

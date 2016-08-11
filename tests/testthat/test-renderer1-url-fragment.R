acontext("url fragment identifier")

iris$id <- 1:nrow(iris)
viz <-
  list(petal=ggplot()+
         geom_point(aes(Petal.Width, Petal.Length, fill=Species,
                        clickSelects=id), data=iris),
       sepal=ggplot()+
         geom_point(aes(Sepal.Width, Sepal.Length, fill=Species,
                        clickSelects=id), data=iris),
       title="Iris data")

old_address <- remDr$getCurrentUrl()[[1]]
new_address=paste(old_address,'#Species={setosa}',sep = '')
test_that("selection through url fragment is working", {
  info <- animint2HTML(viz)
  remDr$navigate(new_address)
  remDr$refresh()
  Sys.sleep(10)
  html <- getHTML()
  nodes <- getNodeSet(html, "//div[@id='plot']//table[@class='legend']//td[@style='opacity: 1;']")
  expect_true(length(nodes) == 2)
})
remDr$navigate(old_address)

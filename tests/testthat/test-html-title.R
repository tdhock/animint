context("HTML page title")

iris$id <- 1:nrow(iris)
viz <-
  list(petal=ggplot()+
       geom_point(aes(Petal.Width, Petal.Length, fill=Species,
                      clickSelects=id), data=iris),
       sepal=ggplot()+
       geom_point(aes(Sepal.Width, Sepal.Length, fill=Species,
                      clickSelects=id), data=iris),
       title="Iris data")

test_that("title option converted to <title>", {
  info <- animint2HTML(viz)
  nodes <- getNodeSet(info$html, "//title")
  generated.title <- xmlValue(nodes[[1]])
  expect_match(generated.title, "Iris data")
})


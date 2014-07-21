library(animint)
iris$id <- 1:nrow(iris)
viz <-
  list(petal=ggplot()+
       geom_point(aes(Petal.Width, Petal.Length, fill=Species,
                      clickSelects=id),
                  data=iris),
       sepal=ggplot()+
       geom_point(aes(Sepal.Width, Sepal.Length, fill=Species,
                      clickSelects=id),
                  data=iris))
gg2animint(viz, "iris")

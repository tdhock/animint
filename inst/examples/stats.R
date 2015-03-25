library(animint)

my.iris <- iris
my.iris$id <- 1:nrow(my.iris)
my.iris$individuals <- 1

viz <-
  list(scatter=ggplot()+
       geom_point(aes(Sepal.Length, Petal.Length, clickSelects=id),
                  data=my.iris, alpha=6/10),

       selector.types=list(id="multiple"),

       counts=ggplot()+
       geom_point(aes(Species, individuals, showSelected=id),
### TODO: compute this stat in the renderer, to see the nummber of
### currently selected points in each of the 3 classes.
                  stat="summary",
                  fun.y="sum",
                  data=my.iris))

animint2dir(viz)

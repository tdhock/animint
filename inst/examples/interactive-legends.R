
library(ggplot2)
library(animint)

p1 <- ggplot() + 
  geom_point(aes(Sepal.Length, Sepal.Width, 
                 colour = Species, size = Species, clickSelects = Species), 
             data = iris) + 
  ggtitle("Sepal Data") + 
  facet_wrap(~Species, nrow = 2)
p2 <- ggplot() + 
  geom_point(aes(Petal.Length, Petal.Width, 
                 colour = Species, size = Species, showSelected = Species
  ), 
  data = iris) + 
  ggtitle("Petal Data")
viz <- list(sepal = p1, 
            petal = p2, 
            title = "Different Panel Styles")

animint2dir(viz, 
            out.dir = "iris_animint", 
            open.browser = F)
servr::httd("iris_animint")

animint2gist(viz, "Interactive Legends")

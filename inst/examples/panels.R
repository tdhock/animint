library(ggplot2)
library(animint)

# sepal plot
p1 <- ggplot() + 
  geom_point(aes(Sepal.Length, Sepal.Width, colour = Species, size = Sepal.Width, 
                 clickSelects = Species), 
             data = iris) + 
  theme(panel.background = element_rect(fill = "lightblue"), 
        panel.border = element_rect(fill = NA, 
                                    color = "black", 
                                    size = 2, 
                                    linetype = "dashed"), 
        panel.margin = grid::unit(.1, "cm")) + 
  facet_wrap(~Species, nrow = 2)

# panel plot
p2 <- ggplot() + 
  geom_point(aes(Petal.Length, Petal.Width, colour = Species, 
                 showSelected = Species), data = iris) + 
  ggtitle("Petal Data") + 
  theme_bw()
viz <- list(sepal = p1, 
            petal = p2, 
            title = "Different Panel Styles", 
            selector.types=list(Species="multiple"))

animint2dir(viz, out.dir = "iris_animint")

animint2gist(viz, "Panel Backgrounds")
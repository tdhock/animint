
library(ggplot2)
library(animint)

p1 <- ggplot() + 
  geom_point(aes(Sepal.Length, Sepal.Width, 
                 colour = Species, size = Sepal.Width), 
             data = iris) + 
  scale_colour_discrete(name = "species") + 
  theme(panel.background = element_rect(fill = "lightblue"), 
        panel.border = element_rect(fill = NA, 
                                    color = "black", 
                                    size = 2, 
                                    linetype = "dashed"), 
        panel.margin = grid::unit(.1, "cm")) + 
  facet_wrap(~Species, nrow = 2)
p2 <- ggplot() + 
  geom_point(aes(Petal.Length, Petal.Width, 
                 colour = Species, size = Species
  ), 
  data = iris) + 
  ggtitle("Petal Data") + 
  theme_bw()
viz <- list(sepal = p1, 
            petal = p2, 
            title = "Different Panel Styles")

animint2dir(viz, 
            out.dir = "iris_animint", 
            open.browser = F)
servr::httd("iris_animint")

animint2gist(viz, "Interactive Legends")

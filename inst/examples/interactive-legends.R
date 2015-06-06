
library(ggplot2)
library(animint)

p1 <- ggplot() + 
  geom_point(aes(Sepal.Length, Sepal.Width, 
                 colour = Species, alpha = Species, size = Sepal.Width), 
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
  geom_point(aes(Petal.Length, Petal.Width, colour = Species, 
                 showSelected = Species), data = iris) + 
  ggtitle("Petal Data") + 
  theme_bw()
viz <- list(sepal = p1, 
            petal = p2, 
            title = "Different Panel Styles", 
            selector.types=list(Species="multiple"))

animint2dir(viz, 
            out.dir = "~/temp/test_animint/iris_animint", 
            open.browser = F)
servr::httd("~/temp/test_animint/iris_animint")

animint2gist(viz, "Interactive Legends")

context("Interactive Legends")

p1 <- ggplot() + 
  geom_point(aes(Sepal.Length, Sepal.Width, 
                 colour = Species, size = Petal.Width), 
             data = iris) + 
  theme(panel.background = element_rect(fill = "lightblue"), 
        panel.border = element_rect(fill = NA, 
                                    color = "black", 
                                    size = 2, 
                                    linetype = "dashed"), 
        panel.margin = grid::unit(.1, "cm")) + 
  facet_wrap(~Species, nrow = 2)
p2 <- ggplot() + 
  geom_point(aes(Petal.Length, Petal.Width, 
                 colour = Species, size = Species), 
  data = iris) + 
  ggtitle("Petal Data") + 
  theme_bw()

viz <- list(sepal = p1, 
            petal = p2, 
            title = "Different Panel Styles")
info <- animint2HTML(viz)

test_that("compiler adds aesthetics, selector.types, and first", {
  dir <- animint2dir(viz, out.dir = "animint-htmltest", open.browser = F)
  
  expect_match(dir$selector.types, "multiple")
  expect_true(all(dir$first$Species %in% c("setosa", "virginica", "versicolor")))
})

test_that("all points are initially draw", {
  
})

test_that("clicking species legend adds and removes points", {
  
})

test_that("clicking sepal.width legend does nothing", {
  
})
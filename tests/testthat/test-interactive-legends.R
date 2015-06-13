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
  expect_true("showSelectedcolour" %in% names(info$geoms$geom1_point_sepal$aes))
  expect_match(info$selector.types, "multiple")
  expect_true(all(info$first$Species %in% c("setosa", "virginica", "versicolor")))
})

## function to extract all circles from an HTML page
get_circles <- function(id) {
  getNodeSet(getHTML(), paste0("//svg[@id='", id, "']//circle"))
}

test_that("all points are initially draw", {
  expect_equal(length(get_circles("sepal")), 150)
  expect_equal(length(get_circles("petal")), 150)
})

test_that("clicking species legend adds and removes points", {
  # virginica points are removed
  clickID("virginica")
  expect_equal(length(get_circles("sepal")), 100)
  expect_equal(length(get_circles("petal")), 100)
  
  # virginica points are added back
  clickID("virginica")
  expect_equal(length(get_circles("sepal")), 150)
  expect_equal(length(get_circles("petal")), 150)
})

test_that("clicking sepal.width legend does nothing", {
  clickID("2.5")
  expect_equal(length(get_circles("sepal")), 150)
  expect_equal(length(get_circles("petal")), 150)
})
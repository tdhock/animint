acontext("Interactive Legends")

## function to extract all circles from an HTML page
get_circles <- function(id) {
  getNodeSet(getHTML(), paste0("//svg[@id='", id, "']//circle"))
}

iris$id <- 1:nrow(iris)
p1 <- ggplot() + 
  geom_point(aes(Sepal.Length, Sepal.Width, colour = Species, 
                 size = Petal.Width, clickSelects = Species, id = id), 
             data = iris) + 
  facet_wrap(~Species, nrow = 2) + 
  ggtitle("Sepal Data")
p2 <- ggplot(iris, aes(Petal.Length, Petal.Width, colour = Species, 
                       size = Sepal.Width, showSelected = Species)) + 
  geom_point() + 
  ggtitle("Petal Data")

viz <- list(sepal = p1, 
            petal = p2)
info <- animint2HTML(viz)

test_that("compiler adds selector.types and first", {
  expect_match(info$selector.types, "multiple")
  expect_true(all(info$first$Species %in% c("setosa", "virginica", "versicolor")))
})

test_that("all points are initially drawn", {
  expect_equal(length(get_circles("sepal")), 150)
  expect_equal(length(get_circles("petal")), 150)
})

test_that("clicking species legend adds and removes points", {
  # virginica points are removed
  clickID("virginica")
  expect_equal(length(get_circles("sepal")), 150)
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

test_that("clicking Sepal point doesn't affect sepal plot", {
  clickID("51")
  expect_equal(length(get_circles("petal")), 100)
  expect_equal(length(get_circles("sepal")), 150)
})

mtcars$am <- factor(mtcars$am)
p <- qplot(data=mtcars, mpg, hp, color = am)
info <- animint2HTML(list(p = p))

test_that("A plot with no show/click aes and a legend should be clickable", {
  expect_equal(length(get_circles("p")), 32)
  clickID("0")
  expect_equal(length(get_circles("p")), 13)
  clickID("0")
  expect_equal(length(get_circles("p")), 32)
})

p_factor <- qplot(data=mtcars, mpg, hp, color = factor(vs))
info <- animint2HTML(list(p = p_factor))

test_that("A plot with an expression should be clickable", {
  expect_equal(length(get_circles("p")), 32)
  clickID("0")
  expect_equal(length(get_circles("p")), 14)
  clickID("0")
  expect_equal(length(get_circles("p")), 32)  
})
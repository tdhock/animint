context("Panel background")

p1 <- ggplot() +
  geom_point(aes(Sepal.Length, Sepal.Width,
                 colour = Species, size = Species), data = iris) +
  theme(panel.background = element_rect(fill = "lightblue"),
        panel.border = element_rect(fill = NA,
                                    color = "black",
                                    size = 2,
                                    linetype = "dashed"),
        panel.margin = grid::unit(.1, "cm")) +
  facet_wrap(~Species, nrow = 2)
p2 <- ggplot() +
  geom_point(aes(Petal.Length, Petal.Width,
                 colour = Species, size = Species), data = iris) +
  ggtitle("Petal Data") +
  theme_bw()
p3 <- ggplot() + 
  geom_point(aes(Petal.Length, Petal.Width, 
                 colour = Species, size = Species), data = iris) + 
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

info <- animint2HTML(list(sepal = p1, petal = p2, blank = p3))

# background rectangle for each panel
background_sepal <- getNodeSet(info$html, '//svg[@id="sepal"]//rect[@class="background_rect"]')
attr_back_sepal <- sapply(background_sepal, xmlAttrs)

background_petal <- getNodeSet(info$html, '//svg[@id="petal"]//rect[@class="background_rect"]')
attr_back_petal <- sapply(background_petal, xmlAttrs)

blank_petal <- getNodeSet(info$html, '//svg[@id="blank"]//rect[@class="background_rect"]')

# border rectangle for each panel
border_sepal <- getNodeSet(info$html, '//svg[@id="sepal"]//rect[@class="border_rect"]')
attr_border_sepal <- sapply(border_sepal, xmlAttrs)

border_petal <- getNodeSet(info$html, '//svg[@id="petal"]//rect[@class="border_rect"]')
attr_border_petal <- sapply(border_petal, xmlAttrs)

# major grid lines
grid_major_sepal <- getNodeSet(info$html, '//svg[@id="sepal"]//line[@class="grid grid_major"]')
attr_major_sepal <- sapply(grid_major_sepal, xmlAttrs)

grid_major_petal <- getNodeSet(info$html, '//svg[@id="petal"]//line[@class="grid grid_major"]')
attr_major_petal <- sapply(grid_major_petal, xmlAttrs)

grid_major_blank <- getNodeSet(info$html, '//svg[@id="blank"]//line[@class="grid grid_major"]')

# minor grid lines
grid_minor_sepal <- getNodeSet(info$html, '//svg[@id="sepal"]//line[@class="grid grid_minor"]')
attr_minor_sepal <- sapply(grid_minor_sepal, xmlAttrs)

grid_minor_petal <- getNodeSet(info$html, '//svg[@id="petal"]//line[@class="grid grid_minor"]')
attr_minor_petal <- sapply(grid_minor_petal, xmlAttrs)

grid_minor_blank <- getNodeSet(info$html, '//svg[@id="blank"]//line[@class="grid grid_minor"]')

# different patterns to access
fillPattern <-
  paste0("fill: ",
         "(?<value>.*?)",
         ";")
strokePattern <- paste0("stroke: ",
                        "(?<value>.*?)",
                        ";")
dasharrayPattern <-
  paste0("stroke-dasharray:",
         "(?<value>.*?)",
         ";")

test_color <- function(value, expected) {
  # convert R color to hexadecimal
  expected_col <- toRGB(expected)

  if(grepl("rgb", value)) {
    expected_string <- paste0("rgb(", paste(col2rgb(expected_col), collapse = ", "), ")")
  } else {
    expected_string <- expected_col
  }

  expect_equal(toupper(value), toupper(expected_string))
}

test_that("panel backgrounds render correctly", {
  # testing that there are the correct number of panels
  expect_equal(length(background_sepal), 3)
  expect_equal(length(background_petal), 1)

  # test background fills
  match_sepal <- str_match_perl(attr_back_sepal["style",], fillPattern)
  value_sepal <- match_sepal[, "value"]
  test_color(value_sepal[1], "lightblue")

  match_petal <- str_match_perl(attr_back_petal["style",], fillPattern)
  value_petal <- match_petal[, "value"]
  test_color(value_petal[1], "white")
  
  # test that there is no rectangle for element_blank()
  expect_equal(length(blank_petal), 0)
})

test_that("panel borders render correctly", {
  # testing that there are the correct number of panels
  expect_equal(length(border_sepal), 3)
  expect_equal(length(border_petal), 1)

  # test border colors
  match_sepal <- str_match_perl(attr_border_sepal["style",], strokePattern)
  value_sepal <- match_sepal[, "value"]
  test_color(value_sepal[1], "black")

  match_petal <- str_match_perl(attr_border_petal["style",], strokePattern)
  value_petal <- match_petal[, "value"]
  test_color(value_petal[1], "grey50")
})

test_that("grid lines are drawn correctly", {
  # correct number of grid lines in both plots
  expect_equal(length(grid_major_sepal), 30)
  expect_equal(length(grid_major_petal), 9)
  expect_equal(length(grid_minor_sepal), 27)
  expect_equal(length(grid_minor_petal), 9)
  expect_equal(length(grid_major_blank), 0)
  expect_equal(length(grid_minor_blank), 0)
})

test_that("renderer can handle no grid lines", {
  # plot
  data(tips, package = "reshape2")
  tips$sex_smoker <- with(tips, interaction(sex, smoker))
  info <- animint2HTML(list(
    p1 = ggplot() + theme(legend.position = "none") +
      geom_point(data = tips, position = "jitter", 
                 aes(x = sex, y = smoker, colour = sex_smoker,
                     clickSelects = sex_smoker)), 
    p2 = ggplot() +
      geom_point(data = tips,
                 aes(x = total_bill, y = tip, colour = sex_smoker,
                     showSelected = sex_smoker))
    ))
  # extract grids
  grid_major_p1 <- getNodeSet(info$html, '//svg[@id="p1"]//line[@class="grid grid_major"]')
  grid_minor_p1 <- getNodeSet(info$html, '//svg[@id="p1"]//line[@class="grid grid_minor"]')
  expect_equal(length(grid_major_p1), 4)
  expect_equal(length(grid_minor_p1), 0)
})
acontext("Panel background")

p1 <- ggplot() +
  geom_point(aes(Sepal.Length, Sepal.Width,
                 colour = Species, size = Species), data = iris) +
  theme_grey() + 
  theme(panel.background = element_rect(fill = "lightblue"),
        panel.border = element_rect(fill = NA,
                                    color = "black",
                                    size = 2,
                                    linetype = "dashed"),
        panel.margin = grid::unit(0.1, "cm")) +
  facet_wrap(~Species, nrow = 2)
p2 <- ggplot() +
  geom_point(aes(Petal.Length, Petal.Width,
                 colour = Species, size = Species), data = iris) +
  ggtitle("Petal Data") +
  theme_bw()
p3 <- p2 + 
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
p4 <- p2 + 
  ## recreating theme_fivethirtyeight from ggthemes package
  theme(rect = element_rect(fill = "#F0F0F0", colour = NA,
                            size = 0.5, linetype = 0),
        line = element_line(colour = "#D2D2D2", size = 0.5, linetype = 1,
                            lineend = "butt"),
        text = element_text(family = "sans", face = "plain",
                            colour = "#3C3C3C", size = 12,
                            hjust = 0.5, vjust = 0.5, angle = 0,
                            lineheight = 0.9, margin = c(0, 0, 0, 0),
                            debug = FALSE),
        panel.background = element_rect(), 
        panel.grid = element_line(), 
        panel.grid.major = element_line(), 
        panel.grid.minor = element_blank(), 
        complete = T)

info <- animint2HTML(list(sepal = p1, petal = p2, blank = p3, gg538 = p4))

rect.list <- getNodeSet(
  info$html, '//svg[@id="plot_sepal"]//rect[@class="border_rect"]')
expect_equal(length(rect.list), 3)
at.mat <- sapply(rect.list, xmlAttrs)

test_that("four unique border_rect x values (some horiz space)", {
  left.vec <- as.numeric(at.mat["x", ])
  width.vec <- as.numeric(at.mat["width", ])
  right.vec <- left.vec + width.vec
  x.values <- unique(c(left.vec, right.vec))
  expect_equal(length(x.values), 4)
})

# extracting html from plots --------------------------------------

# background rectangle for each panel
background_sepal <- getNodeSet(
  info$html, '//svg[@id="plot_sepal"]//rect[@class="background_rect"]')
attr_back_sepal <- sapply(background_sepal, xmlAttrs)

background_petal <- getNodeSet(
  info$html, '//svg[@id="plot_petal"]//rect[@class="background_rect"]')
attr_back_petal <- sapply(background_petal, xmlAttrs)

blank_petal <- getNodeSet(
  info$html, '//svg[@id="plot_blank"]//rect[@class="background_rect"]')

gg538 <- getNodeSet(
  info$html, '//svg[@id="plot_gg538"]//rect[@class="background_rect"]')
attr_gg538 <- sapply(gg538, xmlAttrs)

# border rectangle for each panel
border_sepal <- getNodeSet(
  info$html, '//svg[@id="plot_sepal"]//rect[@class="border_rect"]')
attr_border_sepal <- sapply(border_sepal, xmlAttrs)

border_petal <- getNodeSet(
  info$html, '//svg[@id="plot_petal"]//rect[@class="border_rect"]')
attr_border_petal <- sapply(border_petal, xmlAttrs)

# major grid lines
grid_major_sepal <- getNodeSet(
  info$html, '//svg[@id="plot_sepal"]//g[@class="grid_major"]//line')
attr_major_sepal <- sapply(grid_major_sepal, xmlAttrs)

grid_major_petal <- getNodeSet(
  info$html, '//svg[@id="plot_petal"]//g[@class="grid_major"]//line')
attr_major_petal <- sapply(grid_major_petal, xmlAttrs)

grid_major_blank <- getNodeSet(
  info$html, '//svg[@id="plot_blank"]//g[@class="grid_major"]//line')

grid_major_gg538 <- getNodeSet(
  info$html, '//svg[@id="plot_gg538"]//g[@class="grid_major"]//line')
attr_major_gg538 <- sapply(grid_major_gg538, xmlAttrs)

# minor grid lines
grid_minor_sepal <- getNodeSet(
  info$html, '//svg[@id="plot_sepal"]//g[@class="grid_minor"]//line')

grid_minor_petal <- getNodeSet(
  info$html, '//svg[@id="plot_petal"]//g[@class="grid_minor"]//line')

grid_minor_blank <- getNodeSet(
  info$html, '//svg[@id="plot_blank"]//g[@class="grid_minor"]//line')

# different patterns to access
fillPattern <- paste0("fill: ",
                      "(?<value>.*?)",
                      ";")
strokePattern <- paste0("stroke: ",
                        "(?<value>.*?)",
                        ";")
dasharrayPattern <- paste0("stroke-dasharray:",
                           "(?<value>.*?)",
                           ";")

# Testing -----------------------------------

test_that("panel backgrounds render correctly", {
  # testing that there are the correct number of panels
  expect_equal(length(background_sepal), 3)
  expect_equal(length(background_petal), 1)
  expect_equal(length(blank_petal), 0)  # no rectangle for element_blank()
  expect_equal(length(gg538), 1)

  # test background fills
  match_sepal <- str_match_perl(attr_back_sepal["style",], fillPattern)
  value_sepal <- match_sepal[, "value"]
  expect_color(value_sepal[1], "lightblue")

  match_petal <- str_match_perl(attr_back_petal["style",], fillPattern)
  value_petal <- match_petal[, "value"]
  expect_color(value_petal[1], "white")
  
  match_gg538 <- str_match_perl(attr_gg538["style",], fillPattern)
  value_gg538 <- match_gg538[, "value"]
  expect_color(value_gg538[1], "#F0F0F0")
})

test_that("panel borders render correctly", {
  # testing that there are the correct number of panels
  expect_equal(length(border_sepal), 3)
  expect_equal(length(border_petal), 1)

  # test border colors
  match_sepal <- str_match_perl(attr_border_sepal["style",], strokePattern)
  value_sepal <- match_sepal[, "value"]
  expect_color(value_sepal[1], "black")

  match_petal <- str_match_perl(attr_border_petal["style",], strokePattern)
  value_petal <- match_petal[, "value"]
  expect_color(value_petal[1], "grey50")
})

test_that("grid lines are drawn correctly", {
  # correct number of grid lines in both plots
  expect_equal(length(grid_major_sepal), 30)
  expect_equal(length(grid_major_petal), 9)
  expect_equal(length(grid_minor_sepal), 27)
  expect_equal(length(grid_minor_petal), 9)
  expect_equal(length(grid_major_blank), 0)
  expect_equal(length(grid_minor_blank), 0)
  expect_equal(length(grid_major_gg538), 9)
  
  # correct color of grid lines
  match_sepal <- str_match_perl(attr_major_sepal["style",], strokePattern)
  value_sepal <- match_sepal[, "value"]
  expect_color(value_sepal[1], "white")
  
  match_petal <- str_match_perl(attr_major_petal["style",], strokePattern)
  value_petal <- match_petal[, "value"]
  expect_color(value_petal[1], "grey90")
  
  match_gg538 <- str_match_perl(attr_major_gg538["style",], strokePattern)
  value_gg538 <- match_gg538[, "value"]
  expect_color(value_gg538[1], "#D2D2D2")
})

data(tips, package = "reshape2")
tips$sex_smoker <- with(tips, interaction(sex, smoker))
ss.viz <- list(
  p1 = ggplot() + theme(legend.position = "none") +
    geom_point(data = tips, position = "jitter", 
               aes(x = sex, y = smoker, colour = sex_smoker,
                   clickSelects = sex_smoker)), 
  p2 = ggplot() +
    geom_point(data = tips,
               aes(x = total_bill, y = tip, colour = sex_smoker,
                   showSelected = sex_smoker))
  )

test_that("renderer can handle no grid lines", {
  info <- animint2HTML(ss.viz)
  # extract grids
  grid_major_p1 <- getNodeSet(
    info$html, '//svg[@id="plot_p1"]//g[@class="grid_major"]//line')
  grid_minor_p1 <- getNodeSet(
    info$html, '//svg[@id="plot_p1"]//g[@class="grid_minor"]//line')
  expect_equal(length(grid_major_p1), 4)
  expect_equal(length(grid_minor_p1), 0)
})

test_that("multiple selection sex_smoker plot", {
  ss.viz$selector.types$sex_smoker <- "multiple"
  info <- animint2HTML(ss.viz)
  circle.list <- getNodeSet(
    info$html, '//svg[@id="plot_p2"]//circle')
  expect_equal(length(circle.list), nrow(tips))
})

test_that("renderer can handle only one grid line", {
  info <- animint2HTML(list(
    petal = p2 + scale_y_log10()
  ))
  # extract grids
  grid_minor_hor <- getNodeSet(info$html, '//svg//g[@class="grid_minor"]//g[@class="hor"]//line')
  grid_minor_vert <- getNodeSet(info$html, '//svg//g[@class="grid_minor"]//g[@class="vert"]//line')
  expect_equal(length(grid_minor_hor), 1)
  expect_equal(length(grid_minor_vert), 4)
})

test_that("no minor grid lines is handed correctly", {
  data(geyser, package = "MASS")
  info <- animint2HTML(list(
    g = ggplot() +  
      geom_point(data = geyser, 
                 aes(x = duration, y = waiting)) + 
      geom_contour(data = geyser, 
                   aes(x = duration, y = waiting), 
                   colour = "blue", size = .5, stat = "density2d") + 
      xlim(0.5, 6) + scale_y_log10(limits = c(40,110)) +
      ggtitle("geom_contour 2d density")
  ))
  # extract grids
  grid_major_hor <- getNodeSet(info$html, '//svg//g[@class="grid_major"]//g[@class="hor"]//line')
  grid_minor_hor <- getNodeSet(info$html, '//svg//g[@class="grid_minor"]//g[@class="hor"]//line')
  expect_equal(length(grid_major_hor), 1)
  expect_equal(length(grid_minor_hor), 0)
})

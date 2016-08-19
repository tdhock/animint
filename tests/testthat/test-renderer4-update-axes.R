acontext("update_axes")

# Plots with axis updates
mtcars$cyl <- as.factor(mtcars$cyl)

no_updates <- ggplot()+geom_point(aes(mpg, disp, 
                                      colour=cyl), 
                                  data = mtcars)

update_x <- no_updates+
  theme_animint(update_axes=c("x"))
update_y <- no_updates+
  theme_animint(update_axes=c("y"))
update_xy <- no_updates+
  theme_animint(update_axes=c("x","y"))

viz <- (list(neither=no_updates, 
            x=update_x, 
            y=update_y, 
            both=update_xy))

expect_warning(animint2HTML(viz),
  paste("update_axes specified for X axis on plot x",
        "but found no geoms with showSelected=singleSelectionVariable,",
        "so created a plot with no updates for X axis"))


# We only update axes for single selectors
viz$selector.types = list(cyl="single")

expect_no_warning(info <- animint2HTML(viz))

# Update selection and get HTML
clickID(c("plot_neither_cyl_variable_8"))
Sys.sleep(0.5)
info$html_updated1 <- getHTML()

# Update selection and get HTML
clickID(c("plot_neither_cyl_variable_4"))
Sys.sleep(0.5)
info$html_updated2 <- getHTML()


## ------------------------------------------------------------------- ##
## Test for tick updates

rect_path <- "//svg[@id='plot_%s']//g[contains(@class, '%saxis')]"
all_rect_paths <- lapply(names(viz), sprintf, fmt=rect_path,
                           c("x","y"))[1:4]

# Take tick diffs for all 4 plots
rect_nodes1 <- sapply(all_rect_paths, getNodeSet, doc=info$html)
original_tick_diff_x <- sapply(rect_nodes1[1, ], getTickDiff, axis="x")
original_tick_diff_y <- sapply(rect_nodes1[2, ], getTickDiff, axis="y")

rect_nodes2 <- sapply(all_rect_paths, getNodeSet, doc=info$html_updated1)
updated_tick_diff_x <- sapply(rect_nodes2[1, ], getTickDiff, axis="x")
updated_tick_diff_y <- sapply(rect_nodes2[2, ], getTickDiff, axis="y")

test_that("axis ticks change when plots are updated",{
  # initial updates -> axis ticks are different for x and y axis
  expect_equal(length(unique(original_tick_diff_x)), 2)
  expect_equal(length(unique(original_tick_diff_y)), 2)
  
  #no_updates
  expect_equal(updated_tick_diff_x[1], original_tick_diff_x[1])
  expect_equal(updated_tick_diff_y[1], original_tick_diff_y[1])
  
  #update_x
  expect_false(updated_tick_diff_x[2] == original_tick_diff_x[2])
  expect_equal(updated_tick_diff_y[2], original_tick_diff_y[2])
  
  #update_y
  expect_equal(updated_tick_diff_x[3], original_tick_diff_x[3])
  expect_false(updated_tick_diff_y[3] == original_tick_diff_y[3])
  
  #update_xy
  expect_false(updated_tick_diff_x[4] == original_tick_diff_x[4])
  expect_false(updated_tick_diff_y[4] == original_tick_diff_y[4])
})


## ------------------------------------------------------------------- ##
## Test for grid updates

## get both horizontal and vertical grid lines
get_grid_lines <- function(html, p_name, grid_class){
  path.i <-
    '//svg[@id="plot_%s"]//g[@class="grid_%s"]//g[@class="%s"]//line'
  path.hor <- sprintf(path.i, p_name, grid_class, "hor")
  path.vert <- sprintf(path.i, p_name, grid_class, "vert")
  nodes_h <- getNodeSet(html, path.hor)
  nodes_v <- getNodeSet(html, path.vert)
  # take x1, x2, y1, y2 values only
  attr_h <- sapply(nodes_h, xmlAttrs)[1:4, ]
  attr_v <- sapply(nodes_v, xmlAttrs)[1:4, ]
  return(list(hor=attr_h, vert=attr_v))
}

minor_grid_attr1 <- minor_grid_attr2 <- minor_grid_attr3 <- list()
major_grid_attr1 <- major_grid_attr2 <- major_grid_attr3 <- list()

p_names <- names(viz)[1:4]
for(p.name in p_names){
  major_grid_attr1[[p.name]] <- get_grid_lines(info$html, p.name, "major")
  major_grid_attr2[[p.name]] <- get_grid_lines(info$html_updated1,
                                               p.name, "major")
  major_grid_attr3[[p.name]] <- get_grid_lines(info$html_updated2,
                                               p.name, "major")
  
  minor_grid_attr1[[p.name]] <- get_grid_lines(info$html, p.name, "minor")
  minor_grid_attr2[[p.name]] <- get_grid_lines(info$html_updated1,
                                               p.name, "minor")
  minor_grid_attr3[[p.name]] <- get_grid_lines(info$html_updated2,
                                               p.name, "minor")
}

test_that("major grids are updated",{
  # initial grid updates
  expect_false(identical(major_grid_attr1$x, major_grid_attr1$neither))
  expect_false(identical(major_grid_attr1$y, major_grid_attr1$neither))
  expect_false(identical(major_grid_attr1$both, major_grid_attr1$neither))
  expect_false(identical(major_grid_attr1$x, major_grid_attr1$neither))
  expect_false(identical(major_grid_attr1$y, major_grid_attr1$neither))
  expect_false(identical(major_grid_attr1$both, major_grid_attr1$neither))
  
  # no_updates
  expect_identical(major_grid_attr2$neither, major_grid_attr1$neither)
  expect_identical(major_grid_attr3$neither, major_grid_attr1$neither)
  
  # update_x -> only vert grids are updated
  expect_identical(major_grid_attr2$x$hor, major_grid_attr1$x$hor)
  expect_identical(major_grid_attr3$x$hor, major_grid_attr1$x$hor)
  expect_false(identical(major_grid_attr2$x$vert,
                         major_grid_attr1$x$vert))
  expect_false(identical(major_grid_attr3$x$vert,
                         major_grid_attr1$x$vert))
  expect_false(identical(major_grid_attr3$x$vert,
                         major_grid_attr2$x$vert))
  
  # update_y -> only hor grids are updated
  expect_false(identical(major_grid_attr2$y$hor,
                         major_grid_attr1$y$hor))
  expect_false(identical(major_grid_attr3$y$hor,
                         major_grid_attr1$y$hor))
  expect_false(identical(major_grid_attr3$y$hor,
                         major_grid_attr2$y$hor))
  expect_identical(major_grid_attr2$y$vert, major_grid_attr1$y$vert)
  expect_identical(major_grid_attr3$y$vert, major_grid_attr1$y$vert)
  
  # update_xy -> both vert and hor grids updated
  expect_false(identical(major_grid_attr2$both$hor,
                         major_grid_attr1$both$hor))
  expect_false(identical(major_grid_attr3$both$hor,
                         major_grid_attr1$both$hor))
  expect_false(identical(major_grid_attr3$both$hor,
                         major_grid_attr2$both$hor))
  expect_false(identical(major_grid_attr2$both$vert,
                         major_grid_attr1$both$vert))
  expect_false(identical(major_grid_attr3$both$vert,
                         major_grid_attr1$both$vert))
  expect_false(identical(major_grid_attr3$both$vert,
                         major_grid_attr2$both$vert))
})

test_that("minor grids are updated",{
  # initial grid updates
  expect_false(identical(minor_grid_attr1$x, minor_grid_attr1$neither))
  expect_false(identical(minor_grid_attr1$y, minor_grid_attr1$neither))
  expect_false(identical(minor_grid_attr1$both, minor_grid_attr1$neither))
  expect_false(identical(minor_grid_attr1$x, minor_grid_attr1$neither))
  expect_false(identical(minor_grid_attr1$y, minor_grid_attr1$neither))
  expect_false(identical(minor_grid_attr1$both, minor_grid_attr1$neither))
  
  #no_updates
  expect_identical(minor_grid_attr2$neither, minor_grid_attr1$neither)
  expect_identical(minor_grid_attr3$neither, minor_grid_attr1$neither)
  
  #update_x -> only vert grids are updated
  expect_identical(minor_grid_attr2$x$hor, minor_grid_attr1$x$hor)
  expect_identical(minor_grid_attr3$x$hor, minor_grid_attr1$x$hor)
  expect_false(identical(minor_grid_attr2$x$vert,
                         minor_grid_attr1$x$vert))
  expect_false(identical(minor_grid_attr3$x$vert,
                         minor_grid_attr1$x$vert))
  expect_false(identical(minor_grid_attr3$x$vert,
                         minor_grid_attr2$x$vert))
  
  #update_y -> only hor grids are updated
  expect_false(identical(minor_grid_attr2$y$hor,
                         minor_grid_attr1$y$hor))
  expect_false(identical(minor_grid_attr3$y$hor,
                         minor_grid_attr1$y$hor))
  expect_false(identical(minor_grid_attr3$y$hor,
                         minor_grid_attr2$y$hor))
  expect_identical(minor_grid_attr2$y$vert, minor_grid_attr1$y$vert)
  expect_identical(minor_grid_attr3$y$vert, minor_grid_attr1$y$vert)
  
  #update_xy -> both vert and hor grids updated
  expect_false(identical(minor_grid_attr2$both$hor,
                         minor_grid_attr1$both$hor))
  expect_false(identical(minor_grid_attr3$both$hor,
                         minor_grid_attr1$both$hor))
  expect_false(identical(minor_grid_attr3$both$hor,
                         minor_grid_attr2$both$hor))
  expect_false(identical(minor_grid_attr2$both$vert,
                         minor_grid_attr1$both$vert))
  expect_false(identical(minor_grid_attr3$both$vert,
                         minor_grid_attr1$both$vert))
  expect_false(identical(minor_grid_attr3$both$vert,
                         minor_grid_attr2$both$vert))
})

## -------------------------------------------------------------------- ##
## Test for zooming of geoms

## Get ranges of geoms
no_updates_ranges1 <- get_pixel_ranges(info$html_updated1,
                                       "geom1_point_neither")
no_updates_ranges2 <- get_pixel_ranges(info$html_updated2,
                                       "geom1_point_neither")

x_updates_ranges1 <- get_pixel_ranges(info$html_updated1,
                                       "geom2_point_x")
x_updates_ranges2 <- get_pixel_ranges(info$html_updated2,
                                       "geom2_point_x")

y_updates_ranges1 <- get_pixel_ranges(info$html_updated1,
                                       "geom3_point_y")
y_updates_ranges2 <- get_pixel_ranges(info$html_updated2,
                                       "geom3_point_y")

xy_updates_ranges1 <- get_pixel_ranges(info$html_updated1,
                                       "geom4_point_both")
xy_updates_ranges2 <- get_pixel_ranges(info$html_updated2,
                                       "geom4_point_both")

test_that("geoms get zoomed-in upon changing selection", {
  # no_updates
  expect_false(all(no_updates_ranges2$x == no_updates_ranges1$x))
  expect_false(all(no_updates_ranges2$y == no_updates_ranges1$y))
  
  # x_updates
  expect_equal(x_updates_ranges2$x, x_updates_ranges1$x)
  expect_false(all(x_updates_ranges2$y == x_updates_ranges1$y))
  
  # y_updates
  expect_false(all(y_updates_ranges2$x == y_updates_ranges1$x))
  expect_equal(y_updates_ranges2$y, y_updates_ranges1$y)
  
  # xy_updates
  expect_equal(xy_updates_ranges2$x, xy_updates_ranges1$x)
  expect_equal(xy_updates_ranges2$y, xy_updates_ranges1$y)
})

## ------------------------------------------------------------------- ##
## Test for different geoms - ribbon, rect, segment, text
## We test each for no warnings, axis tick updates and grid updates

## ------------------------- geom_ribbon ----------------------------- ##
set.seed(132)
ribbondata <- data.frame(x=seq(0, 1, .1), ymin=runif(11, 0, 1), ymax=runif(11, 1, 2))
ribbondata <- rbind(cbind(ribbondata, group="low"),
                    cbind(ribbondata, group="medium"),
                    cbind(ribbondata, group="high"))
ribbondata[12:22,2:3] <- ribbondata[12:22,2:3]+runif(11, 1, 10)
ribbondata[23:33,2:3] <- ribbondata[12:22,2:3]+runif(11, 1, 10)
ribbon <- ggplot() + 
  geom_ribbon(data=ribbondata, aes(x=x, ymin=ymin, ymax=ymax, group=group, fill=group), alpha=.5) + 
  ggtitle("geom_ribbon") +
  theme_animint(update_axes = c("y"))
viz <- list(ribbon=ribbon, selector.types=list(group="single"))
expect_no_warning(info <- animint2HTML(viz))

# Update selection and get HTML
clickID(c("plot_ribbon_group_variable_high"))
Sys.sleep(0.5)
info$html_updated <- getHTML()

minor_grid_attr1 <- major_grid_attr1 <- minor_grid_attr2 <- 
  major_grid_attr2 <- list()

minor_grid_attr1 <- get_grid_lines(info$html, names(viz)[[1]], "minor")
minor_grid_attr2 <- get_grid_lines(info$html_updated, names(viz)[[1]], "minor")
major_grid_attr1 <- get_grid_lines(info$html, names(viz)[[1]], "major")
major_grid_attr2 <- get_grid_lines(info$html_updated, names(viz)[[1]], "major")

test_that("geom_ribbon has grid updates", {
  # y axis updates -> hor grids updated
  expect_identical(minor_grid_attr1$vert, minor_grid_attr2$vert)
  expect_identical(major_grid_attr1$vert, major_grid_attr2$vert)
  expect_false(identical(minor_grid_attr1$hor,
                         minor_grid_attr2$hor))
  expect_false(identical(major_grid_attr1$hor,
                         major_grid_attr2$hor))
})

path.i <- "//svg[@id='plot_ribbon']//g[contains(@class, 'yaxis')]"
nodes1 <- getNodeSet(info$html, path.i)
nodes2 <- getNodeSet(info$html_updated, path.i)
original_tick_diff <- sapply(nodes1, getTickDiff, axis="y")
updated_tick_diff <- sapply(nodes2, getTickDiff, axis="y")

test_that("geom_ribbon has axis tick updates", {
  expect_false(identical(updated_tick_diff, original_tick_diff))
})

## ------------------------- geom_rect ----------------------------- ##
data_f <- data.frame(
  x = rep(c(2, 5, 7, 9, 12), 2),
  y = rep(c(1, 2), each = 5),
  z = factor(rep(1:5, each = 2)),
  w = rep(diff(c(0, 4, 6, 8, 10, 14)), 2)
)
rect <- ggplot(data_f, aes(xmin = x - w / 2, xmax = x + w / 2, ymin = y, ymax = y + 1)) +
  geom_rect(aes(fill = z, width = w), colour = "grey50") +
  theme_animint(update_axes=c("x", "y"))
viz <- list(rect=rect)
viz$selector.types <- list(z="single")
expect_no_warning(info <- animint2HTML(viz))

# Update selection and get HTML
clickID(c("plot_rect_z_variable_4"))
Sys.sleep(0.5)
info$html_updated <- getHTML()

minor_grid_attr1 <- major_grid_attr1 <- minor_grid_attr2 <- 
  major_grid_attr2 <- list()

minor_grid_attr1 <- get_grid_lines(info$html, names(viz)[[1]], "minor")
minor_grid_attr2 <- get_grid_lines(info$html_updated, names(viz)[[1]], "minor")
major_grid_attr1 <- get_grid_lines(info$html, names(viz)[[1]], "major")
major_grid_attr2 <- get_grid_lines(info$html_updated, names(viz)[[1]], "major")

test_that("geom_rect has grid updates",{
  # xy axis updates -> both vert/hor grids updated
  expect_false(identical(minor_grid_attr1$vert, minor_grid_attr2$vert))
  expect_false(identical(major_grid_attr1$vert, major_grid_attr2$vert))
  expect_false(identical(minor_grid_attr1$hor, minor_grid_attr2$hor))
  expect_false(identical(major_grid_attr1$hor, major_grid_attr2$hor))
})

path.i <- "//svg[@id='plot_rect']//g[contains(@class, '%saxis')]"
path.xy <- sapply(c("x", "y"), sprintf, fmt=path.i)
nodes1_x <- getNodeSet(info$html, path.xy[["x"]])
nodes1_y <- getNodeSet(info$html, path.xy[["y"]])
nodes2_x <- getNodeSet(info$html_updated, path.xy[["x"]])
nodes2_y <- getNodeSet(info$html_updated, path.xy[["y"]])

original_tick_diff_x <- sapply(nodes1_x, getTickDiff, axis="x")
original_tick_diff_y <- sapply(nodes1_y, getTickDiff, axis="y")
updated_tick_diff_x <- sapply(nodes2_x, getTickDiff, axis="x")
updated_tick_diff_y <- sapply(nodes2_y, getTickDiff, axis="y")

test_that("geom_rect has axis tick updates", {
  expect_false(identical(updated_tick_diff_x, original_tick_diff_x))
  expect_false(identical(updated_tick_diff_y, original_tick_diff_y))
})

## ----------------------- geom_segment ----------------------------- ##
data_f <- data.frame(x1=c(runif(10, 0, 10),runif(10, 10, 20),runif(10, 20, 30),
                          runif(10, 30, 40)),
                     x2=runif(40, 0, 10),
                     y1=c(runif(10, -10, 10),runif(10, 0, 20),runif(10, 20, 30),
                          runif(10, -30, 50)),
                     y2=runif(40, 0, 40),
                     ss = as.factor(rep(1:4,each=10)))

segment <- ggplot() +
  geom_segment(aes(x=x1, y=y1, xend=x2, yend=y2, colour=ss),
               data=data_f) + 
  theme_animint(update_axes=c("x", "y"))
viz <- list(segment=segment, selector.types=list(ss="single"))
expect_no_warning(info <- animint2HTML(viz))

# Update selection and get HTML
clickID(c("plot_segment_ss_variable_3"))
Sys.sleep(0.5)
info$html_updated <- getHTML()

minor_grid_attr1 <- major_grid_attr1 <- minor_grid_attr2 <- 
  major_grid_attr2 <- list()

minor_grid_attr1 <- get_grid_lines(info$html, names(viz)[[1]], "minor")
minor_grid_attr2 <- get_grid_lines(info$html_updated, names(viz)[[1]], "minor")
major_grid_attr1 <- get_grid_lines(info$html, names(viz)[[1]], "major")
major_grid_attr2 <- get_grid_lines(info$html_updated, names(viz)[[1]], "major")

test_that("geom_segment has grid updates",{
  # xy axis updates -> both vert/hor grids updated
  expect_false(identical(minor_grid_attr1$vert, minor_grid_attr2$vert))
  expect_false(identical(major_grid_attr1$vert, major_grid_attr2$vert))
  expect_false(identical(minor_grid_attr1$hor, minor_grid_attr2$hor))
  expect_false(identical(major_grid_attr1$hor, major_grid_attr2$hor))
})

path.i <- "//svg[@id='plot_segment']//g[contains(@class, '%saxis')]"
path.xy <- sapply(c("x", "y"), sprintf, fmt=path.i)
nodes1_x <- getNodeSet(info$html, path.xy[["x"]])
nodes1_y <- getNodeSet(info$html, path.xy[["y"]])
nodes2_x <- getNodeSet(info$html_updated, path.xy[["x"]])
nodes2_y <- getNodeSet(info$html_updated, path.xy[["y"]])

original_tick_diff_x <- sapply(nodes1_x, getTickDiff, axis="x")
original_tick_diff_y <- sapply(nodes1_y, getTickDiff, axis="y")
updated_tick_diff_x <- sapply(nodes2_x, getTickDiff, axis="x")
updated_tick_diff_y <- sapply(nodes2_y, getTickDiff, axis="y")

test_that("geom_segment has axis tick updates", {
  expect_false(identical(updated_tick_diff_x, original_tick_diff_x))
  expect_false(identical(updated_tick_diff_y, original_tick_diff_y))
})

##  ------------------------- geom_text ------------------------- ##
text <- ggplot() + geom_text(aes(mpg, disp, colour=cyl, label=hp), 
                             data = mtcars) +
  theme_animint(update_axes=c("x", "y"))
viz <- list(text=text, selector.types=list(cyl="single"))
expect_no_warning(info <- animint2HTML(viz))

# Update selection and get HTML
clickID(c("plot_text_cyl_variable_4"))
Sys.sleep(0.5)
info$html_updated <- getHTML()

minor_grid_attr1 <- major_grid_attr1 <- minor_grid_attr2 <- 
  major_grid_attr2 <- list()

minor_grid_attr1 <- get_grid_lines(info$html, names(viz)[[1]], "minor")
minor_grid_attr2 <- get_grid_lines(info$html_updated, names(viz)[[1]], "minor")
major_grid_attr1 <- get_grid_lines(info$html, names(viz)[[1]], "major")
major_grid_attr2 <- get_grid_lines(info$html_updated, names(viz)[[1]], "major")

test_that("geom_segment has grid updates",{
  # xy axis updates -> both vert/hor grids updated
  expect_false(identical(minor_grid_attr1$vert, minor_grid_attr2$vert))
  expect_false(identical(major_grid_attr1$vert, major_grid_attr2$vert))
  expect_false(identical(minor_grid_attr1$hor, minor_grid_attr2$hor))
  expect_false(identical(major_grid_attr1$hor, major_grid_attr2$hor))
})

path.i <- "//svg[@id='plot_text']//g[contains(@class, '%saxis')]"
path.xy <- sapply(c("x", "y"), sprintf, fmt=path.i)
nodes1_x <- getNodeSet(info$html, path.xy[["x"]])
nodes1_y <- getNodeSet(info$html, path.xy[["y"]])
nodes2_x <- getNodeSet(info$html_updated, path.xy[["x"]])
nodes2_y <- getNodeSet(info$html_updated, path.xy[["y"]])

original_tick_diff_x <- sapply(nodes1_x, getTickDiff, axis="x")
original_tick_diff_y <- sapply(nodes1_y, getTickDiff, axis="y")
updated_tick_diff_x <- sapply(nodes2_x, getTickDiff, axis="x")
updated_tick_diff_y <- sapply(nodes2_y, getTickDiff, axis="y")

test_that("geom_text has axis tick updates", {
  expect_false(identical(updated_tick_diff_x, original_tick_diff_x))
  expect_false(identical(updated_tick_diff_y, original_tick_diff_y))
})

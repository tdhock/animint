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
               "axis updates only work for single selection variables")


# We only update axes for single selectors
viz$selector.types = list(cyl="single")

info <- animint2HTML(viz)

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
  # initially all are same
  expect_equal(length(unique(original_tick_diff_x)), 1)
  expect_equal(length(unique(original_tick_diff_y)), 1)
  
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
  # initially all are identical
  expect_identical(major_grid_attr1$x, major_grid_attr1$neither)
  expect_identical(major_grid_attr1$y, major_grid_attr1$neither)
  expect_identical(major_grid_attr1$both, major_grid_attr1$neither)
  expect_identical(major_grid_attr1$x, major_grid_attr1$neither)
  expect_identical(major_grid_attr1$y, major_grid_attr1$neither)
  expect_identical(major_grid_attr1$both, major_grid_attr1$neither)
  
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
  # initially all are identical
  expect_identical(minor_grid_attr1$x, minor_grid_attr1$neither)
  expect_identical(minor_grid_attr1$y, minor_grid_attr1$neither)
  expect_identical(minor_grid_attr1$both, minor_grid_attr1$neither)
  expect_identical(minor_grid_attr1$x, minor_grid_attr1$neither)
  expect_identical(minor_grid_attr1$y, minor_grid_attr1$neither)
  expect_identical(minor_grid_attr1$both, minor_grid_attr1$neither)
  
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

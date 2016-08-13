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
p_names <- names(viz)[1:4]
major_grid_paths_hor <- 
  '//svg[@id="plot_%s"]//g[@class="grid_major"]//g[@class="hor"]//line'
major_grid_paths_ver <- 
  '//svg[@id="plot_%s"]//g[@class="grid_major"]//g[@class="vert"]//line'
all_major_grid_paths_hor <- sapply(p_names, sprintf,
                               fmt=major_grid_paths_hor)
all_major_grid_paths_ver <- sapply(p_names, sprintf,
                                   fmt=major_grid_paths_ver)

minor_grid_paths_hor <- 
  '//svg[@id="plot_%s"]//g[@class="grid_minor"]//g[@class="hor"]//line'
minor_grid_paths_ver <- 
  '//svg[@id="plot_%s"]//g[@class="grid_minor"]//g[@class="vert"]//line'
all_minor_grid_paths_hor <- sapply(p_names, sprintf,
                                   fmt=minor_grid_paths_hor)
all_minor_grid_paths_ver <- sapply(p_names, sprintf,
                                   fmt=minor_grid_paths_ver)

major_grid_nodes_h1 <- lapply(all_major_grid_paths_hor, getNodeSet,
                             doc=info$html)
major_grid_nodes_v1 <- lapply(all_major_grid_paths_ver, getNodeSet,
                              doc=info$html)
major_grid_nodes_h2 <- lapply(all_major_grid_paths_hor, getNodeSet,
                              doc=info$html_updated1)
major_grid_nodes_v2 <- lapply(all_major_grid_paths_ver, getNodeSet,
                              doc=info$html_updated1)

minor_grid_nodes_h1 <- lapply(all_minor_grid_paths_hor, getNodeSet,
                              doc=info$html)
minor_grid_nodes_v1 <- lapply(all_minor_grid_paths_ver, getNodeSet,
                              doc=info$html)
minor_grid_nodes_h2 <- lapply(all_minor_grid_paths_hor, getNodeSet,
                              doc=info$html_updated1)
minor_grid_nodes_v2 <- lapply(all_minor_grid_paths_ver, getNodeSet,
                              doc=info$html_updated1)

major_grid_attr_h1 <- major_grid_attr_h2 <- 
  major_grid_attr_v1 <- major_grid_attr_v2 <- list()

minor_grid_attr_h1 <- minor_grid_attr_h2 <- 
  minor_grid_attr_v1 <- minor_grid_attr_v2 <- list()

for(p.name in p_names){
  major_grid_attr_h1[[p.name]] <- sapply(major_grid_nodes_h1[[p.name]], xmlAttrs)
  major_grid_attr_v1[[p.name]] <- sapply(major_grid_nodes_v1[[p.name]], xmlAttrs)
  major_grid_attr_h2[[p.name]] <- sapply(major_grid_nodes_h2[[p.name]], xmlAttrs)
  major_grid_attr_v2[[p.name]] <- sapply(major_grid_nodes_v2[[p.name]], xmlAttrs)
  
  minor_grid_attr_h1[[p.name]] <- sapply(minor_grid_nodes_h1[[p.name]], xmlAttrs)
  minor_grid_attr_v1[[p.name]] <- sapply(minor_grid_nodes_v1[[p.name]], xmlAttrs)
  minor_grid_attr_h2[[p.name]] <- sapply(minor_grid_nodes_h2[[p.name]], xmlAttrs)
  minor_grid_attr_v2[[p.name]] <- sapply(minor_grid_nodes_v2[[p.name]], xmlAttrs)
}

test_that("major grids are updated",{
  # initially all are identical
  expect_identical(major_grid_attr_h1$x, major_grid_attr_h1$neither)
  expect_identical(major_grid_attr_h1$y, major_grid_attr_h1$neither)
  expect_identical(major_grid_attr_h1$both, major_grid_attr_h1$neither)
  expect_identical(major_grid_attr_v1$x, major_grid_attr_v1$neither)
  expect_identical(major_grid_attr_v1$y, major_grid_attr_v1$neither)
  expect_identical(major_grid_attr_v1$both, major_grid_attr_v1$neither)

  #no_updates
  expect_identical(major_grid_attr_h2$neither, major_grid_attr_h1$neither)
  expect_identical(major_grid_attr_v2$neither, major_grid_attr_v1$neither)
  
  #update_x -> only vert grids are updated
  # taking only x1,x2,y1,y2 values for comparison
  expect_identical(major_grid_attr_h2$x, major_grid_attr_h1$x)
  expect_false(identical(major_grid_attr_v2$x[1:4, ],
                         major_grid_attr_v1$x[1:4, ]))
  
  #update_y -> only hor grids are updated
  expect_false(identical(major_grid_attr_h2$y[1:4, ],
                         major_grid_attr_h1$y[1:4, ]))
  expect_identical(major_grid_attr_v2$y, major_grid_attr_v1$y)
  
  #update_xy -> both vert and hor grids updated
  expect_false(identical(major_grid_attr_h2$both[1:4, ],
                         major_grid_attr_h1$both[1:4, ]))
  expect_false(identical(major_grid_attr_v2$both[1:4, ],
                         major_grid_attr_v1$both[1:4, ]))
})

test_that("minor grids are updated",{
  # initially all are identical
  expect_identical(minor_grid_attr_h1$x, minor_grid_attr_h1$neither)
  expect_identical(minor_grid_attr_h1$y, minor_grid_attr_h1$neither)
  expect_identical(minor_grid_attr_h1$both, minor_grid_attr_h1$neither)
  expect_identical(minor_grid_attr_v1$x, minor_grid_attr_v1$neither)
  expect_identical(minor_grid_attr_v1$y, minor_grid_attr_v1$neither)
  expect_identical(minor_grid_attr_v1$both, minor_grid_attr_v1$neither)
  
  #no_updates
  expect_identical(minor_grid_attr_h2$neither, minor_grid_attr_h1$neither)
  expect_identical(minor_grid_attr_v2$neither, minor_grid_attr_v1$neither)
  
  # update_x -> only vert grids are updated
  # taking only x1,x2,y1,y2 values for comparison
  expect_identical(minor_grid_attr_h2$x, minor_grid_attr_h1$x)
  expect_false(identical(minor_grid_attr_v2$x[1:4, ],
                         minor_grid_attr_v1$x[1:4, ]))
  
  # update_y -> only hor grids are updated
  expect_false(identical(minor_grid_attr_h2$y[1:4, ],
                         minor_grid_attr_h1$y[1:4, ]))
  expect_identical(minor_grid_attr_v2$y, minor_grid_attr_v1$y)
  
  # update_xy -> both vert and hor grids updated
  expect_false(identical(minor_grid_attr_h2$both[1:4, ],
                         minor_grid_attr_h1$both[1:4, ]))
  expect_false(identical(minor_grid_attr_v2$both[1:4, ],
                         minor_grid_attr_v1$both[1:4, ]))
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

acontext("update axis ticks and grids")

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

viz <- (list(g=no_updates, 
            g1=update_x, 
            g2=update_y, 
            g3=update_xy))

expect_warning(animint2HTML(viz),
               "axis updates work for a unique single selection variable")


# We only update axes for single selectors
viz$selector.types = list(cyl="single")

info <- animint2HTML(viz)


## ------------------------------------------------------------------- ##
## Test for tick updates
rect_path <- "//svg[@id='plot_%s']//g[contains(@class, '%saxis')]"
all_rect_paths <- lapply(names(viz), sprintf, fmt=rect_path,
                           c("x","y"))[1:4]

# Take tick diffs for all 4 plots
rect_nodes1 <- sapply(all_rect_paths, getNodeSet, doc=info$html)
original_tick_diff_x <- sapply(rect_nodes1[1, ], getTickDiff, axis="x")
original_tick_diff_y <- sapply(rect_nodes1[2, ], getTickDiff, axis="y")

# Update selection and get HTML
clickID(c("plot_g_cyl_variable_8"))
Sys.sleep(0.5)
info$html_updated <- getHTML()

rect_nodes2 <- sapply(all_rect_paths, getNodeSet, doc=info$html_updated)
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
                              doc=info$html_updated)
major_grid_nodes_v2 <- lapply(all_major_grid_paths_ver, getNodeSet,
                              doc=info$html_updated)

minor_grid_nodes_h1 <- lapply(all_minor_grid_paths_hor, getNodeSet,
                              doc=info$html)
minor_grid_nodes_v1 <- lapply(all_minor_grid_paths_ver, getNodeSet,
                              doc=info$html)
minor_grid_nodes_h2 <- lapply(all_minor_grid_paths_hor, getNodeSet,
                              doc=info$html_updated)
minor_grid_nodes_v2 <- lapply(all_minor_grid_paths_ver, getNodeSet,
                              doc=info$html_updated)

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
  expect_identical(major_grid_attr_h1$g1, major_grid_attr_h1$g)
  expect_identical(major_grid_attr_h1$g2, major_grid_attr_h1$g)
  expect_identical(major_grid_attr_h1$g3, major_grid_attr_h1$g)
  expect_identical(major_grid_attr_v1$g1, major_grid_attr_v1$g)
  expect_identical(major_grid_attr_v1$g2, major_grid_attr_v1$g)
  expect_identical(major_grid_attr_v1$g3, major_grid_attr_v1$g)

  #no_updates
  expect_identical(major_grid_attr_h2$g, major_grid_attr_h1$g)
  expect_identical(major_grid_attr_v2$g, major_grid_attr_v1$g)
  
  #update_x -> only vert grids are updated
  # taking only x1,x2,y1,y2 values for comparison
  expect_identical(major_grid_attr_h2$g1, major_grid_attr_h1$g1)
  expect_false(identical(major_grid_attr_v2$g1[1:4, ],
                         major_grid_attr_v1$g1[1:4, ]))
  
  #update_y -> only hor grids are updated
  expect_false(identical(major_grid_attr_h2$g2[1:4, ],
                         major_grid_attr_h1$g2[1:4, ]))
  expect_identical(major_grid_attr_v2$g, major_grid_attr_v1$g)
  
  #update_xy -> both vert and hor grids updated
  expect_false(identical(major_grid_attr_h2$g3[1:4, ],
                         major_grid_attr_h1$g3[1:4, ]))
  expect_false(identical(major_grid_attr_v2$g3[1:4, ],
                         major_grid_attr_v1$g3[1:4, ]))
})

test_that("minor grids are updated",{
  # initially all are identical
  expect_identical(minor_grid_attr_h1$g1, minor_grid_attr_h1$g)
  expect_identical(minor_grid_attr_h1$g2, minor_grid_attr_h1$g)
  expect_identical(minor_grid_attr_h1$g3, minor_grid_attr_h1$g)
  expect_identical(minor_grid_attr_v1$g1, minor_grid_attr_v1$g)
  expect_identical(minor_grid_attr_v1$g2, minor_grid_attr_v1$g)
  expect_identical(minor_grid_attr_v1$g3, minor_grid_attr_v1$g)
  
  #no_updates
  expect_identical(minor_grid_attr_h2$g, minor_grid_attr_h1$g)
  expect_identical(minor_grid_attr_v2$g, minor_grid_attr_v1$g)
  
  # update_x -> only vert grids are updated
  # taking only x1,x2,y1,y2 values for comparison
  expect_identical(minor_grid_attr_h2$g1, minor_grid_attr_h1$g1)
  expect_false(identical(minor_grid_attr_v2$g1[1:4, ],
                         minor_grid_attr_v1$g1[1:4, ]))
  
  # update_y -> only hor grids are updated
  expect_false(identical(minor_grid_attr_h2$g2[1:4, ],
                         minor_grid_attr_h1$g2[1:4, ]))
  expect_identical(minor_grid_attr_v2$g, minor_grid_attr_v1$g)
  
  # update_xy -> both vert and hor grids updated
  expect_false(identical(minor_grid_attr_h2$g3[1:4, ],
                         minor_grid_attr_h1$g3[1:4, ]))
  expect_false(identical(minor_grid_attr_v2$g3[1:4, ],
                         minor_grid_attr_v1$g3[1:4, ]))
})

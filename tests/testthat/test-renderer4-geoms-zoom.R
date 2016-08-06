acontext("zooming of geoms")

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
# We only update axes for single selectors
viz$selector.types = list(cyl="single")

info <- animint2HTML(viz)

# Update selection and get HTML
clickID(c("plot_g_cyl_variable_8"))
Sys.sleep(0.5)
info$html_updated1 <- getHTML()

# Update selection and get HTML
clickID(c("plot_g_cyl_variable_4"))
Sys.sleep(0.5)
info$html_updated2 <- getHTML()

## Get ranges of geoms
no_updates_nodes1 <-
  getNodeSet(info$html_updated1, '//g[@class="geom1_point_g"]//circle')
no_updates_nodes2 <-
  getNodeSet(info$html_updated2, '//g[@class="geom1_point_g"]//circle')
no_updates_attrs1 <- sapply(no_updates_nodes1, xmlAttrs)[c("cx", "cy"), ]
no_updates_attrs2 <- sapply(no_updates_nodes2, xmlAttrs)[c("cx", "cy"), ]
no_updates_xranges1 <- range(as.numeric(no_updates_attrs1[1, ]), na.rm = T)
no_updates_xranges2 <- range(as.numeric(no_updates_attrs2[1, ]), na.rm = T)
no_updates_yranges1 <- range(as.numeric(no_updates_attrs1[2, ]), na.rm = T)
no_updates_yranges2 <- range(as.numeric(no_updates_attrs2[2, ]), na.rm = T)

x_updates_nodes1 <-
  getNodeSet(info$html_updated1, '//g[@class="geom2_point_g1"]//circle')
x_updates_nodes2 <-
  getNodeSet(info$html_updated2, '//g[@class="geom2_point_g1"]//circle')
x_updates_attrs1 <- sapply(x_updates_nodes1, xmlAttrs)[c("cx", "cy"), ]
x_updates_attrs2 <- sapply(x_updates_nodes2, xmlAttrs)[c("cx", "cy"), ]
x_updates_xranges1 <- range(as.numeric(x_updates_attrs1[1, ]), na.rm = T)
x_updates_xranges2 <- range(as.numeric(x_updates_attrs2[1, ]), na.rm = T)
x_updates_yranges1 <- range(as.numeric(x_updates_attrs1[2, ]), na.rm = T)
x_updates_yranges2 <- range(as.numeric(x_updates_attrs2[2, ]), na.rm = T)

y_updates_nodes1 <-
  getNodeSet(info$html_updated1, '//g[@class="geom3_point_g2"]//circle')
y_updates_nodes2 <-
  getNodeSet(info$html_updated2, '//g[@class="geom3_point_g2"]//circle')
y_updates_attrs1 <- sapply(y_updates_nodes1, xmlAttrs)[c("cx", "cy"), ]
y_updates_attrs2 <- sapply(y_updates_nodes2, xmlAttrs)[c("cx", "cy"), ]
y_updates_xranges1 <- range(as.numeric(y_updates_attrs1[1, ]), na.rm = T)
y_updates_xranges2 <- range(as.numeric(y_updates_attrs2[1, ]), na.rm = T)
y_updates_yranges1 <- range(as.numeric(y_updates_attrs1[2, ]), na.rm = T)
y_updates_yranges2 <- range(as.numeric(y_updates_attrs2[2, ]), na.rm = T)

xy_updates_nodes1 <-
  getNodeSet(info$html_updated1, '//g[@class="geom4_point_g3"]//circle')
xy_updates_nodes2 <-
  getNodeSet(info$html_updated2, '//g[@class="geom4_point_g3"]//circle')
xy_updates_attrs1 <- sapply(xy_updates_nodes1, xmlAttrs)[c("cx", "cy"), ]
xy_updates_attrs2 <- sapply(xy_updates_nodes2, xmlAttrs)[c("cx", "cy"), ]
xy_updates_xranges1 <- range(as.numeric(xy_updates_attrs1[1, ]), na.rm = T)
xy_updates_xranges2 <- range(as.numeric(xy_updates_attrs2[1, ]), na.rm = T)
xy_updates_yranges1 <- range(as.numeric(xy_updates_attrs1[2, ]), na.rm = T)
xy_updates_yranges2 <- range(as.numeric(xy_updates_attrs2[2, ]), na.rm = T)


test_that("geoms get zoomed-in upon changing selection", {
  # no_updates
  expect_false(isTRUE(all.equal(no_updates_xranges2, no_updates_xranges1)))
  expect_false(isTRUE(all.equal(no_updates_yranges2, no_updates_yranges1)))
  
  # x_updates
  expect_equal(x_updates_xranges2, x_updates_xranges1)
  expect_false(isTRUE(all.equal(x_updates_yranges2, x_updates_yranges1)))
  
  # y_updates
  expect_false(isTRUE(all.equal(y_updates_xranges2, y_updates_xranges1)))
  expect_equal(y_updates_yranges2, y_updates_yranges1)
  
  # xy_updates
  expect_equal(xy_updates_xranges2, xy_updates_xranges1)
  expect_equal(xy_updates_yranges2, xy_updates_yranges1)
})

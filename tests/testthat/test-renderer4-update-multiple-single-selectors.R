acontext("multiple single selectors- axis updates")

# Plots with axis updates
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$hp <- as.factor(mtcars$hp)

no_updates <- ggplot()+geom_point(aes(mpg, disp, 
                                      colour=cyl, showSelected=hp), 
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
viz$selector.types = list(cyl="single", hp="single")

## TODO: Implement axis updates for multiple single selectors
expect_silent(animint2HTML(viz))

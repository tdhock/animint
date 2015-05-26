context("hjust text anchor simple")

# demonstration of gradient descent algorithm from animation package
grad.desc <- function(
  FUN = function(x, y) x^2 + 2 * y^2, rg = c(-3, -3, 3, 3), init = c(-3, 3),
  gamma = 0.05, tol = 0.001, gr = NULL, len = 50, nmax = 50) {
  x <- seq(rg[1], rg[3], length = len)
  y <- seq(rg[2], rg[4], length = len)
  contour <- expand.grid(x = x, y = y)
  contour$z <- as.vector(outer(x, y, FUN))
  
  nms = names(formals(FUN))
  grad = if (is.null(gr)) {
    deriv(as.expression(body(FUN)), nms, function.arg = TRUE)
  } else {
    function(...) {
      res = FUN(...)
      attr(res, 'gradient') = matrix(gr(...), nrow = 1, ncol = 2)
      res
    }
  }
  
  xy <- init
  newxy <- xy - gamma * attr(grad(xy[1], xy[2]), 'gradient')
  z <- FUN(newxy[1], newxy[2])
  gap <- abs(z - FUN(xy[1], xy[2]))
  i <- 1
  while (gap > tol && i <= nmax) {
    xy <- rbind(xy, newxy[i, ])
    newxy <- rbind(newxy, xy[i + 1, ] - gamma * attr(grad(xy[i + 1, 1], xy[i + 1, 2]), 'gradient'))
    z <- c(z, FUN(newxy[i + 1, 1], newxy[i + 1, 2]))
    gap <- abs(z[i + 1] - FUN(xy[i + 1, 1], xy[i + 1, 2]))
    i <- i + 1
    if (i > nmax) warning('Maximum number of iterations reached!')
  }
  objective <- data.frame(iteration = 1:i, x = xy[, 1], y = xy[, 2], z = z)
  invisible(list(contour = contour, objective = objective))
}

dat <- grad.desc()
objective <- dat$objective

grad.desc.viz <- function(hjust) {
  objective$hjust <- hjust
  
  objective.plot <- ggplot() +
    geom_line(data = objective, aes(x = iteration, y = z), colour = "red") + 
    geom_point(data = objective, aes(x = iteration, y = z), colour = "red") + 
    geom_tallrect(data = objective, aes(xmin = iteration - 1 / 2, xmax = iteration + 1 / 2, 
                                         clickSelects = iteration), alpha = .3) + 
    geom_text(data = objective, aes(x = iteration, y = z + 0.3, showSelected = iteration, 
                                     label = iteration), hjust = hjust) + 
    ggtitle("objective value vs. iteration") + 
    theme_animint(width = 600, height = 600)
  
  viz <- list(objective = objective.plot, time = list(variable = "iteration", ms = 2000), 
              title = "Demonstration of Gradient Descent Algorithm")
}

test_that('geom_text(hjust=0) => <text style="text-anchor: start">', {
  viz <- grad.desc.viz(hjust = 0)
  info <- animint2HTML(viz)
  style.value <- getStyleValue(info$html, '//g[@class="geom4_text_objective"]//text', 
                               "text-anchor")
  expect_match(style.value, "start")
})
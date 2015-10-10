acontext("hjust text anchor")

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
contour <- dat$contour
objective <- dat$objective
objective <- plyr::ldply(objective$iteration, function(i) {
  df <- subset(objective, iteration <= i)
  cbind(df, iteration2 = i)
})
objective2 <- subset(objective, iteration == iteration2)

grad.desc.viz <- function(hjust) {
  objective2$hjust <- hjust
  
  contour.plot <- ggplot() + 
    geom_contour(data = contour, aes(x = x, y = y, z = z, colour = ..level..), size = .5) + 
    scale_colour_continuous(name = "z value") + 
    geom_path(data = objective, aes(x = x, y = y, showSelected = iteration2), 
              colour = "red", size = 1) + 
    geom_point(data = objective, aes(x = x, y = y, showSelected = iteration2), colour = "green", 
               size = 2) + 
    geom_text(data = objective2, aes(x = x, y = y - 0.2, showSelected = iteration2, label = round(z, 2))) + 
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) + 
    ggtitle("contour of function value") + 
    theme_animint(width = 600, height = 600)
  
  objective.plot <- ggplot() +
    geom_line(data = objective2, aes(x = iteration, y = z), colour = "red") + 
    geom_point(data = objective2, aes(x = iteration, y = z), colour = "red") + 
    geom_tallrect(data = objective2, aes(xmin = iteration - 1 / 2, xmax = iteration + 1 / 2, 
                                         clickSelects = iteration2), alpha = .3) + 
    geom_text(data = objective2, aes(x = iteration, y = z + 0.3, showSelected = iteration2, 
                                     label = iteration), hjust = hjust) + 
    ggtitle("objective value vs. iteration") + 
    theme_animint(width = 600, height = 600)
  
  viz <- list(contour = contour.plot, objective = objective.plot, 
              time = list(variable = "iteration2", ms = 2000), 
              title = "Demonstration of Gradient Descent Algorithm")
}

viz <- grad.desc.viz(hjust = 0)
info <- animint2HTML(viz)

test_that("unspecified hjust means text-anchor: middle (other hjust=0)", {
  style.value <-
    getStyleValue(info$html, '//g[@class="geom4_text_contour"]//text', 
                  "text-anchor")
  expect_match(style.value, "middle")
})  

test_that('geom_text(hjust=0) => <text style="text-anchor: start">', {
  style.value <-
    getStyleValue(info$html, '//g[@class="geom8_text_objective"]//text', 
                  "text-anchor")
  expect_match(style.value, "start")
})

viz <- grad.desc.viz(hjust = 1)
info <- animint2HTML(viz)

test_that("unspecified hjust means text-anchor: middle (other hjust=1)", {
  style.value <-
    getStyleValue(info$html, '//g[@class="geom4_text_contour"]//text', 
                  "text-anchor")
  expect_match(style.value, "middle")
})  

test_that('geom_text(hjust=1) => <text style="text-anchor: end">', {
  style.value <-
    getStyleValue(info$html, '//g[@class="geom8_text_objective"]//text', 
                  "text-anchor")
  expect_match(style.value, "end")
})

viz <- grad.desc.viz(hjust = 0.5)
info <- animint2HTML(viz)

test_that("unspecified hjust means text-anchor: middle (other hjust=0.5)", {
  style.value <-
    getStyleValue(info$html, '//g[@class="geom4_text_contour"]//text', 
                  "text-anchor")
  expect_match(style.value, "middle")
})  

test_that('geom_text(hjust=0.5) => <text style="text-anchor: middle">', {
  style.value <-
    getStyleValue(info$html, '//g[@class="geom8_text_objective"]//text', 
                  "text-anchor")
  expect_match(style.value, "middle")
})

test_that('geom_text(hjust=other) => unsupported value error', {
  viz <- grad.desc.viz(hjust = 0.8)
  expect_error(animint2HTML(viz), "animint only supports hjust values 0, 0.5, 1")
})

# adapted from  https://github.com/rstudio/ggvis/blob/master/demo/tourr.r
library(tourr)
library(animint)

mat <- rescale(as.matrix(flea[1:6]))
tour <- new_tour(mat, grand_tour(), NULL)

tour_dat <- function(step_size) {
  step <- tour(step_size)
  proj <- center(mat %*% step$proj)
  data.frame(x = proj[,1], y = proj[,2], 
             species = flea$species)
}

steps <- c(0, rep(1/15, 200))
stepz <- cumsum(steps)
dats <- lapply(steps, tour_dat)
datz <- Map(function(x, y) cbind(x, step = y), dats, stepz)
dat <- do.call("rbind", datz)

p <- ggplot() + 
  geom_point(data = dat, 
             aes(x = x, y = y, colour = species, showSelected = step))
plist <- list(
  plot = p,
  time = list(variable = "step", ms = 100),
  duration = list(step = 200)
)
animint2dir(plist, "tour", open.browser = FALSE)
servr::httd("tour")

# TODO: how can we acheive the same effect without computing all projections apriori?
# POSSIBLE SOLUTIONS:
# (1) Make animint smart enough to transition after receiving new data from remote R session.
# (2) Reimplement tourr in JavaScript (lol)

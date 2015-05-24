#' Data Source:
#' National Geospatial Intelligence Agency (NGIA)
#' lat/lon locations and descriptions of pirate attacks 
#' 
#' Data contains the latitude and longitude coordinates 
#' of pirate attacks from 1978-2013.  In some cases, 
#' a description of the attacks is also provided.
#' 
#' See ?animint::pirates for more information

library(ggplot2)
library(animint)
library(dplyr)      ## for data manipulation
library(tidyr)      ## for data manipulation
library(maps)       ## to plot world map
library(lubridate)  ## to work with dates
library(stringr)    ## to work with strings
theme_set(theme_bw())

# functions to create 2d bins ---------------------------

# generates the bins needed for 2d binning
bin_2d <- function(x, y, xbins, ybins) {
  
  # binwidths
  x_width <- diff(range(x)) / xbins
  y_width <- diff(range(y)) / ybins
  # cutpoints
  x_cuts <- seq(min(x), max(x), by = x_width)
  y_cuts <- seq(min(y), max(y), by = y_width)
  
  # setting up all the bins
  x_bins <- interaction(x_cuts[-length(x_cuts)], x_cuts[-1], sep = "_")
  y_bins <- interaction(y_cuts[-length(y_cuts)], y_cuts[-1], sep = "_")
  bins <- expand.grid(x_bins, y_bins)
  
  # breaking into columns for x and y lower and upper points
  x_bins2 <- t(sapply(bins$Var1, function(z) str_split(z, "_")[[1]] ))
  y_bins2 <- t(sapply(bins$Var2, function(z) str_split(z, "_")[[1]] ))
  bins <- data.frame(x_low = as.numeric(x_bins2[, 1]), 
                     x_hi = as.numeric(x_bins2[, 2]), 
                     y_low = as.numeric(y_bins2[, 1]), 
                     y_hi = as.numeric(y_bins2[, 2]))
  # identifying mean of each bin
  bins$xmid <- (bins$x_low + bins$x_hi) / 2
  bins$ymid <- (bins$y_low + bins$y_hi) / 2
  bins
}

# counts the number of points in each 2d bin
# double counting points on boundary (not worried about this)
count_bins <- function(bins, x, y) {
  bins$count <- apply(bins, 1, function(z) {
    mean(x >= z[1] & x <= z[2] & y >= z[3] & y <= z[4]) * length(x)
  })
  subset(bins, select = c(xmid, ymid, count))
}

# data -------------------------------------------------

# loading data
data(pirates, package = "animint")
countries <- map_data("world") %>% 
  filter(region != "Antarctica")

# remove data prior to 1995 - it's not that interesting
p_df <- pirates %>% 
  tbl_df() %>% 
  arrange(DateOfOcc) %>% 
  mutate(date = year(ymd(as.character(DateOfOcc)))) %>% 
  filter(date > 1995)

# aggregating by date
p_df2 <- p_df %>% 
  group_by(date) %>% 
  summarise(attacks = n()) %>% 
  ungroup() %>% 
  mutate(total_attacks = cumsum(attacks))

# binning data --------------------------------------------
# I'm sure that there is a better way to do this

## generating tiles
## each year, I will count the number of attacks in each tile
bin_df <- bin_2d(p_df$coords.x1, p_df$coords.x2, 60, 30) %>% tbl_df()

# counting attacks in each tile in each year
d <- p_df %>% 
  select(date, coords.x1, coords.x2) %>% 
  group_by(date) %>% 
  do(count = count_bins(bin_df, .$coords.x1, .$coords.x2))

## merging dates and counts
## should figure out how to do this with dplyr
l <- list()
for(i in 1:nrow(d)) {
  l[[i]] <- d$count[[i]]
  l[[i]]$date <- d$date[i]
}

# unnesting the list and counting total attacks
p_df3 <- l %>% 
  unnest() %>% 
  arrange(date, xmid, ymid) %>% 
  group_by(xmid, ymid) %>% 
  mutate(attacks = cumsum(count)) %>% 
  ungroup() %>% 
  filter(attacks > 0)

# animint plots -----------------------------------------

# total number of attacks
p_time <- ggplot() + 
  geom_line(aes(date, total_attacks), data = p_df2) + 
  make_tallrect(p_df2, "date") + 
  labs(y = "Total Attacks", x = "Date") + 
  theme_animint(width = 450)

# points on world map
p_points <- ggplot() + 
  geom_rect(aes(xmin = -180, xmax = 192, ymin = -60, ymax = 84), 
            fill = "lightblue", size = I(.3)) + 
  geom_polygon(aes(long, lat, group = group), size = I(1), 
               data = countries, fill = "lightgrey", colour = "darkgreen") +
  geom_point(aes(coords.x1, coords.x2, showSelected = date), 
             size = 3, alpha = I(.5), data = p_df) + 
  make_text(p_df, 0, 90, "date", "Pirate Attacks in %d") + 
  theme(axis.line=element_blank(), axis.text=element_blank(), 
        axis.ticks=element_blank(), axis.title=element_blank(), 
        panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + 
  theme_animint(width = 800)

# tiles on world map
p_tiles <- ggplot() + 
  geom_rect(aes(xmin = -180, xmax = 192, ymin = -60, ymax = 84), 
            fill = "lightblue", size = I(.3)) + 
  geom_polygon(aes(long, lat, group = group), size = I(1), 
               data = countries, fill = "lightgrey", colour = "darkgreen") +
  geom_tile(aes(xmid, ymid, fill = log(attacks), 
                clickSelects = id, showSelected = date), 
            data = p_df3, colour = I("red")) + 
  make_text(p_df, 0, 90, "date", "Pirate Attacks from 1995 to %d") + 
  scale_fill_gradient(low = "#fee5d9", high = "#a50f15", name = "Attacks", 
                      labels = c(1, 10, 50, 400), 
                      breaks = log(c(1, 10, 50, 400))) + 
  theme(axis.line=element_blank(), axis.text=element_blank(), 
        axis.ticks=element_blank(), axis.title=element_blank(), 
        panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + 
  theme_animint(width = 800)

# tiles over time
p_time2 <- ggplot() + 
  geom_line(aes(date, log(attacks), group = id, showSelected = id), 
            data = p_df3)

# passing to animint
ani_list <- list( 
  points = p_points, 
  timeTotal = p_time,
  tileTotal = p_tiles, 
  time = list(variable = "date", ms = 300)
)
animint2dir(ani_list, "pirates_viz", open.browser = FALSE)
servr::httd("pirates_viz")

animint2gist(ani_list, "Pirate Attacks Since 1995")

context("save separate chunks")

# retrieve state-level data from the CDC's FluView Portal and save as FluView.RData
# under animint/data directory
# library(cdcfluview)
# state_flu <- get_state_data(2008:2014)
# save(state_flu, file = "~/Documents/R/packages/animint/data/FluView.RData", 
#      compress = "xz")
data(FluView)

# data clean
state_flu <- state_flu[, !names(state_flu) %in% c("URL", "WEBSITE")]
state_flu$state <- tolower(state_flu$STATENAME)
state_flu$level <- as.numeric(gsub("Level ", "", state_flu$ACTIVITY.LEVEL))
state_flu$WEEKEND <- as.Date(state_flu$WEEKEND, format = "%b-%d-%Y")
max(state_flu$WEEKEND)
state_flu <- subset(state_flu, WEEKEND <= as.Date("2015-05-23") & 
                      !STATENAME %in% c("District of Columbia", "New York City", 
                                        "Puerto Rico", "Alaska", "Hawaii"))

### visualize CDC FluView data
library(animint)
library(plyr)

# activity level heatmap
level.heatmap <- ggplot() + 
  geom_tile(data = state_flu, aes(x = WEEKEND, y = STATENAME, fill = level, 
                                  clickSelects = WEEKEND)) + 
  geom_tallrect(aes(xmin = WEEKEND - 3, xmax = WEEKEND + 3, clickSelects = WEEKEND), 
                data = state_flu, alpha = .5) + 
  scale_x_date(expand = c(0, 0)) + 
  scale_fill_gradient2(low = "white", high = "red", breaks = 0:10) + 
  theme_animint(width = 1200, height = 700) + 
  ggtitle("CDC ILI Activity Level in Lower 48 States")

# state map
theme_opts <- list(theme(panel.grid.minor = element_blank(), 
                         panel.grid.major = element_blank(), 
                         panel.background = element_blank(), 
                         panel.border = element_blank(), 
                         plot.background = element_rect(fill = "#E6E8Ed"), 
                         axis.line = element_blank(), 
                         axis.text.x = element_blank(), 
                         axis.text.y = element_blank(), 
                         axis.ticks = element_blank(), 
                         axis.title.x = element_blank(), 
                         axis.title.y = element_blank()))

USpolygons <- map_data("state")
USpolygons$subregion <- NULL
USpolygons <- subset(USpolygons, region != "district of columbia")

USdots <- ddply(USpolygons, .(region), summarise, mean.lat = mean(lat), 
                      mean.long = mean(long))

# add state flu
map_flu <- ldply(unique(state_flu$WEEKEND), function(we) {
  df <- subset(state_flu, WEEKEND == we)
  # merge(USpolygons, df, by.x = "region", by.y = "state")
  merge(USdots, df, by.x = "region", by.y = "state")
})

state.map <- ggplot() + 
  make_text(map_flu, -100, 50, "WEEKEND", "CDC FluView in Lower 48 States ending %s") + 
#   geom_polygon(data = map_flu, aes(x = long, y = lat, group = group, fill = level, 
#                                    showSelected = WEEKEND), 
#                colour = "black", size = 1) + 
  geom_point(data = map_flu, aes(x = mean.long, y = mean.lat, fill = level, 
                                 showSelected = WEEKEND), 
             color = "black", size = 10) + 
  scale_fill_gradient2(low = "white", high = "red", breaks = 0:10, guide = "none") + 
  theme_opts + 
  theme_animint(width = 750, height= 500)

viz <- list(levelHeatmap = level.heatmap, stateMap = state.map, title = "FluView")
animint2dir(viz, out.dir = "FluView")
# animint2gist(viz, out.dir = "FluView")
library("animint")
data("pirates")

summary(duplicated(pirates)) # duplicate records! Any idea why??
pirates <- unique(pirates)
# help(pirates) # variable descriptions are lacking
names(pirates) <- gsub("coords.x1", "long", names(pirates))
names(pirates) <- gsub("coords.x2", "lat", names(pirates))
pirates$year <- as.integer(substr(pirates$DateOfOcc, 0, 4))

world <- map_data("world")
max_lat <- max(world$lat)
min_lat <- min(world$lat)
max_long <- max(world$long)
min_long <- min(world$long)
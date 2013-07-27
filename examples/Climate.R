#' 2006 Data Expo: 
#' Data source: 
#' NASA Goddard Institute for Space Studies (GISS)
#' subset of the monthly climatology of the International 
#' Satellite Cloud Climatology Project (ISCCP), which was
#' developed “to collect weather satellite radiance
#' measurements and to analyze them to infer the
#' global distribution of clouds, their properties, and
#' their diurnal, seasonal and interannual variations.”
#' 
#' Data contains: Monthly observations of atmospheric variables 1995-2000, 
#' between 113.75ºW-56.25ºW, 21.25ºS-36.25ºN with 2.5º grid
#' spacing.
#' 
#' Variables: pressure, temperature, ozone and low, medium, 
#' high cloud cover.
#' The pressure, ozone and near-surface temperature
#' observations are computed from the TIROS Optical Vertical 
#' Sounder (TOVS) satellite climatology observations
#'
#' Temperatures are given in degrees celsius (original data had Kelvin).

library(animint)
library(ggplot2)
library(maps)
data(climate)
climate$time2 <- climate$year + climate$month/12-1/24 # create continuous time variable.

countries <- map_data("world")
countries <- subset(countries, (lat < 38)&(lat>-24))
countries <- subset(countries, ((-long)>54)&((-long)<118))


temp.seq <- ggplot() + 
  make_tallrect(data=climate, "time2") + 
  geom_line(data=climate, aes(x=time2, y=temperature, group=id, clickSelects = id, colour=lat), alpha=.4) + 
  scale_colour_gradient2(low="purple", mid="white", high="orange")

clouds.high <- ggplot() + 
  geom_tile(data=climate, aes(x=long, y=lat, fill=cloudhigh, clickSelects=id, showSelected=time2), colour="grey") + 
  scale_fill_gradient("Coverage", low="skyblue", high="white", limits=c(0, 75)) + 
  geom_path(data=countries, aes(x=long, y=lat, group=group)) + 
  ggtitle("High Altitute Cloud Cover")

clouds.mid <- ggplot() + 
  geom_tile(data=climate, aes(x=long, y=lat, fill=cloudmid, clickSelects=id, showSelected=time2), colour="grey") + 
  scale_fill_gradient("Coverage", low="skyblue", high="white", limits=c(0, 75)) + 
  geom_path(data=countries, aes(x=long, y=lat, group=group)) + 
  ggtitle("Mid Altitute Cloud Cover")

clouds.low <- ggplot() + 
  geom_tile(data=climate, aes(x=long, y=lat, fill=cloudlow, clickSelects=id, showSelected=time2), colour="grey") + 
  scale_fill_gradient("Coverage", low="skyblue", high="white", limits=c(0, 75)) + 
  geom_path(data=countries, aes(x=long, y=lat, group=group)) + 
  ggtitle("Low Altitute Cloud Cover")

ozone.map <- ggplot() + 
  geom_tile(data=climate, aes(x=long, y=lat, fill=ozone, clickSelects=id, showSelected=time2), colour="grey") + 
  scale_fill_gradient("Concentration", low="white", high="brown") + 
  geom_path(data=countries, aes(x=long, y=lat, group=group)) + 
  ggtitle("Ozone Concentration")

gg2animint(list(temperature=temp.seq, 
                cloudslow = clouds.low, 
                cloudsmid=clouds.mid, 
                cloudshigh=clouds.high, 
                ozone = ozone.map,
                time = list(variable="time2", ms=2000)
                ))
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
library(lubridate)
library(plyr)

data(climate)
# climate$time2 <- climate$year + floor(climate$month/3)/4 
climate$time2 <- decimal_date(ymd(as.character(climate$date)))
countries <- map_data("world")
countries <- subset(countries, (lat < 38)&(lat>-24))
countries <- subset(countries, ((-long)>54)&((-long)<118))


temp.seq <- ggplot() + 
  make_tallrect(data=climate, "time2") + 
  geom_line(data=climate, aes(x=time2, y=temperature, showSelected=id))

clouds.high <- ggplot() + 
  geom_tile(data=climate, aes(x=long, y=lat, fill=cloudhigh, clickSelects=id, showSelected=time2, colour=I("grey")))+ 
  scale_fill_gradient("Coverage", low="skyblue", high="white", limits=c(0, 75)) + 
  geom_path(data=countries, aes(x=long, y=lat, group=group)) + 
  ggtitle("High Altitute Cloud Cover")+ 
  theme(axis.line=element_blank(), axis.text=element_blank(), 
        axis.ticks=element_blank(), axis.title=element_blank())

clouds.mid <- ggplot() + 
  geom_tile(data=climate, aes(x=long, y=lat, fill=cloudmid, clickSelects=id, showSelected=time2, colour=I("grey")))+ 
  scale_fill_gradient("Coverage", low="skyblue", high="white", limits=c(0, 75)) + 
  geom_path(data=countries, aes(x=long, y=lat, group=group)) + 
  ggtitle("Mid Altitute Cloud Cover")+ 
  theme(axis.line=element_blank(), axis.text=element_blank(), 
        axis.ticks=element_blank(), axis.title=element_blank())

clouds.low <- ggplot() + 
  geom_tile(data=climate, aes(x=long, y=lat, fill=cloudlow, clickSelects=id, showSelected=time2, colour=I("grey")))+ 
  scale_fill_gradient("Coverage", low="skyblue", high="white", limits=c(0, 75)) + 
  geom_path(data=countries, aes(x=long, y=lat, group=group)) + 
  ggtitle("Low Altitute Cloud Cover")+ 
  theme(axis.line=element_blank(), axis.text=element_blank(), 
        axis.ticks=element_blank(), axis.title=element_blank())

ozone.map <- ggplot() + 
  geom_tile(data=climate, aes(x=long, y=lat, fill=ozone, clickSelects=id, showSelected=time2, colour=I("grey")))+ 
  scale_fill_gradient("Concentration", low="white", high="brown") + 
  geom_path(data=countries, aes(x=long, y=lat, group=group)) + 
  ggtitle("Ozone Concentration")+ 
  theme(axis.line=element_blank(), axis.text=element_blank(), 
        axis.ticks=element_blank(), axis.title=element_blank())

# Create variable showing temp-avg.monthly.temp at that location
climate <- ddply(climate, .(id, month), transform, tempdev = temperature - mean(temperature))

temperature.map <- ggplot() + 
  geom_tile(data=climate, aes(x=long, y=lat, fill=tempdev, clickSelects=id, showSelected=time2, colour=I("grey")))+ 
  scale_fill_gradient2("Temperature", low="blue", mid="white", high="red", limits=c(-20, 20), midpoint=0) + 
  geom_path(data=countries, aes(x=long, y=lat, group=group)) + 
  ggtitle("Temperature Deviation from Monthly Norm")+ 
  theme(axis.line=element_blank(), axis.text=element_blank(), 
        axis.ticks=element_blank(), axis.title=element_blank())

gg2animint(list(temperature = temp.seq, 
                cloudslow = clouds.low, 
                cloudsmid = clouds.mid, 
                cloudshigh = clouds.high, 
                ozone = ozone.map,
                tempmap = temperature.map,
                time = list(variable="time2", ms=3000),
                width = list(450),
                height = list(450)
                ))

#' NOAA SVRGIS data (Severe Report Database + Geographic Information System)
#' http://www.spc.noaa.gov/gis/svrgis/
#' Data - http://www.spc.noaa.gov/wcm/#data
#' Location Codes - http://www.spc.noaa.gov/wcm/loccodes.html
#' State FIPS Codes - http://www.spc.noaa.gov/wcm/fips_usa.gif
#' County FIPS Codes - http://www.spc.noaa.gov/wcm/stnindex_all.txt
#' 
#' Image Inspiration -  http://www.kulfoto.com/pic/0001/0033/b/h4n5832833.jpg
library(maps)
library(plyr)
library(ggplot2)
library(animint)

url <- "http://www.spc.noaa.gov/wcm/data/1950-2012_torn.csv"
data <- read.csv("./data/1950-2012_torn.csv", header=FALSE)
names(data) <- c("ID", "year", "month", "day", "date", "time", "tz", "state", "fips", "state.tnum", "f", "injuries", "fatalities", "propertyLoss", "cropLoss", "startLat", "startLong", "endLat", "endLong", "trackLength", "trackWidth", "numStatesAffected", "stateNumber", "segmentNumber", "FipsCounty1", "FipsCounty2", "FipsCounty3", "FipsCounty4")

continentalUS <- function(lat, long){
  apply(cbind(lat>25.2, lat<49.4, long> -124.7, long< -67.1), 1, prod)
}
data3 <- sapply(1:nrow(data), function(i){ 
  df <- data[i,]
  # if tornado was <.2 miles long and coords are missing, set start=end or end=start.
  if(df[16]==0 & df[18]!=0 & df[20]<.2){ df[16] = df[18] }
  if(df[17]==0 & df[19] !=0 & df[20]<.2){ df[17] = df[19] }
  if(df[16]!=0 & df[18]==0 & df[20]<.2){ df[18] = df[16] }
  if(df[17]!=0 & df[19] ==0 & df[20]<.2){ df[19] = df[17] }
  df
})
data2 <- subset(data3, continentalUS(startLat, startLong) & continentalUS(endLat, endLong) & f>=0)
data2$time <- data2$year + (data2$month-.5)/12
save("data", file="./data/Tornadoes.RData")


states_map <- map_data("state")

ggplot() + geom_polygon(data=states_map, aes(x=long, y=lat, group=group), fill="black", colour="grey") +
  geom_segment(data=data2, aes(x=startLong, y=startLat, xend=endLong, yend=endLat, size=trackWidth, alpha=f), colour="skyblue") +
  scale_size_continuous("Width (yd)", range=c(.5, 2)) + 
  scale_alpha_continuous("Strength (F or EF scale)")

ggplot() + geom_histogram(data=data2, aes(x=time))
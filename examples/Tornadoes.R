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

# url <- "http://www.spc.noaa.gov/wcm/data/1950-2012_torn.csv"
data <- read.csv("../data/1950-2012_torn.csv", header=FALSE)
names(data) <- c("ID", "year", "month", "day", "date", "time", "tz", "state", "fips", "state.tnum", "f", "injuries", "fatalities", "propertyLoss", "cropLoss", "startLat", "startLong", "endLat", "endLong", "trackLength", "trackWidth", "numStatesAffected", "stateNumber", "segmentNumber", "FipsCounty1", "FipsCounty2", "FipsCounty3", "FipsCounty4")

continentalUS <- function(lat, long){
  apply(cbind(lat>25.2, lat<49.4, long> -124.7, long< -67.1), 1, prod)
}

data2 <- data
# if startLat is 0 and endLat is >0 and dist < .2, set startLat = endLat
idx <- which(data$startLat==0 & data$endLat!=0 & data$trackLength<.2)
data2$startLat[idx] <- data2$endLat[idx]
# if startLat is > 0 and endLat is 0 and dist < .2, set endLat = startLat
idx <- which(data$startLat!=0 & data$endLat==0 & data$trackLength<.2)
data2$endLat[idx] <- data2$startLat[idx]
# if startLong is 0 and endLong is >0 and dist < .2, set startLong = endLong
idx <- which(data$startLong==0 & data$endLong!=0 & data$trackLength<.2)
data2$startLong[idx] <- data2$endLong[idx]
# if startLong is > 0 and endLong is 0 and dist < .2, set endLong = startLong
idx <- which(data$startLong!=0 & data$endLong==0 & data$trackLength<.2)
data2$endLong[idx] <- data2$startLong[idx]

# continuous-ish time
data2$time <- data2$year + (data2$month-.5)/12
save("data2", file="./data/Tornadoes.RData")

data3 <- subset(data2, continentalUS(startLat, startLong) & continentalUS(endLat, endLong) & f>=0)

states_map <- map_data("state")
quickfacts <- read.csv("http://quickfacts.census.gov/qfd/download/DataSet.txt")
quickfacts <- quickfacts[,c(1, 2, 51)]
names(quickfacts) <- c("FipsCounty", "TotalPop2012", "LandArea")
state.facts <- quickfacts[which(quickfacts$FipsCounty%%1000==0),]
state.facts$FipsCounty <- state.facts$FipsCounty/1000 # to conform with tornadoes dataset state FIPS codes
names(state.facts) <- c("fips", "TotalPop2012", "LandArea")

data3 <- merge(data3, state.facts, all.x=TRUE)
data3 <- ddply(data3, .(fips, f), transform, TornadoesSqMile = length(LandArea)/LandArea)

states_map$state = state.abb[match(states_map$region, tolower(state.name))]

stateOrder <- data.frame(state = unique(data3$state)[order(unique(data3$TornadoesSqMile), decreasing=T)],
                         rank = 1:49)
data3$state <- factor(data3$state, levels=stateOrder$state, ordered=TRUE)
data3$weight <- 1/data3$LandArea

statemap <- ggplot() + geom_polygon(data=states_map, aes(x=long, y=lat, group=group), fill="black", colour="grey") +
  geom_segment(data=data3, aes(x=startLong, y=startLat, xend=endLong, yend=endLat, size=trackWidth), colour="#55B1F7", alpha=.2) +
  geom_segment(data=data3, aes(x=startLong, y=startLat, xend=endLong, yend=endLat, size=trackWidth, alpha=f), colour="#55B1F7") +
  scale_size_continuous("Width (yd)", range=c(.5, 2)) + 
  scale_alpha_continuous("Strength (F or EF scale)", range=c(.3, 1)) + 
  ggtitle("Tornado Paths, 1950-2006")

## BUG! interactive version... geom_bar + stat_bin does not work!
USpolygons <- states_map
UStornadoes <- data3
tornado.bar <- list(map=ggplot()+
     geom_polygon(aes(x=long, y=lat, group=group),
                  data=USpolygons, fill="black", colour="grey") +
     geom_segment(aes(x=startLong, y=startLat, xend=endLong, yend=endLat,
                      showSelected=year),
                  colour="#55B1F7", data=UStornadoes),
     ts=ggplot()+
     geom_bar(aes(year, clickSelects=year),data=UStornadoes))
gg2animint(tornado.bar, "tornado-bar")

## OK: interactive version with lines instead of bars!
UStornadoCounts <-
  ddply(UStornadoes, .(state, year), summarize, count=length(state))
tornado.ts <-
  list(map=ggplot()+
       geom_polygon(aes(x=long, y=lat, group=group, clickSelects=state),
                    data=USpolygons, fill="black", colour="grey") +
       geom_segment(aes(x=startLong, y=startLat, xend=endLong, yend=endLat,
                        showSelected=year),
                    colour="#55B1F7", data=UStornadoes),
       ts=ggplot()+
       make_tallrect("year", UStornadoCounts)+
       geom_line(aes(year, count, clickSelects=state, group=state),
                 data=UStornadoCounts, alpha=3/5, size=4))
gg2animint(tornado.ts, "tornado")

## TODO: why doesn't this animation work?
tornado.anim <-
  list(map=ggplot()+
       geom_polygon(aes(x=long, y=lat, group=group, clickSelects=state),
                    data=USpolygons, fill="black", colour="grey") +
       geom_segment(aes(x=startLong, y=startLat, xend=endLong, yend=endLat,
                        showSelected=year),
                    colour="#55B1F7", data=UStornadoes),
       ts=ggplot()+
       make_tallrect("year", UStornadoCounts)+
       geom_line(aes(year, count, clickSelects=state, group=state),
                 data=UStornadoCounts, alpha=3/5, size=4),
       time=list(variable="year",ms=2000))
gg2animint(tornado.ts, "tornado-anim")

# Works with geom_freqpoly
timehist <- ggplot() + geom_freqpoly(data=data3, aes(x=year, group=f, fill=f, colour=f)) + xlab("Time") + ylab("Recorded Tornadoes") + ggtitle("Recorded Tornadoes over Time")

statehist <- ggplot() + geom_freqpoly(data=data3, aes(x=state, group=factor(f), fill=f, colour=f, weight=weight), stat="bin") + xlab("State") + ylab("Tornadoes per Square Mile") + ggtitle("Recorded Tornadoes by State") + coord_flip()

gg2animint(list(mapa=timehist))
gg2animint(list(mapa=statehist))
gg2animint(list(mapa=statemap))


timehist <- ggplot() + geom_histogram(data=data3, aes(x=year, group=f, fill=f, colour=f)) + xlab("Time") + ylab("Recorded Tornadoes") + ggtitle("Recorded Tornadoes over Time")

statehist <- ggplot() + geom_histogram(data=data3, aes(x=state, group=f, fill=f, colour=f, weight=weight), stat="bin") + xlab("State") + ylab("Tornadoes per Square Mile") + ggtitle("Recorded Tornadoes by State") + coord_flip()

gg2animint(list(mapa=timehist))
gg2animint(list(mapa=statehist))

gg2animint(list(statemap = statemap, timehist = statehist,
                width=list(statemap=1000, timehist=500),  
                height=list(statemap=420, timehist=420)))

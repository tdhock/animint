#' NOAA SVRGIS data (Severe Report Database + Geographic Information System)
#' http://www.spc.noaa.gov/gis/svrgis/
#' Data - http://www.spc.noaa.gov/wcm/#data
#' Location Codes - http://www.spc.noaa.gov/wcm/loccodes.html
#' State FIPS Codes - http://www.spc.noaa.gov/wcm/fips_usa.gif
#' County FIPS Codes - http://www.spc.noaa.gov/wcm/stnindex_all.txt
#' State/County Area and Population - http://quickfacts.census.gov/qfd/download/DataSet.txt
#' 
#' Image Inspiration -  http://www.kulfoto.com/pic/0001/0033/b/h4n5832833.jpg
library(maps)
library(plyr)
library(ggplot2)
library(animint)
data(UStornadoes)
stateOrder <- data.frame(state = unique(UStornadoes$state)[order(unique(UStornadoes$TornadoesSqMile), decreasing=T)], rank = 1:49) # order states by tornadoes per square mile
UStornadoes$state <- factor(UStornadoes$state, levels=stateOrder$state, ordered=TRUE)
UStornadoes$weight <- 1/UStornadoes$LandArea
# useful for stat_bin, etc. 

USpolygons <- map_data("state")
USpolygons$state = state.abb[match(USpolygons$region, tolower(state.name))]

statemap <- ggplot() + geom_polygon(data=USpolygons, aes(x=long, y=lat, group=group), fill="black", colour="grey") +
  geom_segment(data=UStornadoes, aes(x=startLong, y=startLat, xend=endLong, yend=endLat, size=trackWidth), colour="#55B1F7", alpha=.2) +
  geom_segment(data=UStornadoes, aes(x=startLong, y=startLat, xend=endLong, yend=endLat, size=trackWidth, alpha=f), colour="#55B1F7") +
  scale_size_continuous("Width (yd)", range=c(.5, 2)) + 
  scale_alpha_continuous("Strength (F or EF scale)", range=c(.3, 1)) + 
  ggtitle("Tornado Paths, 1950-2006")

## BUG! interactive version... geom_bar + stat_bin does not work!
USpolygons <- USpolygons
UStornadoes <- UStornadoes

## ERROR: geom_bar + stat_bin + clickSelects does not make sense! We
## should stop with an error!
tornado.bar <-
  list(map=ggplot()+
       geom_polygon(aes(x=long, y=lat, group=group),
                    data=USpolygons, fill="black", colour="grey") +
       geom_segment(aes(x=startLong, y=startLat, xend=endLong, yend=endLat,
                        showSelected=year),
                    colour="#55B1F7", data=UStornadoes),
       ts=ggplot()+
       geom_bar(aes(year, clickSelects=year),data=UStornadoes))
gg2animint(tornado.bar, "tornado-bar")

## OK: stat_summary + clickSelects ensures unique x values.
tornado.bar <-
  list(map=ggplot()+
       geom_polygon(aes(x=long, y=lat, group=group),
                    data=USpolygons, fill="black", colour="grey") +
       geom_segment(aes(x=startLong, y=startLat, xend=endLong, yend=endLat,
                        showSelected=year),
                    colour="#55B1F7", data=UStornadoes),
       ts=ggplot()+
       stat_summary(aes(year, year, clickSelects=year),
                    data=UStornadoes, fun.y=length, geom="bar"))
gg2animint(tornado.bar, "tornado-bar")

## Same plot, using make_bar abbreviation.
tornado.bar <-
  list(map=ggplot()+
       geom_polygon(aes(x=long, y=lat, group=group),
                    data=USpolygons, fill="black", colour="grey") +
       geom_segment(aes(x=startLong, y=startLat, xend=endLong, yend=endLat,
                        showSelected=year),
                    colour="#55B1F7", data=UStornadoes),
       ts=ggplot()+
       make_bar(UStornadoes, "year"))
gg2animint(tornado.bar, "tornado-bar")

UStornadoCounts <-
  ddply(UStornadoes, .(state, year), summarize, count=length(state))
## OK: select state to show that subset of bars!
tornado.ts.bar <-
  list(map=ggplot()+
       make_text(UStornadoCounts, -100, 50, "year", "Tornadoes in %d")+
       geom_polygon(aes(x=long, y=lat, group=group, clickSelects=state),
                    data=USpolygons, fill="black", colour="grey") +
       geom_segment(aes(x=startLong, y=startLat, xend=endLong, yend=endLat,
                        showSelected=year),
                    colour="#55B1F7", data=UStornadoes),
       ts=ggplot()+
       make_text(UStornadoes, 1980, 200, "state")+
       geom_bar(aes(year, count, clickSelects=year, showSelected=state),
                data=UStornadoCounts, stat="identity", position="identity"))
gg2animint(tornado.ts.bar, "tornado-ts-bar")
## OK: interactive version with lines instead of bars!
tornado.ts.line <-
  list(map=ggplot()+
       geom_polygon(aes(x=long, y=lat, group=group, clickSelects=state),
                    data=USpolygons, fill="black", colour="grey") +
       geom_segment(aes(x=startLong, y=startLat, xend=endLong, yend=endLat,
                        showSelected=year),
                    colour="#55B1F7", data=UStornadoes),
       ts=ggplot()+
       make_tallrect(UStornadoCounts, "year")+
       geom_line(aes(year, count, clickSelects=state, group=state),
                 data=UStornadoCounts, alpha=3/5, size=4))
gg2animint(tornado.ts.line, "tornado-ts-line")


tornado.anim <-
  list(map=ggplot()+
       geom_polygon(aes(x=long, y=lat, group=group, clickSelects=state),
                    data=USpolygons, fill="black", colour="grey") +
       geom_segment(aes(x=startLong, y=startLat, xend=endLong, yend=endLat,
                        showSelected=year),
                    colour="#55B1F7", data=UStornadoes),
       ts=ggplot()+
       make_tallrect(UStornadoCounts, "year")+
       geom_line(aes(year, count, clickSelects=state, group=state),
                 data=UStornadoCounts, alpha=3/5, size=4),
       time=list(variable="year",ms=2000))
gg2animint(tornado.anim, "tornado-anim")

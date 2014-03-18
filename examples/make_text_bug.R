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

UStornadoCounts <-
  ddply(UStornadoes, .(state, year), summarize, count=length(state))

## BUG: This one does not render the make_text in the second plot,
## why?
tornado.points.anim <-
  list(map=ggplot()+
       make_text(UStornadoes, -100, 50, "year",
                 "Tornado paths and endpoints in %d")+
       geom_segment(aes(x=startLong, y=startLat, xend=endLong, yend=endLat,
                        showSelected=year),
                    colour=seg.color, data=UStornadoes)+
       geom_point(aes(endLong, endLat, showSelected=year),
                    colour=seg.color,
                  data=UStornadoes)+
       geom_polygon(aes(x=long, y=lat, group=group, clickSelects=state),
                    data=USpolygons, fill="grey", colour="black", alpha=3/4)+
       theme(axis.line=element_blank(), axis.text=element_blank(), 
             axis.ticks=element_blank(), axis.title=element_blank()),
       bug=ggplot()+
       ggtitle("There should be state = XXX below")+
       make_text(UStornadoes, 1980, 200, "state")+
       geom_bar(aes(year, count, clickSelects=year, showSelected=state),
                data=UStornadoCounts, stat="identity", position="identity")+
       geom_text(aes(year, count + 5, label=count,
                     showSelected2=year, showSelected=state),
                data=UStornadoCounts, size=20))
gg2animint(tornado.points.anim, "tornado-points-anim")

## Works. I moved the make_text after geom_bar.
tornado.points.anim <-
  list(map=ggplot()+
       make_text(UStornadoes, -100, 50, "year",
                 "Tornado paths and endpoints in %d")+
       geom_segment(aes(x=startLong, y=startLat, xend=endLong, yend=endLat,
                        showSelected=year),
                    colour=seg.color, data=UStornadoes)+
       geom_point(aes(endLong, endLat, showSelected=year),
                    colour=seg.color,
                  data=UStornadoes)+
       geom_polygon(aes(x=long, y=lat, group=group, clickSelects=state),
                    data=USpolygons, fill="grey", colour="black", alpha=3/4)+
       theme(axis.line=element_blank(), axis.text=element_blank(), 
             axis.ticks=element_blank(), axis.title=element_blank()),
       bug=ggplot()+
       ggtitle("There should be state = XXX below")+
       geom_bar(aes(year, count, clickSelects=year, showSelected=state),
                data=UStornadoCounts, stat="identity", position="identity")+
       make_text(UStornadoes, 1980, 200, "state")+
       geom_text(aes(year, count + 5, label=count,
                     showSelected2=year, showSelected=state),
                data=UStornadoCounts, size=20))
gg2animint(tornado.points.anim, "tornado-points-anim")

## Works. I deleted the last geom_text.
tornado.points.anim <-
  list(map=ggplot()+
       make_text(UStornadoes, -100, 50, "year",
                 "Tornado paths and endpoints in %d")+
       geom_segment(aes(x=startLong, y=startLat, xend=endLong, yend=endLat,
                        showSelected=year),
                    colour=seg.color, data=UStornadoes)+
       geom_point(aes(endLong, endLat, showSelected=year),
                    colour=seg.color,
                  data=UStornadoes)+
       geom_polygon(aes(x=long, y=lat, group=group, clickSelects=state),
                    data=USpolygons, fill="grey", colour="black", alpha=3/4)+
       theme(axis.line=element_blank(), axis.text=element_blank(), 
             axis.ticks=element_blank(), axis.title=element_blank()),
       bug=ggplot()+
       ggtitle("There should be state = XXX below")+
       make_text(UStornadoes, 1980, 200, "state")+
       geom_bar(aes(year, count, clickSelects=year, showSelected=state),
                data=UStornadoCounts, stat="identity", position="identity"))
gg2animint(tornado.points.anim, "tornado-points-anim")


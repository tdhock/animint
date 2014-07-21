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

works_with_R("3.0.2",
             animint="2014.3.19",
             ggplot2="0.9.3.1",
             maps="2.3.6",
             lubridate="1.3.3",
             plyr="1.8.1")


data(climate)
climate$time2 <- decimal_date(ymd(as.character(climate$date)))

countries <- map_data("world")
# Map coordinate limits chosen so that the polygons displayed are at least reasonably complete. 
countries <- subset(countries, (lat < 38)&(lat>-24))
countries <- subset(countries, ((-long)>54)&((-long)<118))

# Create variable showing temp-avg.monthly.temp at that location
climate <- ddply(climate, .(id, month), transform, tempdev = temperature - mean(temperature), surfdev = surftemp - mean(surftemp))
climate <- climate[order(climate$date, climate$id), ]

# data frame with formatted labels
dates <- ddply(climate, .(date), summarise, month=month[1], year = year[1], time2 = time2[1], textdate = paste(month.name[month], year))
dates <- dates[order(dates$date),]

climate <- climate[order(climate$time2),]

long.names <- c(surftemp="Surface temperature",
                temperature="Temperature",
                surfdev="Deviation from monthly norm")
lims <- list(surftemp=c(-10, 40),
             surfdev=c(-8, 8))
var.names <- c("surftemp", "surfdev")
dot.alpha <- 6/10
viz <- list()
getlab <- function(var.name){
  sprintf("%s (degrees Celsius)", long.names[[var.name]])
}
for(var.name in var.names){
  long.name <- long.names[[var.name]]
  viz[[sprintf("%sMap", var.name)]] <- ggplot() + 
    geom_tile(aes_string(x="long", y="lat", fill=var.name,
                  clickSelects="id", showSelected="time2"),
              data=climate)+ 
    scale_fill_gradient2("deg. C", low="blue", mid="white", high="red",
                         midpoint=0, limits=lims[[var.name]]) + 
    ggtitle(long.name)+
    geom_path(aes(long, lat, group=group), col="grey",
              data=countries) + 
    geom_text(aes(-86, 39, label=textdate, showSelected=time2),
              data=dates)+ 
    theme(axis.line=element_blank(), axis.text=element_blank(), 
          axis.ticks=element_blank(), axis.title=element_blank())
}
selected.color <- "violet"
viz$scatterNow <- ggplot()+
  geom_text(aes(20, -7, label=sprintf("all regions in %s", textdate),
                showSelected=time2),
            data=dates)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  xlim(-10, 40)+
  ylim(-8, 8)+
  xlab(getlab(var.names[[1]]))+
  ylab(getlab(var.names[[2]]))+
  ## WANT to be able to specify:
  geom_point(aes_string(x=var.names[[1]], y=var.names[[2]],
                        clickSelects="id", showSelected="time2"),
             data=climate, alpha=dot.alpha,
             selected.color="black", selected.fill=selected.color)+
  ## OLD repetitive syntax:
  geom_point(aes_string(x=var.names[[1]], y=var.names[[2]],
                        clickSelects="id", showSelected="time2"),
             data=climate, alpha=dot.alpha)+
  geom_point(aes_string(x=var.names[[1]], y=var.names[[2]],
                        showSelected2="id", showSelected="time2"),
             data=climate, colour="black", fill=selected.color)
for(var.name in var.names){
  long.name <- long.names[[var.name]]
  viz[[sprintf("%sTimeSeries", var.name)]] <- ggplot()+
    ## make_text(climate, 1998, max(climate[[var.name]]),
    ##           "id", "region col-row = %s")+
    geom_hline(yintercept=0)+
    make_tallrect(climate, "time2") +
    xlab("Year of measurement")+
    ylab(getlab(var.name))+
    ## WANT to be able to specify:
    geom_line(aes_string(x="time2", y=var.name,
                         group="id", clickSelects="id"),
              data=climate, alpha=3/100,
              selected.color=selected.color)
    ## OLD repetitive syntax:
    geom_line(aes_string(x="time2", y=var.name,
                         group="id", clickSelects="id"),
              data=climate, alpha=1/2 + 3/100)
    ##+
    geom_line(aes_string(x="time2", y=var.name,
                         showSelected="id"),
              data=climate, colour=selected.color)
}
viz$scatterHere <- ggplot()+
  make_text(climate, 20, -7,
            "id", "all times for region %s")+
  xlab(getlab(var.names[[1]]))+
  ylab(getlab(var.names[[2]]))+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  xlim(-10, 40)+
  ylim(-8, 8)+
  ## WANT to be able to specify:
  geom_point(aes_string(x=var.names[[1]], y=var.names[[2]],
                        clickSelects="time2", showSelected="id"),
             data=climate, alpha=dot.alpha,
             selected.color="black", selected.fill="white")
  ## OLD repetitive syntax:
  geom_point(aes_string(x=var.names[[1]], y=var.names[[2]],
                        clickSelects="time2", showSelected="id"),
             data=climate, alpha=dot.alpha)
  ##+
  geom_point(aes_string(x=var.names[[1]], y=var.names[[2]],
                        showSelected2="time2", showSelected="id"),
             data=climate, color="black", fill="white")
viz$width <-
  structure(as.list(rep(400, length(viz))),
            names=names(viz))
viz$width[grep("TimeSeries", names(viz$width))] <- 463
## Non-animated version for screenshot.
viz$duration <- list(time2=3000)
gg2animint(viz, "climate")
## Animated version for the web.
viz.anim <- viz
viz.anim$time <- list(variable="time2", ms=5000)
gg2animint(viz.anim, "climate-anim")



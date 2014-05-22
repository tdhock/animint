library(animint)
data(WorldBank)
check <- function(p, w=""){
  stopifnot(is.character(w))
  stopifnot(length(w)==1)
  if(is.ggplot(p)){
    p <- list(plot=p)
  }
  stopifnot(is.list(p))
  list(plot=p, warn=w)
}
wb <- ggplot()+
  geom_point(aes(life.expectancy, fertility.rate, size=population),
             data=WorldBank)
motion.area <-
  list(scatter=ggplot()+
       geom_point(aes(life.expectancy, fertility.rate, clickSelects=country,
                      showSelected=year, colour=region, size=population),
                  data=WorldBank)+
       geom_text(aes(life.expectancy, fertility.rate, label=country,
                     showSelected=country, showSelected2=year),
                 data=WorldBank)+
       scale_size_continuous(range=c(3, 10))+
       make_text(WorldBank, 55, 9, "year"),
       ts=ggplot()+
       make_tallrect(WorldBank, "year")+
       geom_line(aes(year, life.expectancy, group=country, colour=region,
                     clickSelects=country),
                 data=WorldBank, size=4, alpha=3/5),
       time=list(variable="year",ms=3000),
       bar=ggplot()+
       geom_bar(aes(country, life.expectancy, fill=region,
                    showSelected=year, clickSelects=country),
                data=WorldBank, stat="identity", position="identity")+
       coord_flip() + theme_animint(height = 1500),
       duration=list(year=1000))
tornado.warn <- paste0("stat_bin is unpredictable ",
                       "when used with clickSelects/showSelected.\n",
                       "Use ddply to do the binning ",
                       "or use make_bar if using geom_bar/geom_histogram.")
library(maps)
data(UStornadoes)
stateOrder <- data.frame(state = unique(UStornadoes$state)[order(unique(UStornadoes$TornadoesSqMile), decreasing=T)], rank = 1:49) # order states by tornadoes per square mile
UStornadoes$state <- factor(UStornadoes$state, levels=stateOrder$state, ordered=TRUE)
UStornadoes$weight <- 1/UStornadoes$LandArea
# useful for stat_bin, etc. 

USpolygons <- map_data("state")
USpolygons$state = state.abb[match(USpolygons$region, tolower(state.name))]
tornado.bar <-
  list(map=ggplot()+
       geom_polygon(aes(x=long, y=lat, group=group),
                    data=USpolygons, fill="black", colour="grey") +
       geom_segment(aes(x=startLong, y=startLat, xend=endLong, yend=endLat,
                        showSelected=year),
                    colour="#55B1F7", data=UStornadoes),
       ts=ggplot()+
       geom_bar(aes(year, clickSelects=year),data=UStornadoes))
## check(x) means there should be no warning and check(x,"warning")
## means that the code should produce "warning"
to.check <-
  list(check(wb),
       check(wb+scale_size_area(),
             "geom_point with size=0 will be invisible"),
       check(motion.area),
       check(tornado.bar, tornado.warn))
for(L in to.check){
  generated <- ""
  tryCatch({
    gg2animint(L$plot, open.browser=FALSE)
  }, warning=function(w){
    generated <<- w$mes
  })
  if(generated != L$warn){
    print(L$warn)
    print(generated)
    stop("expected first, but got second")
  }
}

context("save separate chunks")
library(plyr)

### test case 1
# retrieve state-level data from the CDC's FluView Portal and save as FluView.RData
# under animint/data directory
# library(cdcfluview)
# state_flu <- get_state_data(2008:2014)
# # data clean
# state_flu <- state_flu[, !names(state_flu) %in% c("URL", "WEBSITE")]
# state_flu <- subset(state_flu, !STATENAME %in% c("District of Columbia", 
#                                                  "New York City", "Puerto Rico", 
#                                                  "Alaska", "Hawaii"))
# state_flu$state <- tolower(state_flu$STATENAME)
# state_flu$level <- as.numeric(gsub("Level ", "", state_flu$ACTIVITY.LEVEL))
# state_flu$WEEKEND <- as.Date(state_flu$WEEKEND, format = "%b-%d-%Y")
# library(ggplot2)
# USpolygons <- map_data("state")
# USpolygons$subregion <- NULL
# USpolygons <- subset(USpolygons, region != "district of columbia")
# # add state flu
# map_flu <- ldply(unique(state_flu$WEEKEND), function(we) {
#   df <- subset(state_flu, WEEKEND == we)
#   merge(USpolygons, df, by.x = "region", by.y = "state")
# })
# save(state_flu, USpolygons, map_flu, 
#      file = "path/to/animint/data/FluView.RData", compress = "xz")

data(FluView)
# use 2008-09 and 2009-10 seasons to test
state_flu <- subset(state_flu, SEASON %in% c("2008-09", "2009-10"))
map_flu <- subset(map_flu, SEASON %in% c("2008-09", "2009-10"))

# visualize CDC FluView data
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

p <- ggplot() + 
  make_text(map_flu, -100, 50, "WEEKEND", "CDC FluView in Lower 48 States ending %s") + 
  scale_fill_gradient2(low = "white", high = "red", breaks = 0:10, guide = "none") + 
  theme_opts + 
  theme_animint(width = 750, height= 500)

if (Sys.getenv("TRAVIS") == "true" | Sys.getenv("WERCKER") == "true") {
  message("tests currently don't work on travis (but should someday)")
} else {
  test_that("save separate chunks for geom_polygon", {
    state.map <- p + 
      geom_polygon(data = map_flu, aes(x = long, y = lat, group = group, fill = level, 
                                       showSelected = WEEKEND), 
                   colour = "black", size = 1)
    viz <- list(levelHeatmap = level.heatmap, stateMap = state.map, title = "FluView")
    out.dir <- file.path(getwd(), "FluView")
    animint2dir(viz, out.dir = out.dir, open.browser = FALSE)
    
    common.chunk <- list.files(path = out.dir, pattern = "geom.+polygon.+chunk_common.tsv", 
                               full.names = TRUE)
    varied.chunks <- list.files(path = out.dir, pattern = "geom.+polygon.+chunk[0-9]+.tsv", 
                                full.names = TRUE)
    # number of chunks
    expect_equal(length(common.chunk), 1L)
    no.chunks <- length(varied.chunks)
    expect_equal(no.chunks, length(unique(map_flu$WEEKEND)))
    # test common.chunk
    common.data <- read.csv(common.chunk, sep = "\t")
    expect_equal(nrow(common.data), nrow(USpolygons))
    expect_true(all(c("x", "y", "group") %in% names(common.data)))
    # randomly choose an varied.chunk to test
    idx <- sample(no.chunks, 1)
    varied.data <- read.csv(varied.chunks[idx], sep = "\t")
    expect_equal(nrow(varied.data), length(unique(USpolygons$group)))
    expect_true(all(c("fill", "group") %in% names(varied.data)))
    
    unlink(out.dir, recursive = TRUE)
  })
}


### test case 2
USdots <- ddply(USpolygons, .(region), summarise, mean.lat = mean(lat), 
                mean.long = mean(long))
# add state flu
map_flu <- ldply(unique(state_flu$WEEKEND), function(we) {
  df <- subset(state_flu, WEEKEND == we)
  merge(USdots, df, by.x = "region", by.y = "state")
})

test_that("save separate chunks for geom_point without specifying group", {
  # the compiler will not break a geom into chunks if any of the resulting 
  # chunk tsv files is estimated to be less than 4KB.
  state.map <- p + 
    geom_point(data = map_flu, aes(x = mean.long, y = mean.lat, fill = level, 
                                   showSelected = WEEKEND), color = "black", size = 10)
  viz <- list(levelHeatmap = level.heatmap, stateMap = state.map, title = "FluView")
  out.dir <- file.path(getwd(), "FluView-point")
  animint2dir(viz, out.dir = out.dir, open.browser = FALSE)
  
  common.chunk <- list.files(path = out.dir, pattern = "geom.+point.+chunk_common.tsv", 
                             full.names = TRUE)
  varied.chunks <- list.files(path = out.dir, pattern = "geom.+point.+chunk[0-9]+.tsv", 
                              full.names = TRUE)
  # number of chunks
  expect_equal(length(common.chunk), 0L)
  expect_equal(length(varied.chunks), 1L)
  # test the only one varied.chunk
  varied.data <- read.csv(varied.chunks, sep = "\t")
  expect_equal(nrow(varied.data), nrow(map_flu))
  expect_true(all(c("fill", "x", "y", "showSelected") %in% names(varied.data)))
  
  unlink(out.dir, recursive = TRUE)
  
  ## force to split into chunks
  state.map <- p + 
    geom_point(data = map_flu, aes(x = mean.long, y = mean.lat, fill = level, 
                                   showSelected = WEEKEND), 
               color = "black", size = 10, chunk_vars = "WEEKEND")
  viz <- list(levelHeatmap = level.heatmap, stateMap = state.map, title = "FluView")
  animint2dir(viz, out.dir = out.dir, open.browser = FALSE)
  
  common.chunk <- list.files(path = out.dir, pattern = "geom.+point.+chunk_common.tsv", 
                             full.names = TRUE)
  varied.chunks <- list.files(path = out.dir, pattern = "geom.+point.+chunk[0-9]+.tsv", 
                              full.names = TRUE)
  # number of chunks
  expect_equal(length(common.chunk), 1L)
  no.chunks <- length(varied.chunks)
  expect_equal(no.chunks, length(unique(map_flu$WEEKEND)))
  # test common.chunk
  common.data <- read.csv(common.chunk, sep = "\t")
  expect_equal(nrow(common.data), nrow(USdots))
  expect_true(all(c("x", "y", "group") %in% names(common.data)))
  # randomly choose an varied.chunk to test
  idx <- sample(no.chunks, 1)
  varied.data <- read.csv(varied.chunks[idx], sep = "\t")
  expect_equal(nrow(varied.data), nrow(USdots))
  expect_true(all(c("fill", "group") %in% names(varied.data)))
    
  unlink(out.dir, recursive = TRUE)
})


### test case 3
data(WorldBank)

scatter=ggplot()+
  geom_point(aes(life.expectancy, fertility.rate, clickSelects=country,
                 showSelected=year, colour=region, size=population,
                 tooltip=paste(country, "population", population),
                 key=country), # key aesthetic for animated transitions!
             data=WorldBank)+
  geom_text(aes(life.expectancy, fertility.rate, label=country,
                showSelected=country, showSelected2=year,
                key=country), # also use key here!
            data=WorldBank, chunk_vars=c("year", "country"))+
  scale_size_animint(breaks=10^(5:9))+
  make_text(WorldBank, 55, 9, "year")

ts=ggplot()+
  make_tallrect(WorldBank, "year")+
  geom_line(aes(year, life.expectancy, group=country, colour=region,
                clickSelects=country),
            data=WorldBank, size=4, alpha=3/5)

test_that("save separate chunks for non-spatial geoms with repetitive field, multiple vars selected, and NAs", {
  viz <- list(scatter = scatter, ts = ts, time=list(variable="year", ms=3000),
         duration=list(year=1000), first=list(year=1975, country="United States"),
         title="World Bank data (multiple selections)")
  out.dir <- file.path(getwd(), "WorldBank-all")
  animint2dir(viz, out.dir = out.dir, open.browser = FALSE)
  
  ## multiple vars selected
  common.chunk <- list.files(path = out.dir, pattern = "geom2_text.+chunk_common.tsv", 
                             full.names = TRUE)
  varied.chunks <- list.files(path = out.dir, pattern = "geom2_text.+chunk[0-9]+.tsv", 
                              full.names = TRUE)
  # number of chunks
  expect_equal(length(common.chunk), 0L)
  no.chunks <- length(varied.chunks)
  expect_equal(no.chunks, 9852)
  # choose first varied.chunk to test
  varied.data <- read.csv(varied.chunks[1], sep = "\t")
  expect_equal(nrow(varied.data), 1)
  expect_true(all(c("x", "y", "label", "key") %in% names(varied.data)))
  
  ## single var selected
  common.chunk <- list.files(path = out.dir, pattern = "geom.+point.+chunk_common.tsv", 
                             full.names = TRUE)
  varied.chunks <- list.files(path = out.dir, pattern = "geom.+point.+chunk[0-9]+.tsv", 
                              full.names = TRUE)
  # number of chunks
  expect_equal(length(common.chunk), 1L)
  no.chunks <- length(varied.chunks)
  expect_equal(no.chunks, 52)
  # test common.chunk
  common.data <- read.csv(common.chunk, sep = "\t")
  expect_equal(nrow(common.data), 214)
  expect_true(all(c("colour", "clickSelects", "key", "fill", "group") %in% names(common.data)))
  # choose first varied.chunk to test
  varied.data <- read.csv(varied.chunks[1], sep = "\t")
  if (Sys.getenv("TRAVIS") == "true" | Sys.getenv("WERCKER") == "true") {
    message("tests currently don't work on travis (but should someday)")
  } else {
    expect_equal(nrow(varied.data), 186)
  }
  expect_true(all(c("size", "x",	"y",	"tooltip", "showSelectedlegendcolour", "group") %in% names(varied.data)))
  
  unlink(out.dir, recursive = TRUE)
})


### test case 4
data(breakpoints)

only.error <- subset(breakpoints$error, type=="E")
only.segments <- subset(only.error, samples==samples[1])
signal.colors <- c(estimate="#0adb0a", latent="#0098ef")

signal=ggplot()+
  geom_point(aes(position, signal, showSelected=samples),
             data=breakpoints$signals)+
  geom_line(aes(position, signal), colour=signal.colors[["latent"]],
            data=breakpoints$imprecision)+
  geom_segment(aes(first.base, mean, xend=last.base, yend=mean,
                   showSelected=segments,
                   showSelected2=samples),
               colour=signal.colors[["estimate"]],
               data=breakpoints$segments)+
  geom_vline(aes(xintercept=base,
                 showSelected=segments,
                 showSelected2=samples),
             colour=signal.colors[["estimate"]],
             linetype="dashed",
             data=breakpoints$breaks)

test_that("save separate chunks for non-spatial geoms with nest_order not being group", {
  viz <- list(signal = signal, title="breakpointError (select one model size)")
  out.dir <- file.path(getwd(), "breakpointError-single")
  animint2dir(viz, out.dir = out.dir, open.browser = FALSE)
  
  common.chunk <- list.files(path = out.dir, pattern = "geom.+segment.+chunk_common.tsv", 
                             full.names = TRUE)
  varied.chunks <- list.files(path = out.dir, pattern = "geom.+segment.+chunk[0-9]+.tsv", 
                              full.names = TRUE)
  # number of chunks
  expect_equal(length(common.chunk), 0L)
  no.chunks <- length(varied.chunks)
  expect_equal(no.chunks, length(unique(breakpoints$segments$samples)))
  # randomly choose an varied.chunk to test
  idx <- sample(no.chunks, 1)
  varied.data <- read.csv(varied.chunks[idx], sep = "\t")
  expect_equal(nrow(varied.data), nrow(breakpoints$segments) / 
                 length(unique(breakpoints$segments$samples)))
  expect_true(all(c("x", "xend", "y", "yend", "showSelected") %in% names(varied.data)))
  
  unlink(out.dir, recursive = TRUE)
})

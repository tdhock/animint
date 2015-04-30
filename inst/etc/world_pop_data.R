works_with_R("3.0.1",rjson="0.2.12")

## Downloaded from and then added quotation marks.
## https://github.com/Polychart/polychart2/blob/master/example/population.coffee

jslines <- readLines("world_pop_data.json")
oneline <- paste(jslines,collapse="\n")
l <- fromJSON(oneline)
worldPop <- data.frame(l)
for(N in c("year","population")){
  worldPop[[N]] <- as.integer(as.character(worldPop[[N]]))
}
worldPop$type <- factor(ifelse(worldPop$year < 2012, "actual", "estimate"))

save(worldPop,file="../data/worldPop.RData")
prompt(worldPop,file="../man/worldPop.Rd")

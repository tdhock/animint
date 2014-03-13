library(animint)
## Make a Gapminder plot (aka Google motion chart), which is actually
## just a scatterplot with size and color that moves over time.
data(WorldBank)
breaks <- 10^(4:9)
wbViz <-
  list(ts=ggplot()+
       make_tallrect(WorldBank, "year")+
       geom_line(aes(year, life.expectancy, group=country, colour=region,
                     clickSelects=country),
                 data=WorldBank, size=3, alpha=3/5),
       time=list(variable="year",ms=3000),
       duration=list(year=1000),
       scatter=ggplot()+
       geom_point(aes(fertility.rate, life.expectancy, clickSelects=country,
                      showSelected=year, colour=region, size=population),
                  data=WorldBank)+
       geom_text(aes(fertility.rate, life.expectancy, label=country,
                     showSelected=country, showSelected2=year),
                 data=WorldBank)+
       make_text(WorldBank, 5, 80, "year")+
       continuous_scale("size","area",palette=function(x){
         scales:::rescale(sqrt(abs(x)), c(2,20), c(0,1))
       },breaks=breaks))
info <- gg2animint(wbViz, open.browser=FALSE)
entries <- info$plots$scatter$legend$population$entries
stopifnot(length(entries) == length(breaks))
label.chr <- sapply(entries, "[[", "label")
label.num <- as.numeric(label.chr)
stopifnot(all.equal(sort(breaks), sort(label.num)))

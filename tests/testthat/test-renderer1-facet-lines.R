acontext("facet lines")

data(WorldBank)
not.na <- subset(WorldBank, !(is.na(life.expectancy) | is.na(fertility.rate)))
TS <- function(df)data.frame(df, facet="Years")
SCATTER <- function(df)data.frame(df, facet="Fertility rate")
years <- unique(not.na[, "year", drop=FALSE])
countries <- unique(not.na[, "country", drop=FALSE])
wb.facets <-
  list(ts=ggplot()+
         xlab("")+
         geom_tallrect(aes(xmin=year-1/2, xmax=year+1/2,
                           clickSelects=year),
                       data=TS(years), alpha=1/2)+
         theme_animint(width=1000)+
         geom_line(aes(year, life.expectancy, group=country, colour=region,
                       clickSelects=country),
                   data=TS(not.na), size=4, alpha=3/5)+
         geom_point(aes(year, life.expectancy, color=region, size=population,
                        showSelected=country, clickSelects=country),
                    data=TS(not.na))+
         geom_point(aes(fertility.rate, life.expectancy, clickSelects=country,
                        showSelected=year, colour=region, size=population,
                        key=country), # key aesthetic for animated transitions!
                    data=SCATTER(not.na))+
         geom_text(aes(fertility.rate, life.expectancy, label=country,
                       showSelected=country, showSelected2=year,
                       clickSelects=country,
                       key=country), #also use key here!
                   data=SCATTER(not.na))+
         scale_size_animint(breaks=10^(5:9))+
         facet_grid(.~facet, scales="free")+
         geom_text(aes(5, 85, label=paste0("year = ", year),
                       showSelected=year),
                   data=SCATTER(years)),
       time=list(variable="year",ms=3000),
       bar=ggplot()+
         theme_animint(height=2400)+
         geom_bar(aes(country, life.expectancy, fill=region,
                      showSelected=year, clickSelects=country,
                      key=country),
                  data=not.na, stat="identity", position="identity")+
         coord_flip(),
       duration=list(year=1000),
       first=list(year=1975, country=c("United States", "Vietnam")),
       selector.types=list(country="multiple"),
       title="World Bank data (multiple selection, facets)")

info <- animint2HTML(wb.facets)

test_that("if group is in nest_order, it is last", {
  for(g in info$geoms){
    nest <- as.character(g$nest_order)
    if("group" %in% nest){
      expect_match(nest[length(nest)], "group")
    }
  }
})

test_that("a <path> is rendered for every country", {
  node.set <- getNodeSet(info$html, '//g[@class="geom2_line_ts"]//path')
  expect_equal(length(node.set), nrow(countries))
})

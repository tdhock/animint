acontext("make_xrect")

data(WorldBank, package = "animint")
not.na <- subset(WorldBank, !(is.na(life.expectancy) | is.na(fertility.rate)))
BOTH <- function(df, top, side){
  data.frame(df,
             top=factor(top, c("Fertility rate", "Years")),
             side=factor(side, c("Years", "Life expectancy")))
}
TS <- function(df)BOTH(df, "Years", "Life expectancy")
SCATTER <- function(df)BOTH(df, "Fertility rate", "Life expectancy")
TS2 <- function(df)BOTH(df, "Fertility rate", "Years")
viz.data.fun <- list(
  title="make_xrect with data.fun",
  ts=ggplot()+
    xlab("")+
    make_tallrect(not.na, "year", data.fun=TS)+
    theme_bw()+
    theme_animint(width=1000, height=800)+
    theme(panel.margin=grid::unit(0, "lines"))+
    geom_line(aes(year, life.expectancy, group=country, colour=region,
                  clickSelects=country, id = country),
              data=TS(not.na), size=4, alpha=3/5)+
    geom_point(aes(year, life.expectancy, color=region, size=population,
                   showSelected=country, clickSelects=country),
               data=TS(not.na))+
    geom_path(aes(fertility.rate, year, group=country, colour=region,
                  clickSelects=country),
              data=TS2(not.na), size=4, alpha=3/5)+
    geom_point(aes(fertility.rate, year, color=region, size=population,
                   showSelected=country, clickSelects=country),
               data=TS2(not.na))+
    make_widerect(not.na, "year", data.fun=TS2)+
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
    facet_grid(side ~ top, scales="free"),
  duration=list(year=2000),
  first=list(year=1975, country=c("United States", "Vietnam")),
  selector.types=list(country="multiple"))
info <- animint2HTML(viz.data.fun)

n.years <- length(unique(not.na$year))

test_that("correct number of widerects rendered", {
  rect.list <- getNodeSet(info$html, '//g[@class="geom6_widerect_ts"]//rect')
  expect_equal(length(rect.list), n.years)
})

test_that("correct number of tallrects rendered", {
  rect.list <- getNodeSet(info$html, '//g[@class="geom1_tallrect_ts"]//rect')
  expect_equal(length(rect.list), n.years)
})


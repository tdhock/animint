fake <- data.frame(year=c(1900, 1950, 2000, 2010),
                   y=1,
                   period=c(1, 1, 2, 2))
viz <-
  list(free=ggplot()+
       theme_bw()+
       theme(panel.margin=grid::unit(0, "cm"))+
       geom_point(aes(year, y), data=fake)+
       scale_x_continuous(breaks=seq(1900, 2010, by=10))+
       facet_grid(.~period, scales="free", space="free"))
viz$fixed <- viz$free+facet_grid(.~period, scales="free")
viz$free_y <- viz$free+facet_grid(.~period, scales="free", space="free_y")
viz$free_x <- viz$free+facet_grid(.~period, scales="free", space="free_x")
viz$no.panels <- viz$free+facet_null()
panels <- list()
facets <- list()
space_free <- list()
for(plot.name in names(viz)){
  gg <- viz[[plot.name]]
  built <- ggplot2::ggplot_build(gg)
  panels[[plot.name]] <- built$panel
  facets[[plot.name]] <- gg$facet
  space_free[[plot.name]] <- gg$facet$space_free
}
str(facets)
## the difference is in space_free:
str(space_free)

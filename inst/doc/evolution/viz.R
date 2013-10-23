data(generation.loci)
colormap <- c(blue="blue",red="red",ancestral="black",neutral="grey30")
ancestral <- subset(generation.loci,population==1 & generation==1)
ancestral$color <- "ancestral"
viz <- 
  list(ts=ggplot()+
       geom_line(aes(generation, frequency, group=population,
                     colour=color, showSelected=locus),
                 data=generation.loci)+
       make_text(generation.loci, 50, 1.05, "locus")+
       make_tallrect(generation.loci, "generation")+
       scale_colour_manual("population", values=colormap)+
       geom_point(aes(generation, frequency, showSelected=locus),
                  data=ancestral),
       loci=ggplot()+
       geom_point(aes(locus, frequency, colour=color,
                      showSelected=generation),
                  data=generation.loci, pch=21)+
       geom_point(aes(locus, frequency, colour=color),
                  data=ancestral, pch=21)+
       scale_colour_manual("population", values=colormap)+
       make_tallrect(generation.loci, "locus")+
       make_text(generation.loci, 35, 1, "generation"),
       duration=list(generation=1000),
       time=list(variable="generation", ms=2000))

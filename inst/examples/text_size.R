library(animint)
data(WorldBank)

wb  <- WorldBank[WorldBank$year == 2010,]

p <- ggplot()+
  geom_text(aes(y=fertility.rate, x=life.expectancy,
                label=iso2c, size=population, colour=population),
            data=wb)+
  scale_size_continuous(range=c(10,20))
print(p)
gg2animint(list(plot1=p), "text_size")


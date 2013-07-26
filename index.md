Animint Tutorial
========================================================

This tutorial is designed to demonstrate animint, a package that converts ggplot2 plots into d3 javascript graphics. Animint allows you to make interactive web-based graphics using familiar R methods. In addition, animint allows graphics to be animated and respond to user clicks.

Let's start with a reasonably common comparison of distributions: two normal distributions with different centers, and a gamma distribution. 


```r
library(ggplot2)
library(animint)
```

```
## Loading required package: RJSONIO
```

```
## Loading required package: proto
```

```
## Loading required package: grid
```

```r
library(plyr)

boxplotdata <- rbind(data.frame(x = 1:50, y = sort(rnorm(50, 3, 1)), group = "N(3,1)"), 
    data.frame(x = 1:50, y = sort(rnorm(50, 0, 1)), group = "N(0,1)"), data.frame(x = 1:50, 
        y = sort(rgamma(50, 2, 1/3)), group = "Gamma(2,1/3)"))
boxplotdata <- ddply(boxplotdata, .(group), transform, ymax = max(y), ymin = min(y), 
    med = median(y))

g1 <- ggplot() + geom_density(data = boxplotdata, aes(x = y, group = group, 
    fill = group), alpha = 0.5) + scale_fill_discrete("Distribution") + xlab("x") + 
    ggtitle("geom_density")
g1
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 

```r
gg2animint(list(plot1 = g1), out.dir = "./g1")
```


You can see the resulting d3 plot [here](g1/index.html).

Animint requires a list of named plots - each plot must be a ggplot2 object and must have a name, such as "plot1" in the example above. You can also specify the output directory - this allows you to control where the generated webpage is stored. If the output directory is not specified, then typically R will create a temporary directory on your computer to store the generated webpage. 

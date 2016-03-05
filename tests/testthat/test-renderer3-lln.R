acontext("ChromHMMinit data set")

#The function which is used to simulate the law of large numbers
law_large_numbers <- function(FUN = rnorm,mu = 0,np = 30,n = 100)
{
  
  #creating the data needed for drawing ploygons and points for showing the animation
  polygon_data = points_data = NULL
  for (i in 1:n) {
    d = colMeans(matrix(replicate(np, FUN(i, mu)), i))
    points_data = c(points_data, d)
    polygon_data = rbind(polygon_data, range(d))
  }
  
  #modifying the data in auch a way that can be used by ggplot and animint to show the animation
  polygon_data_frame=point_data_frame=NULL
  for (i in 1:n) {
    a_data <- c(1:i, i:1)
    b_data <- c(polygon_data[1:i, 1], polygon_data[i:1, 2])
    temp1 <-  data.frame(x=a_data,y=b_data,index=replicate(2*i,i))
    #creating the dataframe by combining the values from previous iteration and indexing them.
    polygon_data_frame <- rbind(polygon_data_frame,temp1)
    
    dm <- rep(1:i, each = np)
    dn <- points_data[1:(i * np)]
    temp2 <-  data.frame(x=dm,y=dn,index=replicate(np*i,i))
    point_data_frame <- rbind(point_data_frame,temp2)
  }
  
  list(polygon_data_frame,point_data_frame)
  
}

#calling the function 
x <- law_large_numbers()
data_frame <- as.data.frame(x[1])
point_data_frame=as.data.frame(x[2])

col.poly = 'bisque'
col.mu = 'gray'
mu=0
#using the ggplot syntax to plot the data
p <- ggplot(data_frame,aes(x=x,y=y,showSelected=index))+
  geom_polygon(color=col.poly)+
  geom_point(data=point_data_frame,aes(x=x,y=y,showSelected=index),color=col.mu,shape=20)+
  geom_abline(intercept=mu,slope=0)+
  labs(x="n",y="Mean of X")+
  theme_bw()+
  ggtitle("Law of Large numbers")
  



#calling animint for interactive animations
viz <- list(ani=p,time = list(variable = "index", ms =200))



info <- animint2HTML(viz)
tsv.file <- file.path("animint-htmltest", "geom3_abline_ani_chunk1.tsv")
tsv.data <- read.table(tsv.file, header=TRUE)

#Testing about abline properties

test_that("columns of abline tsv", {
  expected.names <- sort(c( "x", "xend", "y", "yend"))
  computed.names <- sort(names(tsv.data))
  expect_identical(computed.names, expected.names)
})

ablines <- getNodeSet(info$html, '//svg//g[@class="geom3_abline_ani"]//line')
attr_ablines <- sapply(ablines, xmlAttrs)
start_ends <- attr_ablines[c("x1", "x2", "y1", "y2"), ]

test_that("ablines render", {
  expect_equal(length(ablines), 1)
})

test_that("Start and end of ablines are not NA", {
  expect_true(all(start_ends != "NaN"))
})

test_that("lines do not exceed ranges of plot", {
  expect_true(all(as.numeric(start_ends) >= 0))
})

#Testing about polygons

polygons <- getNodeSet(info$html, '//svg//g[@class="geom1_polygon_ani"]//path')
attr_polygons <- sapply(polygons, xmlAttrs)
polygon_paths <- attr_polygons[c("d"), ]


test_that("polygons render", {
  expect_equal(length(polygons), 1)
})

test_that("Start and end of polygons are not NA", {
  expect_true(all(polygon_paths != "NaN"))
})

#Testing about points

points <- getNodeSet(info$html, '//svg//g[@class="geom2_point_ani"]//circle')
attr_points <- sapply(points, xmlAttrs)
points_paths <- attr_points[c("cx","cy","r"), ]

test_that("points render", {
  expect_equal(length(points), 60)
})

test_that("Start and end of points are not NA", {
  expect_true(all(points_paths != "NaN"))
})

#Testing the labels of the plot

test_that("ggtitle is rendered", {
  expect_identical(info$plots$ani$title, "Law of Large numbers")
  ptitle <- getNodeSet(info$html, "//text[@class='plottitle']")
  expect_identical(xmlValue(ptitle[[1]]), "Law of Large numbers")
})

test_that("ylab renders", {
  expect_identical(info$plots$ani$ytitle, "Mean of X")
  ylabel <- getNodeSet(info$html, "//text[@class='ytitle']")
  expect_identical(xmlValue(ylabel[[1]]), "Mean of X")
})

test_that("xlab renders", {
  expect_identical(info$plots$ani$xtitle, "n")
  ylabel <- getNodeSet(info$html, "//text[@class='xtitle']")
  expect_identical(xmlValue(ylabel[[1]]), "n")
})















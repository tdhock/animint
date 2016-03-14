## Load required packages
library(testthat)
library(ggplot2)
library(proto)
library(animint)
library(RSelenium)


## Helper Functions
animint2HTML <- function(plotList) {
    res1 <- animint2dir(plotList, out.dir = "animint-htmltest",
                       open.browser = FALSE)
    remDr$refresh()
    Sys.sleep(1)
    res1$html <- getHTML()
    res1
}

acontext("lilac chaser vi")

## Function to implement the vi.lilac.chaser() function from package 'animation'
vi_lilac_chaser <- function(np = 10,
                            nmax = 1,
                            col = 'magenta',
                            p.size = 20,
                            c.size = 4
)
{
    x <- seq(0, 2 * pi * np/(np + 1), length = np)  # Get co-ordinates to plot

    # Get data in a data-frame to pass to ggplot
    df <- data.frame()
    for (i in 1:np) {
        df <- rbind(df, cbind(sin(x[-i]), cos(x[-i]), ptn = i))}
    colnames(df) <- c("sinv", "cosv", "ptn")


    # Plot to display the points and the '+' mark in the middle
    p1 <- ggplot(data = df) +
        # Display the points
        geom_point(data = df,
                   aes(x = sinv, y = cosv, showSelected = ptn),
                   col = col,
                   size = p.size) +
        # Display the '+' mark
        geom_segment(aes(x=-0.1, y=0, xend=0.1, yend=0), size=c.size) +
        geom_segment(aes(x=0, y=-0.1, xend=0, yend=0.1), size=c.size) +
        xlim(c(-1.33, 1.33)) +
        ylim(c(-1.33, 1.33)) +
        # Hide the axes, titles and others..
        theme_bw() +
        theme(axis.line=element_blank(),
              axis.text.x=element_blank(), axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(), axis.title.y=element_blank(),
              legend.position="none",
              panel.background=element_blank(),panel.border=element_blank(),
              panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
              plot.background=element_blank())


    # Automate using animint taking point number 'ptn' as variable
    plots <- list(plot1 = p1)
    plots$time <- list(variable = "ptn", ms = 150)
    plots$duration <- list(ptn=0)
    return(plots)
}

plots <- vi_lilac_chaser()
info <- animint2HTML(plots)

test_that("axes hidden", {
    # info <- animint2HTML(viz)
    ec <- function(element, class){
        data.frame(element, class)
    }
    elem.df <- rbind(
        ec("rect", paste0(c("background","border"), "_rect")),
        ec("g", "axis"),
        ec("path", "domain"),
        ec("text", paste0(c("x", "y"), "title")))
    for(elem.i in seq_along(elem.df$element)){
        xpath <- with(elem.df[elem.i, ], {
            sprintf('//%s[@class="%s"]', element, class)
        })
        element.list <- getNodeSet(info$html, xpath)
        expect_equal(length(element.list), 0)
    }
})

test_that("x and y have no labels", {
    xlabel <- getNodeSet(info$html, "//text[@class='xtitle']")
    ylabel <- getNodeSet(info$html, "//text[@class='ytitle']")
    expect_false(identical(length(c(xlabel, ylabel)), c(0, 0)))
})

test_that("Different points are rendered", {
    x1_pts <- getNodeSet(info$html, "//circle[@class='geom']/@cx")
    y1_pts <- getNodeSet(info$html, "//circle[@class='geom']/@cy")
    x1_pts <- sapply(x1_pts, xmlNode)
    y1_pts <- sapply(y1_pts, xmlNode)

    Sys.sleep(1.739)  # Wait an arbitrary amount to get point locations

    info$html <- getHTML()

    x2_pts <- getNodeSet(info$html, "//circle[@class='geom']/@cx")
    y2_pts <- getNodeSet(info$html, "//circle[@class='geom']/@cy")
    x2_pts <- sapply(x2_pts, xmlNode)
    y2_pts <- sapply(y2_pts, xmlNode)
    expect_false(identical(x1_pts, x2_pts))
    expect_false(identical(y1_pts, y2_pts))
})

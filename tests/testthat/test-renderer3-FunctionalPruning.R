context("FunctionalPruning")

data(FunctionalPruning, package="animint")
min.bug.viz <- list(
  pruning=ggplot()+
    geom_line(aes(mean, cost,
                  showSelected=minimization),
              color="grey",
              size=5,
              data=FunctionalPruning$envelope)+
    geom_line(aes(mean, cost, color=data.i.fac,
                  group=paste(piece.i, data.i),
                  showSelected=minimization),
              data=FunctionalPruning$cost.lines)+
    geom_point(aes(min.cost.mean, min.cost,
                   color=data.i.fac,
                   showSelected=minimization),
               size=5,
               data=FunctionalPruning$minima),
  data=ggplot()+
    geom_tile(aes(timestep, total.segments,
                  fill=optimal.cost,
                  id=paste0("segs", total.segments, "time", timestep),
                  clickSelects=minimization),
              data=FunctionalPruning$grid),
  first=list(minimization="2 segments up to data point 4")
)
info <- animint2HTML(min.bug.viz)

some.lines <- subset(FunctionalPruning$cost.lines, timestep==4 & n.segments==2)
with(some.lines, table(data.i, piece.i))
test_that("one line rendered for min envelope", {
  path.list <- getNodeSet(
    info$html, 
    '//g[@class="geom1_line_pruning"]//path')
  expect_equal(length(path.list), 1)
})

test_that("four lines rendered for cost candidates", {
  path.list <- getNodeSet(
    info$html, 
    '//g[@class="geom2_line_pruning"]//path')
  expect_equal(length(path.list), 4)
})

html <- clickHTML(id="segs1time4")

test_that("min envelope line disappears", {
  path.list <- getNodeSet(
    info$html, 
    '//g[@class="geom1_line_pruning"]//path')
  expect_equal(length(path.list), 0)
})

test_that("cost candidates lines disappear", {
  path.list <- getNodeSet(
    info$html, 
    '//g[@class="geom2_line_pruning"]//path')
  expect_equal(length(path.list), 0)
})


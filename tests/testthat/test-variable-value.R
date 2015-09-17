context("variable value")

test_that("selector.aes errors when no matching variable for value", {
  a.list <-
    list(c("clickSelects.variable", "clickSelects2.variable",
           "clickSelects2.value"),
         c("clickSelects.variable", "clickSelects2.variable",
           "clickSelects.value"),
         c("showSelected.variable", "showSelected2.variable",
           "showSelected2.value"),
         c("showSelected.variable", "showSelected2.variable",
           "showSelected.value"),
         c("clickSelects.variable", "showSelected2.variable",
           "clickSelects.value"),
         "showSelected.variable",
         "showSelected2.variable",
         "clickSelects.variable",
         "clickSelects2.variable",
         "showSelected.value",
         "showSelected2.value",
         "clickSelects.value",
         "clickSelects2.value")
  for(a.vec in a.list){
    expect_error({
      selector.aes(a.vec)
    }, ".variable or .value aes not found")
  }
})

problems <-
  data.frame(problemStart=c(100, 200, 100, 150, 200, 250),
             problemEnd=c(200, 300, 150, 200, 250, 300),
             problem.i=c(1, 2, 1, 2, 3, 4),
             bases.per.problem=c(100, 100, 50, 50, 50, 50))
problems$problem.name <- with(problems, {
  sprintf("size.%d.problem.%d", bases.per.problem, problem.i)
})

sizes <- data.frame(bases.per.problem=c(50, 100),
                    problems=c(2, 4))

problems$peakStart <- problems$problemStart + 10
problems$peakEnd <- problems$problemEnd - 10

samples <-
  rbind(data.frame(problems, sample.id="sample1", peaks=1),
        data.frame(problems, sample.id="sample1", peaks=2),        
        data.frame(problems, sample.id="sample2", peaks=2))

peaks <-
  expand.grid(peaks=0:2, 
              problem.name=problems$problem.name)
peaks$error.type <-
  c("false positive", "false negative", "correct")
rownames(problems) <- problems$problem.name
peaks$bases.per.problem <-
  problems[paste(peaks$problem.name), "bases.per.problem"]

peak.problems <-
  rbind(data.frame(problems, peaks=1),
        data.frame(problems, peaks=2))

viz <-
  list(problems=ggplot()+
         ggtitle("select problem")+
         geom_segment(aes(problemStart, problem.i,
                          clickSelects=problem.name,
                          showSelected=bases.per.problem,
                          xend=problemEnd, yend=problem.i),
                      size=5,
                      data=data.frame(problems, sample.id="problems"))+
         geom_text(aes(200, 5,
                       label=paste("problem size", bases.per.problem),
                       showSelected=bases.per.problem),
                   data=data.frame(sizes, sample.id="problems"))+
         geom_segment(aes(peakStart, problem.i,
                          showSelected.variable=paste0(problem.name, "peaks"),
                          showSelected.value=peaks,
                          clickSelects=problem.name,
                          showSelected2=bases.per.problem,
                          xend=peakEnd, yend=problem.i),
                      data=data.frame(peak.problems, sample.id="problems"),
                      size=10,
                      color="deepskyblue")+
         ## TODO: yend=y=0 as params not aes?
         geom_segment(aes(peakStart, 0,
                          showSelected.variable=paste0(problem.name, "peaks"),
                          showSelected.value=peaks,
                          clickSelects=problem.name,
                          showSelected2=bases.per.problem,
                          xend=peakEnd, yend=0),
                      data=samples,
                      size=10,
                      color="deepskyblue")+
         theme_bw()+
         theme(panel.margin=grid::unit(0, "cm"))+
         facet_grid(sample.id ~ .),

       title="viz with .variable .value",
       
       sizes=ggplot()+
         ggtitle("select problem size")+
         geom_point(aes(bases.per.problem, problems,
                        clickSelects=bases.per.problem),
                    size=10,
                    data=sizes),

       peaks=ggplot()+
         ggtitle("select number of peaks")+
         geom_point(aes(peaks, peaks,
                        color=error.type,
                        id=peaks,
                        showSelected=problem.name,
                        showSelected2=bases.per.problem,
                        clickSelects.variable=paste0(problem.name, "peaks"),
                        clickSelects.value=peaks),
                    size=10,
                    data=peaks)+
         geom_text(aes(1, 3, label=problem.name,
                       showSelected2=bases.per.problem,
                       showSelected=problem.name),
                   data=problems))

info <- animint2HTML(viz)

test_that(".variable and .value makes compiler create selectors", {
  selector.names <- sort(names(info$selectors))
  problem.selectors <- paste0(problems$problem.name, "peaks")
  expected.names <-
    sort(c("problem.name",
           "error.type",
           problem.selectors,
           "bases.per.problem"))
  expect_identical(selector.names, expected.names)
  selected <- sapply(info$selectors[problem.selectors], "[[", "selected")
  expect_true(all(selected == "1"))
})

test_that(".variable and .value renders correctly at first", {
  node.list <-
    getNodeSet(info$html, '//g[@class="geom4_segment_problems"]//line')
  expect_equal(length(node.list), 2)
})

test_that("clicking reduces the number of peaks", {
  no.peaks.html <- clickHTML(id=0)
  node.list <-
    getNodeSet(no.peaks.html, '//g[@class="geom4_segment_problems"]//line')
  expect_equal(length(node.list), 1)
})

test_that("clicking increases the number of peaks", {
  more.peaks.html <- clickHTML(id=2)
  node.list <-
    getNodeSet(more.peaks.html, '//g[@class="geom4_segment_problems"]//line')
  expect_equal(length(node.list), 3)
  ## TODO: test for //g[@class="geom6_point_peaks"]//circle//title
  ## (tooltip that indicates the number of peaks).
})

viz.for <-
  list(problems=ggplot()+
         ggtitle("select problem")+
         geom_segment(aes(problemStart, problem.i,
                          clickSelects=problem.name,
                          showSelected=bases.per.problem,
                          xend=problemEnd, yend=problem.i),
                      size=5,
                      data=data.frame(problems, sample.id="problems"))+
         geom_text(aes(200, 5,
                       label=paste("problem size", bases.per.problem),
                       showSelected=bases.per.problem),
                   data=data.frame(sizes, sample.id="problems"))+
         theme_bw()+
         theme(panel.margin=grid::unit(0, "cm"))+
         facet_grid(sample.id ~ .),

       title="viz with for loop",
       
       sizes=ggplot()+
         ggtitle("select problem size")+
         geom_point(aes(bases.per.problem, problems,
                        clickSelects=bases.per.problem),
                    size=10,
                    data=sizes),

       peaks=ggplot()+
         ggtitle("select number of peaks")+
         geom_text(aes(1, 3, label=problem.name,
                       showSelected=problem.name),
                   data=problems))

pp.list <- split(peak.problems, peak.problems$problem.name)
s.list <- split(samples, samples$problem.name)
p.list <- split(peaks, peaks$problem.name)

for(problem.name in names(p.list)){
  s.name <- paste0(problem.name, "peaks")
  p <- p.list[[problem.name]]
  p[[s.name]] <- p$peaks
  pp <- pp.list[[problem.name]]
  pp[[s.name]] <- pp$peaks
  s <- s.list[[problem.name]]
  s[[s.name]] <- s$peaks
  p$bases.per.problem <- pp$bases.per.problem[1]
  viz.for$problems <- viz.for$problems+
    geom_segment(aes_string("peakStart", "problem.i",
                            id="problem.name",
                            showSelected=s.name,
                            clickSelects="problem.name",
                            showSelected2="bases.per.problem",
                            xend="peakEnd", yend="problem.i"),
                 data=data.frame(pp, sample.id="problems"),
                 size=10,
                 color="deepskyblue")+
    geom_segment(aes_string("peakStart", "0",
                            showSelected=s.name,
                            clickSelects="problem.name",
                            showSelected2="bases.per.problem",
                            xend="peakEnd", yend="0"),
                 data=s,
                 size=10,
                 color="deepskyblue")
  viz.for$peaks <- viz.for$peaks+
         geom_point(aes_string("peaks", "peaks",
                               showSelected="problem.name",
                               showSelected2="bases.per.problem",
                               clickSelects=s.name),
                    size=10,
                    data=p)
}

info <- animint2HTML(viz.for)

chunk.counts <- function(html=getHTML()){
  node.set <-
    getNodeSet(html, '//td[@class="downloaded"]')
  as.integer(sapply(node.set, xmlValue))
}

test_that("counts of chunks downloaded or not at first", {
  value.vec <- chunk.counts()
  expect_equal(value.vec,
               c(1, 1, 1, 1, 1, 1,
                 0, 0, 0, 0, 0, 0, 0, 0,
                 1, 1, 1,
                 0, 0, 0, 0, 0))
})

test_that("changing problem downloads one chunk", {
  clickID('size.100.problem.2')
  value.vec <- chunk.counts()
  expect_equal(value.vec,
               c(1, 1, 1, 1, 1, 1,
                 0, 0, 0, 0, 0, 0, 0, 0,
                 1, 1, 1, 1,
                 0, 0, 0, 0))
})

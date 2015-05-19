context("params")

df <- data.frame(z=rnorm(100))

viz <-
  list(step=ggplot()+
         geom_step(aes(seq_along(z), z),
                   data=df,
                   size=3,
                   color="grey50"))

pattern <-
  paste0("(?<name>\\S+?)",
         ": *",
         "(?<value>.+?)",
         ";")

test_that("color is converted to RGB colour", {
  info <- animint2HTML(viz)

  expect_equal(length(info$geoms), 1)
  g <- info$geoms[[1]]
  expected.colour <- as.character(toRGB("grey50"))
  expect_identical(g$params$colour, expected.colour)
  
  node.list <- getNodeSet(info$html, '//g[@class="geom1_step_step"]//path')
  expect_equal(length(node.list), 1)
  node <- node.list[[1]]
  attr.vec <- xmlAttrs(node)
  style.str <- attr.vec[["style"]]
  style.mat <- str_match_all_perl(style.str, pattern)[[1]]
  style.vec <- style.mat[, "value"]
  expect_identical(style.vec[["fill"]], "none")
  expect_match(style.vec[["stroke-width"]], "3")
  stroke <- style.vec[["stroke"]]
  if(grepl("rgb", stroke)){
    expected.regex <- paste(col2rgb(expected.colour), collapse=", *")
    expect_match(stroke, expected.regex)
    ## On firefox, stroke is "rgb(127, 127, 127)"
  }else{
    print(stroke)
    expect_identical(stroke, expected.colour)
    ## On phantomjs, stroke is "#7f7f7f"
  }
})

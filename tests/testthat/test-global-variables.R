context("global variables")

viz <- list(scatter=ggplot()+
  geom_point(aes(y=Petal.Length, x=Sepal.Length,
                color=Species),
            data=iris))

myScript <- 'myArray = [];
for(var b in window) { 
  if(window.hasOwnProperty(b)) {myArray.push(b);} 
}
return myArray;'

getVariables <- function(){
  remDr$executeScript(myScript)[[1]]
}

test_that("animint.js only defines 1 object, called animint", {
  info <- animint2HTML(viz)
  animint.vars <- getVariables()
  index.file <- file.path("animint-htmltest", "index.html")
  html.lines <- readLines(index.file)
  html.without <- html.lines[!grepl("animint.js", html.lines)]
  cat(html.without, file=index.file, sep="\n")
  remDr$refresh()
  without.vars <- getVariables()
  diff.vars <- animint.vars[!animint.vars %in% without.vars]
  expect_identical(sort(diff.vars), c("animint", "plot"))
})

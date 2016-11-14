acontext("url fragment identifier")

iris$observation <- 1:nrow(iris)
viz <- list(
  petal=ggplot()+
    geom_point(aes(Petal.Width, Petal.Length, fill=Species,
                   id=paste0("petal", observation),
                   clickSelects=observation), data=iris),
  sepal=ggplot()+
    geom_point(aes(Sepal.Width, Sepal.Length, fill=Species,
                   id=paste0("sepal", observation),
                   clickSelects=observation), data=iris),
  first=list(observation=5))

info <- animint2HTML(viz)

test_that("all species are selected for url with no selection", {
  opacity.str <- getStyleValue(info$html, "//td[@class='legend_entry_label']", "opacity")
  opacity.num <- as.numeric(opacity.str)
  opacity.exp <- rep(1, 6)
  expect_equal(opacity.num, opacity.exp)
})

test_that("one observation selected for url with no selection", {
  circle.xpath <- "//circle[@class='geom']"
  node.list <- getNodeSet(info$html, circle.xpath)
  opacity.str <- getStyleValue(info$html, circle.xpath, "opacity")
  names(opacity.str) <- sapply(node.list, xmlAttrs)["id",]
  selected.names <- paste0(c("petal", "sepal"), 5)
  is.selected <- names(opacity.str) %in% selected.names
  expect_equal(as.numeric(opacity.str[is.selected]), rep(1, 2))
  expect_equal(as.numeric(opacity.str[!is.selected]), rep(0.5, 298))
})

old_address <- remDr$getCurrentUrl()[[1]]
new_address <- paste0(old_address, '#Species={setosa}')
remDr$navigate(new_address)
remDr$refresh()
Sys.sleep(10)
html <- getHTML()

test_that("one species is selected for url with selection", {
  entry.xpath <- "//td[@class='legend_entry_label']"
  node.list <- getNodeSet(html, entry.xpath)
  opacity.str <- getStyleValue(html, entry.xpath, "opacity")
  opacity.num <- as.numeric(opacity.str)
  value.vec <- sapply(node.list, xmlValue)
  is.setosa <- value.vec == "setosa"
  expect_equal(opacity.num[is.setosa], rep(1, 2))
  expect_equal(opacity.num[!is.setosa], rep(0.5, 4))
})

test_that("Current Url is displayed properly",{
  url.xpath <- "//tr[@class='selectorurl']//a"
  node <- getNodeSet(html,url.xpath)
  href <- xmlValue(node[[1]])
  expect_equal(href,paste0(new_address,'observation={5}'))
})

remDr$navigate(old_address)


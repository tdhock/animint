acontext("NA separate lines")

data(txhousing)

san.marcos <- subset(txhousing, city=="San Marcos")
## ggplot2 draws separate lines when there are missing values.
ggplot()+
  geom_line(aes(x = date, y = median),
            data=san.marcos)

viz <- list(
  ggdata=ggplot(txhousing)+
    geom_line(aes(x = date, y = median, group = city, 
                  clickSelects=city),
              alpha = 0.6),
  selected=ggplot()+
    geom_line(aes(x = date, y = median, group = city, showSelected=city),
              data=txhousing),
  first=list(city="San Marcos")
)
info <- animint2HTML(viz)

test_that("three <path> rendered for highlighted San Marcos", {
  xpath <- '//g[@class="geom1_line_ggdata"]//path'
  path.list <- getNodeSet(info$html, xpath)
  opacity.str <- getStyleValue(info$html, xpath, "opacity")
  opacity.num <- as.numeric(opacity.str)
  hilite.list <- path.list[opacity.num == 0.6]
  expect_equal(length(hilite.list), 3)
})

test_that("three <path> rendered for selected San Marcos", {
  path.list <- getNodeSet(info$html, '//g[@class="geom2_line_selected"]//path')
  expect_equal(length(path.list), 3)
})

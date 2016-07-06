acontext("tooltip-interactivity")
## !!! wercker has some problem with this test, so not using this !!!
data("WorldBank", package = "animint")
WorldBank1975 <- WorldBank[WorldBank$year == 1975, ]

test_that("Interactivity does not mess up tooltip titles",{
  ## Take only first 3 chars of region names. Avoid non-alphanumerics
  WorldBank1975$region <- tolower(
    substring(as.character(WorldBank1975$region), 1, 3))
  
  ex_plot <- ggplot() +
    geom_point(aes(fertility.rate, life.expectancy, color = region,
                   tooltip = country, href = "https://github.com"),
               data = WorldBank1975)
  
  viz <- list(ex = ex_plot)
  info <- animint2HTML(viz)
  
  # Apply clickID with some time difference
  apply_with_interval <- function(func, list, interval){
    for (elem in list){
      acontext(paste0("id - ", elem))
      Sys.sleep(interval)
      func(elem)
    }
  }
  # Hide some points first and check rendered titles
  hide_these_first <- 
    c("plot_ex_region_variable_eas_label",
      "plot_ex_region_variable_eur_label",
      "plot_ex_region_variable_lat_label",
      "plot_ex_region_variable_mid_label",
      "plot_ex_region_variable_sub_label")
  
  b <- apply_with_interval(clickID, hide_these_first, 1)
  
  Sys.sleep(1)
  info$html <- getHTML()
  
  displayed_regions <- WorldBank1975$region == "nor" | 
    WorldBank1975$region == "sou"
  displayed_countries <- unique(WorldBank1975$country[displayed_regions])
  
  title_nodes1 <-
    getNodeSet(info$html, '//g[@class="geom1_point_ex"]//a//title')
  rendered_titles1 <- sapply(title_nodes1, xmlValue)
  expect_identical(sort(rendered_titles1), sort(displayed_countries))
  
  # Hide all countries -> No titles
  hide_these_second <- 
    c("plot_ex_region_variable_nor_label",
      "plot_ex_region_variable_sou_label")
  b <- apply_with_interval(clickID, hide_these_second, 1)
  
  Sys.sleep(2)
  info$html <- getHTML()
  
  title_nodes2 <-
    getNodeSet(info$html, '//g[@class="geom1_point_ex"]//a//title')
  expect_equal(length(title_nodes2), 0)
  
  # Show previous points again and compare titles
  b <- apply_with_interval(clickID, hide_these_second, 1)
  
  Sys.sleep(1)
  info$html <- getHTML()
  
  title_nodes3 <-
    getNodeSet(info$html, '//g[@class="geom1_point_ex"]//a//title')
  rendered_titles3 <- sapply(title_nodes3, xmlValue)
  expect_identical(sort(rendered_titles3), sort(displayed_countries))
})

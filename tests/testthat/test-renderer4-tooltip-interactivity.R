acontext("tooltip-interactivity")

data("WorldBank", package = "animint")
WorldBank1975 <- WorldBank[WorldBank$year == 1975, ]

test_that("Interactivity does not mess up tooltip titles",{
  WorldBank1975$region <- as.character(WorldBank1975$region)
  only_alphanums <- function(x, pat, repl){gsub(pat, replacement = repl, x)}
  WorldBank1975$region <- sapply(WorldBank1975$region, only_alphanums, 
                                 "[&()]", "_")
  ex_plot <- ggplot() +
    geom_point(aes(fertility.rate, life.expectancy, color = region,
                   tooltip = country, href = "https://github.com"),
               data = WorldBank1975)
  
  viz <- list(ex = ex_plot)
  info <- animint2HTML(viz)
  
  # Apply clickID with some time difference
  apply_with_interval <- function(func, list, interval){
    for (elem in list){
      Sys.sleep(interval)
      func(elem)
    }
  }
  # Hide some points first and check rendered titles
  hide_these_first <- 
    c("plot_ex_region_variable_East_Asia___Pacific__all_income_levels_",
      "plot_ex_region_variable_Europe___Central_Asia__all_income_levels_",
      "plot_ex_region_variable_Latin_America___Caribbean__all_income_levels_",
      "plot_ex_region_variable_Middle_East___North_Africa__all_income_levels_",
      "plot_ex_region_variable_Sub-Saharan_Africa__all_income_levels_")
  
  b <- apply_with_interval(clickID, hide_these_first, 0.01)
  
  Sys.sleep(0.5)
  info$html <- getHTML()
  
  displayed_regions <- WorldBank1975$region == "North America" | 
    WorldBank1975$region == "South Asia"
  displayed_countries <- unique(WorldBank1975$country[displayed_regions])
  
  title_nodes1 <-
    getNodeSet(info$html, '//g[@class="geom1_point_ex"]//a//title')
  rendered_titles1 <- sapply(title_nodes1, xmlValue)
  expect_identical(sort(rendered_titles1), sort(displayed_countries))
  
  # Hide all countries -> No titles
  hide_these_second <- 
    c("plot_ex_region_variable_North_America",
      "plot_ex_region_variable_South_Asia")
  b <- apply_with_interval(clickID, hide_these_second, 1)
  
  Sys.sleep(1)
  info$html <- getHTML()
  
  title_nodes2 <-
    getNodeSet(info$html, '//g[@class="geom1_point_ex"]//a//title')
  expect_equal(length(title_nodes2), 0)
  
  # Show previous points again and compare titles
  b <- apply_with_interval(clickID, hide_these_second, 0.01)
  
  Sys.sleep(0.5)
  info$html <- getHTML()
  
  title_nodes3 <-
    getNodeSet(info$html, '//g[@class="geom1_point_ex"]//a//title')
  rendered_titles3 <- sapply(title_nodes3, xmlValue)
  expect_identical(sort(rendered_titles3), sort(displayed_countries))
})

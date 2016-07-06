acontext("tooltip-interactivity")

data("CO2")

plot_viz <- ggplot() + 
  geom_point(aes(conc, uptake, color=Treatment, tooltip=Plant),
             data = CO2)
viz <- list(p=plot_viz)
info <- animint2HTML(viz)

test_that("Interactivity does not mess up tooltip titles", {
  # Initially all titles are rendered
  title_nodes1 <-
    getNodeSet(info$html, '//g[@class="geom1_point_p"]//circle//title')
  rendered_titles1 <- sapply(title_nodes1, xmlValue)
  
  expect_equal(length(rendered_titles1), length(CO2$Plant))
  expect_identical(sort(unique(rendered_titles1)), 
                   sort(sapply(unique(CO2$Plant), as.character)))
  
  # Hide titles with Treatment = "nonchilled" 
  clickID(c("plot_p_Treatment_variable_nonchilled"))
  info$html <- getHTML()
  
  title_nodes2 <-
    getNodeSet(info$html, '//g[@class="geom1_point_p"]//circle//title')
  rendered_titles2 <- sapply(title_nodes2, xmlValue)
  rendered_titles2_unique <- unique(rendered_titles2)
  actual_titles2 <- c("Qc1", "Qc2", "Qc3", "Mc1", "Mc2", "Mc3")
  
  expect_equal(length(rendered_titles2), sum(CO2$Treatment == "chilled"))
  expect_identical(sort(rendered_titles2_unique), sort(actual_titles2))
  
  # Hide all titles
  clickID(c("plot_p_Treatment_variable_chilled"))
  info$html <- getHTML()
  
  title_nodes3 <-
    getNodeSet(info$html, '//g[@class="geom1_point_p"]//circle//title')
  
  expect_equal(length(title_nodes3), 0)
  
  # Show titles with Treatment = "nonchilled"
  clickID(c("plot_p_Treatment_variable_nonchilled"))
  info$html <- getHTML()
  
  title_nodes4 <-
    getNodeSet(info$html, '//g[@class="geom1_point_p"]//circle//title')
  rendered_titles4 <- sapply(title_nodes4, xmlValue)
  rendered_titles4_unique <- unique(rendered_titles4)
  actual_titles4 <- c("Qn1", "Qn2", "Qn3", "Mn1", "Mn2", "Mn3")
  
  expect_equal(length(rendered_titles4), sum(CO2$Treatment == "nonchilled"))
  expect_identical(sort(rendered_titles4_unique), sort(actual_titles4))
})

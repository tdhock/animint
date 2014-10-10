library(shiny)
appDir <- system.file("examples", "shiny", package = "animint")
runApp(appDir)
# unfortunately, with the current implementation,
# this will create a (probably unwanted) animint_assets folder
# in the app directory. How to do this upon exiting the shiny app?
unlink(file.path(appDir, "animint_assets"), recursive = TRUE)

# Set working directory to the main folder
setwd("../")

# Setup virtual environment
renv::load()

# Set options here
message("Set options")
options(
        golem.app.prod = FALSE,
        shiny.reactlog = TRUE,
        shiny.trace = TRUE,
        shiny.autoreload = TRUE,
        shiny.launch.browser = FALSE,
        # shiny.autoreload.pattern = glob2rx("R/*.R"),
        shiny.port = 8080
) # TRUE = production mode, FALSE = development mode

message(glue::glue("Current environment: {getwd()}"))
# message(glue::glue("Renv: {renv::status()}"))

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

options(shiny.autoreload = TRUE)
# Run the application
cashApp()
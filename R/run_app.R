#' Run app
#'
#' @return
#' @export
#'
#' @examples
cashApp <- function() {
  file_list <- list_files()

  my_theme <- bslib::bs_theme(version = 4,
                              base_font = bslib::font_google("Fira Sans"),
                              code_font = bslib::font_google("Fira Code"),
                              heading_font = bslib::font_google("Arvo")
                              ) %>%
    bslib::bs_theme_update(font_scale = NULL, `enable-gradients` = TRUE,
                  `enable-shadows` = TRUE, `enable-rounded` = TRUE, spacer = "1rem",
                  bootswatch = "darkly")

  ui <- shiny::bootstrapPage(
    theme = my_theme,
    tags$head(htmltools::includeCSS("./inst/app/www/styles.css")),
    tags$head(HTML('<script src="https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/js/bootstrap.min.js" integrity="sha384-JZR6Spejh4U02d8jOt6vLEHfe/JQGiRRSQQxSfFWpi1MquVdAyjUar5+76PVCmYl" crossorigin="anonymous"></script>')),

    mod_main_ui("mod_main")
  )

  server <- function(input, output, session) {
    session$onSessionEnded(function() {
      # stopApp()
    })
    mod_main_server("mod_main", file_list)
  }

  shiny::shinyApp(ui, server)
}


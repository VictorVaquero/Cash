#' main UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
mod_main_ui <- function(id) {
  ns <- NS(id)

  tags$div(
    class = "container-fluid h-100",
    tags$div(
      class = "row h-100",
      tags$nav(
        class = "col-5 col-sm-4 col-md-3 col-lg-2 sidebar bg-dark p-0",
        tags$div(
          class = "pl-3 pt-2 pb-2 logo shadow-sm",
          icon("money-bill-wave"),
          tags$a(class = "navbar-brand", href = "#", 'Cash')
        ),
        tags$div(
          class = "list-group pl-0 h-100 bg-dark shadow",
          id = "list-sidebar",
          sideItem("Portada", "wallet", .active = TRUE),
          sideItem("Metadatos", "book"),
          sideItem("Resumen global", "chart-pie"),
          sideItem("Planificación y detalles", "piggy-bank")
          # textOutput(ns("aux"))
        )
      ),
      tags$div(
        class = "col-7 col-sm-8 col-md-9 col-lg-10 p-0",
        tags$nav(
          class = "navbar navbar-expand-sm navbar-dark bg-dark p-0 shadow-sm",
          tags$div(
            class = "d-flex justify-content-between flex-wrap flex-md-nowrap align-items-center pt-4 mb-3  w-100",
            tags$div(
              class = "btn-toolbar ml-auto mr-2",
              tags$button(class = "btn", "Ajustes")
            )
          )
        ),
        tags$main(
          class = "m-4 tab-content", role = "main",

          customPage("Metadatos",  mod_metadata_ui(ns("mod_metadata"))),
          customPage("Resumen global", mod_visualizations_ui(ns("mod_visualizations"))),
          customPage("Planificación y detalles", uiOutput(ns("redirect_mod_planification")))

        )
      )
    )
  )
}

#' main Server Functions
#'
#' @noRd
#'  @param file_list tibble with all cash related data files available
#'
#' @import shiny
mod_main_server <- function(id, file_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    metadata_results <- mod_metadata_server("mod_metadata", file_list)
    transactions <- mod_visualizations_server("mod_visualizations", metadata_results$book, metadata_results$accounts)
    mod_planification_server("mod_planification", metadata_results$accounts, transactions)

    output$aux <- renderText({
      paste("AS: ", shiny::getQueryString())
    })

    output$redirect_mod_planification <- renderUI({
      mod_planification_ui(ns("mod_planification"), metadata_results$accounts)
    })

  })
}


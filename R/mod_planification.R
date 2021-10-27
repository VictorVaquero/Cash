#' Planification UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_planification_ui <- function(id, accounts) {
  ns <- NS(id)
  tagList(
    tags$div(
      class = "row",
      tags$div(
        class = "col-12 card-deck",
        customCard(
          title = tags$div(
            class = "d-flex align-items-center",
            tags$h5(class = "mr-auto mb-0", tags$span(class = "text-info", "Real"), " vs ", tags$span(class = "text-danger", "Predicho")),
            shinyWidgets::pickerInput(
              inputId = ns("account_proyection_button"),
              label = "",
              selected = "6df1ed637347daee156997bebd11924a",
              choices = setNames(accounts()$id, accounts()$nombre),
              options = list(
                `live-search` = TRUE,
                `multiple data-selected-text-format` = "count > 1",
                `style` = ""
              ),
              multiple = TRUE,
              inline = TRUE,
              width = "fit"
            )
            # tags$button(class = "btn btn-outline-primary action-button shiny-bound-input", id = ns("account_proyection_button"), "TODO")
          ),
          content = plotly::plotlyOutput(ns("account_proyection_plot"))
        ),
        customCard(
          title = "",
          content = ""
        )
      )
    ),
    tags$div(
      class = "row mt-3",
      tags$div(
        class = "col-6 card-deck",
        customCard(
          "Gastos en viajes",
          content = plotly::plotlyOutput(ns("trips_summarized_expenses"))
        )
      )
    )
  )
}

#' Planification Server Functions
#'
#' @noRd
mod_planification_server <- function(id, accounts, transactions) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    state <- shiny::reactiveValues(
      account_proyection_selected = "Cerveza"
    )

    # Seter
    shiny::observeEvent(input$account_proyection_button, {
      message(glue::glue(".....selected {input$account_proyection_button}"))
      state$account_proyection_selected <- input$account_proyection_button
    })

    # Visualizations

    output$account_proyection_plot <- plotly::renderPlotly({
      message(".....account proyection plot")

      p <- calculate_proyections(transactions(), accounts(), state$account_proyection_selected)
      plot_proyections(p$fitted, p$forecasted)
    })

    output$trips_summarized_expenses <- plotly::renderPlotly({
      message(".....trips summarized expenses plot")

      data <- calculate_trips_expenses(transactions())
      plot_trips_expenses(data)
    })
  })
}


calculate_proyections <- function(transactions, accounts, selected_accounts, .do_reset = FALSE) {

  # print(accounts)
  # print(accounts %>%
  #         dplyr::filter(id %in% selected_accounts))
  # print(accounts %>%
  #         dplyr::filter(id %in% selected_accounts) %>%
  #         dplyr::pull(lista_cuentas_ids))
  # TODO: Resetea los gastos
  if(any(purrr::map_lgl(accounts %>%
                    dplyr::filter(id %in% selected_accounts) %>%
                    dplyr::pull(lista_cuentas_ids),
                    ~ "6df1ed637347daee156997bebd11924a" %in% .x)))
    .do_reset <- TRUE

  # Movimiento gasto diario
  data <- transactions %>%
    dplyr::filter(purrr::map_lgl(
      lista_cuentas_ids,
      ~ any(.x %in% selected_accounts)
    ))

  if(nrow(data) == 0) {
    warning("No transactions found in the selected accounts")
    # TODO: Imagen extra? Grafico en blanco?
  }

  data <- data %>%
    # dplyr::mutate(cuenta = purrr::map_chr(lista_cuentas, ~ .x[1])) %>% # TODO: Porque hice esto?
    dplyr::mutate(cuenta = nombre) %>%
    dplyr::arrange(fecha_transacion) %>%
    mutate(valor_acumulado = cumsum(valor))

  my_ts <- ts(data = data$valor_acumulado, frequency = 1)
  fit <- forecast::ets(my_ts)
  # sqrt(fit$sigma2)

  fitted_model <- fit %>%
    {
      tibble(
        real = .$x %>% as.numeric(),
        fitted = .$fitted %>% as.numeric(),
        res = .$residuals %>% as.numeric(),
        date = data$fecha_transacion
      )
    }

  if(.do_reset)
    fitted_model <- fitted_model %>%
      dplyr::mutate(
        reset = if_else(lubridate::month(date) != dplyr::lag(lubridate::month(date)), real, 0),
        reset = replace_na(reset, 0)
      ) %>%
      dplyr::group_by(cumsum(reset)) %>%
      dplyr::mutate(reset = first(reset)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        real = real - reset,
        fitted = pmax(0, fitted - reset)
      )

  # TODO: Porque calculo dos veces los intervalos?
  fitted_model <- fitted_model %>%
    dplyr::mutate(
      fitted_min = pmax(0, fitted - 1.96 * sqrt(fit$sigma2)),
      fitted_max = fitted + 1.96 * sqrt(fit$sigma2)
    )

  forecasted <- forecast::forecast(fit, h = days_remainder(last(fitted_model$date))) %>%
    dplyr::as_tibble() %>%
    dplyr::transmute(
      date = last(fitted_model$date) + lubridate::days(row_number()),
      fitted_max = `Hi 95`,
      fitted_min = `Lo 95`,
      fitted = `Point Forecast`
    )

  if(.do_reset)
    forecasted <- forecasted %>%
      dplyr::mutate(
        fitted_max = fitted_max - last(fitted_model$reset),
        fitted_min = fitted_min - last(fitted_model$reset),
        fitted = fitted - last(fitted_model$reset)
      )

  forecasted <- forecasted %>%
    dplyr::add_row(
      fitted_model %>%
        filter(date == last(date)) %>%
        transmute(
          date,
          fitted,
          fitted_max = fitted,
          fitted_min = fitted
        ),
      .before = 1
    )

  return(list(
    fitted = fitted_model,
    forecasted = forecasted
  ))
}

calculate_trips_expenses <- function(transactions) {

  # TODO: Sacar fuera
  mapping_table <- list(
    "Viaje Vitoria 2021" = tolower(c("Viaje Vitoria")),
    "Viaje Navaluenga 2021" = tolower(c("Viaje Navaluenga 2021")),
    "Visita Madrid 2021" = tolower(c("Visita madrid 2021", "Viaje madrid 2021")),
    "Camping 2021" = tolower(c("Viaje camping 2021")),
    "Viaje Canarias 2021" = tolower(c("Viaje canarias 2021")),
    "Barcelona 2021" = tolower(c("Viaje barcelona 2021"))
  )


  # TODO: Modif directa en transaciones ?
  data <- transactions %>%
    dplyr::filter(nombre == "Viajes") %>%
    dplyr::mutate(
      notas =
        purrr::map_chr(notas, ~ {
          viaje <- .x
          normalizado <- names(mapping_table)[purrr::map_lgl(mapping_table, ~ any(tolower(viaje) %in% .x))]
          if(!length(normalizado)) {
            normalizado <- viaje
            message(glue::glue("No mapping table for trip - {viaje}"))
          }

          return(normalizado)
        })
    )

  data <- data %>%
    dplyr::group_by(notas) %>%
    dplyr::summarise(
      valor = sum(valor)
    )

  return(data)
}

# TODO: Controlar ya la paleta de colores antes de que se vaya de las manos
plot_proyections <- function(fitted, forecasted) {
  plotly::plot_ly() %>%
    vline(
      x = fitted %>% filter(real == 0) %>% pull(date),
      dash = "dash",
      color = "rgba(256, 256, 256, 0.5)"
    ) %>%
    plotly::add_ribbons(
      data = fitted,
      x = ~date,
      ymin = ~fitted_min,
      ymax = ~fitted_max,
      name = "Predicho",
      line = list(width = 0),
      fillcolor = "rgba(227, 74, 51, 0.1)"
    ) %>%
    plotly::add_lines(
      data = fitted,
      x = ~date,
      y = ~fitted,
      name = "Predicho",
      line = list(color = "rgba(227, 74, 51, 0.2)"),
    ) %>%
    plotly::add_lines(
      data = fitted,
      x = ~date,
      y = ~real,
      line = list(color = "rgb(52, 152, 219)"),
      name = "Real"
    ) %>%
    plotly::add_ribbons(
      data = forecasted,
      x = ~date,
      ymin = ~fitted_min,
      ymax = ~fitted_max,
      name = "Predicho",
      line = list(width = 0),
      fillcolor = "rgba(227, 74, 51, 0.1)"
    ) %>%
    plotly::add_lines(
      data = forecasted,
      x = ~date,
      y = ~fitted,
      name = "Predicho",
      line = list(color = "rgba(227, 74, 51, 0.2)"),
    ) %>%
    my_plotly_theme() %>%
    plotly::layout(
      showlegend = FALSE,
      xaxis = list(title = ""),
      yaxis = list(title = "Euros")
    )
}

plot_trips_expenses <- function(data) {

  fig <- plotly::plot_ly(
    showlegend = FALSE,
    orientation = "h"
  )

  purrr::walk(unique(data$notas), ~ {
    fig <<- fig %>%
      plotly::add_bars(
        data = data %>%
          filter(notas == .x),
        y = ~ glue::glue("{notas}  "),
        x = ~valor,
        color = ~notas,
        colors = my_palette(9),
        name = .x,
        marker = list(
          # colors = colors,
          line = list(color = "#FFFFFF", width = 1)
        ),
        text = ~ paste(valor, " €"),
        # textposition = "inside",
        # textinfo = "valor+label",
        # insidetextfont = list(color = "#FFFFFF"),
        hoverinfo = "text"
      )
  })

  fig <- fig %>%
    plotly::layout(
      # title = paste0("Gastos disgregados para el mes de ", ini_mes, collapse = ""),
      title = "",
      yaxis = list(
        title = ""
      ),
      xaxis = list(
        title = "Euros"
      ),
      margin = list(
        l = "0"
      )
    )
  fig %>%
    my_plotly_theme()

}

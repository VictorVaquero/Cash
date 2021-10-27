#' visualizations UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_visualizations_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(
      class = "row",
      tags$div(
        class = "col-12 card-deck",
        customCard(
          title = tags$h5(tags$span(class = "text-success", "Ingresos"), " y ", tags$span(class = "text-danger", "Gastos")),
          content = plotly::plotlyOutput(ns("global_income_loss_plot"))),
        customCard(
          tags$h5("Movimiento diario"),
          content = plotly::plotlyOutput(ns("daily_movement_plot"))),
        customCard(
          title = tags$div(class = "d-flex align-items-center",
                           tags$h5(class = "mr-auto mb-0", tags$span(class = "text-success", "Ganancias"), " o ", tags$span(class = "text-danger", "Perdidas")),
                           tags$button(class="btn btn-outline-primary action-button shiny-bound-input", id = ns("bar_line_daily_movement_button"), "Bar/Line")
                           ),
          content = plotly::plotlyOutput(ns("daily_total_movement_plot")))
      )
    ),
    tags$div(
      class = "row mt-3",
      tags$div(
        class = "col-12 card-deck",
        customCard(
          # title = tags$p(tags$span(class = "text-success", "Ganancias"), " o ", tags$span(class = "text-danger", "Perdidas")),
          title = "Desglose mensual",
          content = plotly::plotlyOutput(ns("expenses_bar_plot"))),
        customCard(
          # title = tags$p(tags$span(class = "text-success", "Ganancias"), " o ", tags$span(class = "text-danger", "Perdidas")),
          content = tags$div(
            plotly::plotlyOutput(ns("full_pie_plot")),
            mydateInput(ns("date_expenses_variable"), NULL,
                             value = lubridate::today(),
                             startview = "month",
                             language = "es",
                             min = lubridate::ymd("2021-03-01"),
                             max = lubridate::today(),
                             # autoclose = TRUE, # TODO: this
                             minviewmode = "months"
                             )
            ))
      )
    )

  )
}

#' Visualizations Server Functions
#'
#' @noRd
mod_visualizations_server <- function(id, book, accounts){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ## ------ Reactives
    state <- shiny::reactiveValues(
      bar_line_daily_movement = "line",
      date_expenses_variable = lubridate::month(lubridate::today(), label = TRUE, abbr = FALSE)
    )

    transactions <- reactive({
      message(".....load transactions")
      get_book_transactions(book(), accounts())
    })

    ## ------ Logica

    # Setters
    shiny::observeEvent(input$bar_line_daily_movement_button, {
        if (state$bar_line_daily_movement == "bar")
          state$bar_line_daily_movement <- "line"
        else
          state$bar_line_daily_movement <- "bar"
        # message(glue::glue(".....change status bar line button to {state$bar_line_daily_movement}"))
    })

    shiny::observeEvent(input$date_expenses_variable, {
      state$date_expenses_variable <- lubridate::month(input$date_expenses_variable, label = TRUE, abbr = FALSE)
    })

    ## ---- Visualizations
    output$global_income_loss_plot <- plotly::renderPlotly({
      message(".....render global income loss plot!")
      global_income_loss_plot(transactions())
    })

    output$daily_movement_plot <- plotly::renderPlotly({
      message(".....render daily movement plot!")
      daily_movement_plot(transactions())
    })

    output$daily_total_movement_plot <- plotly::renderPlotly({
      message(".....render daily total movement plot!")
      if (state$bar_line_daily_movement == "line")
        return(daily_total_movement_plot(transactions()))
      if (state$bar_line_daily_movement == "bar")
        return(monthly_movement_total_plot(transactions()))
    })


    output$expenses_bar_plot <- plotly::renderPlotly({
      message(".....render expenses bar plot!")
      expenses_bar_plot(transactions())
    })

    output$full_pie_plot <- plotly::renderPlotly({
      message(".....render full pie plot!")
      # full_pie_plot(transactions())
      full_expenses_plot(transactions(), state$date_expenses_variable)
    })

    return(transactions)
  })
}




# Gastos - Ingresos por mes (lineas)
global_income_loss_plot <- function(transactions) {
  data <- transactions %>%
    dplyr::filter(cuenta_base %in% c("Ingresos", "Gastos")) %>%
    dplyr::group_by(mes, cuenta_base) %>%
    dplyr::summarise(valor = sum(valor))

  plotly::plot_ly(
    data = data,
    x = ~mes,
    y = ~ abs(valor),
    color = ~cuenta_base,
    type = "scatter",
    mode = "line",
    colors = my_palette(2)
  ) %>%
    plotly::add_trace(
      data = data,
      x = ~mes,
      y = ~ abs(valor),
      color = ~cuenta_base,
      type = "scatter",
      marker = list(
        opacity = 0.8,
        size = 10,
        line = list(width = 2,
                    color = "rgb(256,256,256)")
      ),
      colors = my_palette(2)
    )  %>%
    plotly::layout(
      title = "",
      font = my_font,
      xaxis = list(title = ""),
      yaxis = list(
        title = "Euros",
        rangemode = "tozero"
      ),
      showlegend = FALSE
    ) %>%
    my_plotly_theme()
}

# Movimiento total diario
daily_movement_plot <- function(transactions) {

  cuentas <- transactions %>%
    dplyr::filter(purrr::map_lgl(lista_cuentas, ~ "Activo circulante" %in% .x)) %>%
    dplyr::mutate(cuenta = purrr::map_chr(lista_cuentas, ~ .x[1])) %>%
    dplyr::bind_rows(transactions %>%
                       dplyr::filter(purrr::map_lgl(lista_cuentas, ~ "Cesar" %in% .x)) %>%
                       dplyr::mutate(cuenta = purrr::map_chr(lista_cuentas, ~ .x[1]))) %>%
    dplyr::pull(cuenta)

  data <- transactions %>%
    dplyr::filter(cuenta_base %in% c("Activo", "Pasivo")) %>%
    dplyr::arrange(fecha_transacion) %>%
    dplyr::mutate(valor = cumsum(valor))

  pt <- plotly::plot_ly(
    data = data,
    x = ~fecha_transacion,
    y = ~valor,
    type = "scatter",
    mode = "lines",
    line = list(color = my_palette(7)[1]),
    name = "Total"
  )
  # %>%
  #   plotly::add_trace(
  #     data = data,
  #     x = ~fecha_transacion,
  #     y = ~valor,
  #     type = "scatter",
  #     mode = "markers",
  #     marker = list(
  #       opacity = 0.5,
  #       color = my_palette(7)[1],
  #       size = 3,
  #       line = list(width = 0.5,
  #                   color = "rgb(256,256,256)")
  #     ),
  #     name = "Total"
  #   )

  purrr::iwalk(unique(cuentas), ~ {
    name <- .x
    data <- transactions %>%
      dplyr::filter(purrr::map_lgl(lista_cuentas, ~ name %in% .x)) %>%
      dplyr::arrange(fecha_transacion) %>%
      dplyr::mutate(valor = cumsum(valor))

    pt <<- pt %>%
      plotly::add_lines(
        data = data,
        name = name,
        # type = "scatter",
        # mode = "lines",
        # marker = list(
        #   opacity = 0.5,
        #   color = my_palette(7)[1+.y],
        #   size = 3,
        #   line = list(width = 0.5,
        #               color = "#FFFFFF")
        # ),
        line = list(color = my_palette(7)[1+.y]),
      )
  })

  pt %>%
    plotly::layout(
      title = "",
      font = my_font(),
      xaxis = list(title = ""),
      yaxis = list(
        title = "Euros",
        rangemode = "tozero"
      ),
      showlegend = FALSE
    ) %>%
    my_plotly_theme()
}

# Movimiento ingresos gastos diario (tendencia total)
# TODO: Arreglar lo de los colores cuando cambia de signo
daily_total_movement_plot <- function(transacciones) {
  data <- transacciones %>%
    dplyr::filter(cuenta_base %in% c("Ingresos", "Gastos")) %>%
    dplyr::arrange(fecha_transacion) %>%
    dplyr::mutate(
      valor = cumsum(-valor),
      sign = sign(valor),
      trace = sign(valor) != lag(sign(valor)),
      trace = dplyr::if_else(is.na(trace), rep(FALSE, length(trace)), trace),
      trace = cumsum(trace)
    )

  data <- data %>%
    bind_rows(data %>%
                dplyr::filter(trace != lag(trace)) %>%
                dplyr::mutate(
      trace = trace -
        1,
      sign = -sign
    )) %>%
    dplyr::mutate(
      sign = factor(sign),
      trace = factor(trace)
    ) %>%
    dplyr::arrange(fecha_transacion, trace)

  plotly::plot_ly(
    data = data %>% dplyr::group_by(trace),
    x = ~fecha_transacion,
    y = ~valor,
    color = ~sign,
    colors = my_palette(2)
  ) %>%
    plotly::add_lines() %>%
    plotly::layout(
      title = "",
      font = my_font(),
      xaxis = list(title = ""),
      yaxis = list(
        title = "Euros",
        rangemode = "tozero"
      ),
      showlegend = FALSE
    ) %>%
    my_plotly_theme()
}


monthly_movement_total_plot <- function(transactions) {
  # Same pero con barras
  data <- transactions %>%
    filter(cuenta_base %in% c("Ingresos", "Gastos")) %>%
    arrange(fecha_transacion) %>%
    group_by(mes) %>%
    summarise(
      valor = sum(-valor),
      sign = factor(sign(valor)),
      color = setNames(my_palette(2), c(-1, 1))[sign]
    ) %>%
    arrange(mes) %>%
    group_by(mes)

  palette <- setNames(my_palette(2), c(-1, 1)) %>%
    purrr::map_chr(~glue::glue({.x}, "66"))

  plotly::plot_ly() %>%
    plotly::add_bars(
      data = data %>% filter(sign==-1),
      x = ~mes,
      y = ~valor,
      marker = list(
        color = palette["-1"],
        line = list(color = my_palette(2)[1],
                    opacity = 1, width = 5))
    ) %>%
    plotly::add_bars(
      data = data %>% filter(sign==1),
      x = ~mes,
      y = ~valor,
      marker = list(
        color = palette["1"],
        line = list(color = my_palette(2)[2],
                    opacity = 1, width = 5))
    ) %>%
    hline(y = mean(data$valor), color = "white", dash = "dash") %>% # TODO: Darle un hover a esto
    plotly::layout(
      title = "",
      font = my_font(),
      xaxis = list(title = ""),
      yaxis = list(
        title = "Euros",
        rangemode = "tozero"
      ),
      showlegend = FALSE
    ) %>%
    my_plotly_theme()
}

# Desglose de gastos (barras)
# TOOD: Más detallado
expenses_bar_plot <- function(transacciones) {
  data <- transacciones %>%
    dplyr::filter(cuenta_base == "Gastos") %>%
    dplyr::mutate(cuenta_segundo_nivel = purrr::map_chr(lista_cuentas, ~ .x[length(.x)-2]),
                  cuenta_segundo_nivel = dplyr::if_else(cuenta_segundo_nivel == "Otros gastos",
                                                        purrr::map_chr(lista_cuentas, ~ .x[1]),
                                                        cuenta_segundo_nivel)) %>%
    dplyr::group_by(mes, cuenta_segundo_nivel) %>%
    dplyr::summarise(valor = sum(valor))

  plotly::plot_ly(
    data = data,
    x = ~mes,
    y = ~valor,
    color = ~cuenta_segundo_nivel,
    colors = my_palette(9),
    marker = list(
      opacity = 1,
      line = list(width = 2, opacity = 1, color = "#FFFFFF")),
    type = "bar"
  ) %>%
    plotly::layout(barmode = "stack",
                   xaxis = list(title = ""),
                   yaxis = list(title = "Euros")) %>%
    my_plotly_theme()
}

# Desglose comida último més (tarta) o ultimos meses
full_pie_plot <- function(transacciones) {
  ini_mes <- "junio"
  meses <- levels(transacciones$mes)
  steps <- purrr::imap(meses, ~
                  list(
                    args = list(visible = c(
                      rep(FALSE, .y - 1), TRUE, rep(FALSE, length(meses) - .y)
                    )),
                    method = "restyle",
                    name = .x,
                    value = .x
                  ))
  data <- transacciones %>%
    dplyr::filter(cuenta_base %in% c("Gastos")) %>%
    dplyr::mutate(cuenta = purrr::map_chr(lista_cuentas, ~ ifelse(length(.x) > 3, .x[length(.x) -
                                                                         3], .x[length(.x) - 2]))) %>%
    dplyr::group_by(mes, cuenta) %>%
    dplyr::summarise(valor = sum(valor))

  fig <- plotly::plot_ly(
    data,
    labels = ~cuenta,
    values = ~valor,
    showlegend = FALSE
  )

  purrr::walk(meses, ~ {
    fig <<- fig %>%
      plotly::add_pie(
        data = data %>%
          dplyr::filter(mes == .x),
        labels = ~cuenta,
        values = ~valor,
        # name = .x,
        visible = ini_mes == .x,
        textposition = "inside",
        textinfo = "valor+label+percent",
        insidetextfont = list(color = "#FFFFFF"),
        colors = my_palette(9),
        marker = list(
          colors = colors,
          line = list(color = "#FFFFFF", width = 1),
          text = ~ paste(cuenta, "\n", "$", valor, " euros")
        ),
        hoverinfo = "text"
      )
  })

  fig <- fig %>%
    plotly::layout(
      # title = paste0("Gastos disgregados para el mes de ", ini_mes, collapse = ""),
      title = "",
      xaxis = list(
        title = "",
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE
      ),
      yaxis = list(
        title = "Euros",
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE
      ),
      sliders = list(list(
        active = ini_mes,
        currentvalue = list(prefix = "Mes: "),
        steps = steps
      ))
    )
  fig %>%
    my_plotly_theme()
}
# Desglose comida último més (tarta) o ultimos meses
full_expenses_plot <- function(transacciones, month) {
  # ini_mes <- "junio"
  ini_mes <- month
  meses <- levels(transacciones$mes)
  steps <- purrr::imap(meses, ~
                         list(
                           args = list(visible = c(
                             rep(FALSE, .y - 1), TRUE, rep(FALSE, length(meses) - .y)
                           )),
                           method = "restyle",
                           name = .x,
                           value = .x
                         ))
  data <- transacciones %>%
    dplyr::filter(cuenta_base %in% c("Gastos")) %>%
    dplyr::mutate(cuenta = purrr::map_chr(lista_cuentas, ~ ifelse(length(.x) > 3, .x[length(.x) -
                                                                                       3], .x[length(.x) - 2]))) %>%
    dplyr::group_by(mes, cuenta) %>%
    dplyr::summarise(valor = sum(valor))

  fig <- plotly::plot_ly(
    showlegend = FALSE
  )

  purrr::walk(meses, ~ {
    fig <<- fig %>%
      plotly::add_bars(
        data = data %>%
          filter(mes == .x),
        # labels = ~cuenta,
        # values = ~valor,
        x = ~cuenta,
        y = ~valor,
        color = ~cuenta,
        colors = my_palette(9),
        name = .x,
        visible = ini_mes == .x,
        text = ~cuenta,
        hovertemplate = paste(
          '%{x}<br>',
          '%{y:.2f}<i>$</i>'),
        # textposition = "inside",
        # textinfo = "valor+label+percent",
        # insidetextfont = list(color = "#FFFFFF"),
        marker = list(
          colors = colors,
          line = list(color = "#FFFFFF", width = 1)
          # text = ~ paste(cuenta, "\n", "$", valor, " euros")
        ),
        hoverinfo = "text"
      )
  })

  fig <- fig %>%
    plotly::layout(
      # title = paste0("Gastos disgregados para el mes de ", ini_mes, collapse = ""),
      title = "",
      xaxis = list(
        title = ""
      ),
      yaxis = list(
        title = "Euros"
      ),
      sliders = list(list(
        active = ini_mes,
        currentvalue = list(prefix = "Mes: "),
        steps = steps
      ))
    )
  fig %>%
    my_plotly_theme()
}

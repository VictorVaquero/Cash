################################################
## title: Procesado de datos financieros
## author: Victor Vaquero Martinez
## date: 1 may 2021
################################################

#
# rm(list = ls())
#
# ## ------------ Librerias ------------
#
# # Manejo librerías
# # library(conflicted)
#
# # Manejo paths
# # library(here)
#
# # General
# library(tidyverse)
# library(lubridate)
# # library(stringi)
#
#
# # Gráficos
# library(plotly)
# library(htmltidy)
#
# # Conflictos
# conflict_prefer("filter", "dplyr")
# conflict_prefer("here", "here")
# conflict_prefer("layout", "graphics")
# conflict_prefer("lag", "dplyr")

## ------------ Variables ------------


## ---------------------------------
## ------------ Lectura ------------
## ---------------------------------

file_list <- list_files()

# Lectura de datos (xml)
# TODO: Ahora solo el último, checkear consistencia
latest_file <- file_list %>%
  filter(
    type == "gnca",
    last_date == max(last_date)
  )
f <- read_cash_xml(latest_file)

books <- get_books(f)
book <- books[1]

## ------------------------------------------------------
## ------------ Limpieza y procesado inicial ------------
## ------------------------------------------------------

# Metadatos del libro
get_book_metadata(book)

# Metadastos de las divisas
get_book_currencies(book)

# Metadatos de cuentas
cuentas <- get_book_accounts(book) %>%
  get_parents_list()
cuentas

# Datos de transacciones
transacciones <- get_book_transactions(book, cuentas)


## ----------------------------------
## ------------ Análisis ------------
## ----------------------------------

my_font <- list(family = "Myriad Pro")

# Gastos - Ingresos por mes (lineas)
pal <- c("#e34a33", "#a1d99b")
plotly::plot_ly(
  data = transacciones %>%
    filter(cuenta_base %in% c("Ingresos", "Gastos")) %>%
    group_by(mes, cuenta_base) %>%
    summarise(valor = sum(valor)),
  x = ~mes,
  y = ~ abs(valor),
  color = ~cuenta_base,
  type = "scatter",
  mode = "line",
  colors = pal
) %>%
  plotly::add_trace(
    data = transacciones %>%
      filter(cuenta_base %in% c("Ingresos", "Gastos")) %>%
      group_by(mes, cuenta_base) %>%
      summarise(valor = sum(valor)),
    x = ~mes,
    y = ~ abs(valor),
    color = ~cuenta_base,
    type = "scatter",
    mode = "markers",
    marker = list(
      opacity = 0.8,
      size = 10,
      line = list(width = 3, opacity = 1, color = "rgb(256, 256, 256")),
    colors = pal
  ) %>%
  plotly::layout(
    title = "Perdidas y ganancias",
    font = my_font,
    xaxis = list(title = ""),
    yaxis = list(
      title = "Euros",
      rangemode = "tozero"
    )
  ) %>%
  my_plotly_theme()

# Movimiento total diario
data <- transacciones %>%
  filter(map_lgl(lista_cuentas, ~ "Activo circulante" %in% .x)) %>%
  mutate(cuenta = map_chr(lista_cuentas, ~ .x[1])) %>%
  bind_rows(transacciones %>%
    filter(map_lgl(lista_cuentas, ~ "Cesar" %in% .x)) %>%
    mutate(cuenta = map_chr(lista_cuentas, ~ .x[1])))
pt <- plot_ly(
  data = transacciones %>%
    filter(cuenta_base %in% c("Activo", "Pasivo")) %>%
    arrange(fecha_transacion) %>%
    mutate(valor = cumsum(valor)),
  x = ~fecha_transacion,
  y = ~valor,
  type = "scatter",
  mode = "line+dots",
  colors = pal,
  name = "Total"
)

for (name in unique(data$cuenta)) {
  pt <- pt %>%
    add_trace(
      data = transacciones %>%
        filter(map_lgl(lista_cuentas, ~ name %in% .x)) %>%
        arrange(fecha_transacion) %>%
        mutate(valor = cumsum(valor)),
      x = ~fecha_transacion,
      y = ~valor,
      type = "scatter",
      mode = "line+dots",
      colors = pal,
      name = name
    )
}

pt %>%
  plotly::layout(
    title = "Pasta",
    font = my_font,
    xaxis = list(title = ""),
    yaxis = list(
      title = "Euros",
      rangemode = "tozero"
    )
  )



# Movimiento ingresos gastos diario (tendencia total)
# TODO: Arreglar lo de los colores cuando cambia de signo
data <- transacciones %>%
  filter(cuenta_base %in% c("Ingresos", "Gastos")) %>%
  arrange(fecha_transacion) %>%
  mutate(
    valor = cumsum(-valor),
    sign = sign(valor),
    trace = sign(valor) != lag(sign(valor)),
    trace = if_else(is.na(trace), rep(FALSE, length(trace)), trace),
    trace = cumsum(trace)
  )

data <- data %>%
  bind_rows(data %>% filter(trace != lag(trace)) %>% mutate(
    trace = trace -
      1,
    sign = -sign
  )) %>%
  mutate(
    sign = factor(sign),
    trace = factor(trace)
  ) %>%
  arrange(fecha_transacion, trace)

plotly::plot_ly(
  data = data %>% group_by(trace),
  x = ~fecha_transacion,
  y = ~valor,
  color = ~sign,
  colors = pal
) %>%
  plotly::add_lines() %>%
  plotly::layout(
    title = "Pasta",
    font = my_font,
    xaxis = list(title = ""),
    yaxis = list(
      title = "Euros",
      rangemode = "tozero"
    ),
    showlegend = FALSE
  )


# Desglose de gastos (barras)
# TOOD: Más detallado
plotly::plot_ly(
  data = transacciones %>%
    dplyr::filter(cuenta_base == "Gastos") %>%
    dplyr::mutate(cuenta_segundo_nivel = purrr::map_chr(lista_cuentas, ~ .x[length(.x) -
      2])) %>%
    group_by(mes, cuenta_segundo_nivel) %>%
    summarise(valor = sum(valor)),
  x = ~mes,
  y = ~valor,
  color = ~cuenta_segundo_nivel,
  marker = list(
    opacity = 0.6,
    line = list(width = 2, opacity = 0.9, color = "rgb(0,0,0)")),
  type = "bar"
) %>%
  plotly::layout(barmode = "stack")





# Desglose comida último més (tarta) o ultimos meses

ini_mes <- "abr"
meses <- levels(transacciones$mes)
steps <- imap(meses, ~
list(
  args = list(visible = c(
    rep(FALSE, .y - 1), TRUE, rep(FALSE, length(meses) - .y)
  )),
  method = "restyle",
  name = .x,
  value = .x
))
data <- transacciones %>%
  filter(cuenta_base %in% c("Gastos")) %>%
  mutate(cuenta = map_chr(lista_cuentas, ~ ifelse(length(.x) > 3, .x[length(.x) -
    3], .x[length(.x) - 2]))) %>%
  group_by(mes, cuenta) %>%
  summarise(valor = sum(valor))

fig <- plot_ly(
  data,
  labels = ~cuenta,
  values = ~valor,
  showlegend = FALSE
)

walk(meses, ~ {
  fig <<- fig %>%
    add_pie(
      data = data %>%
        filter(mes == .x),
      labels = ~cuenta,
      values = ~valor,
      # name = .x,
      visible = ini_mes == .x,
      textposition = "inside",
      textinfo = "valor+label+percent",
      insidetextfont = list(color = "#FFFFFF"),
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
    title = paste0("Gastos disgregados para el mes de ", ini_mes, collapse = ""),
    xaxis = list(
      showgrid = FALSE,
      zeroline = FALSE,
      showticklabels = FALSE
    ),
    yaxis = list(
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
fig

# Balance
transacciones %>%
  filter(cuenta_base %in% c("Activo", "Pasivo")) %>%
  mutate(nombre_completo = map_chr(lista_cuentas, ~ paste0(rev(unlist(
    .x
  )), collapse = ":"))) %>%
  group_by(nombre_completo) %>%
  summarise(valor = sum(valor))


# Viajes
transacciones %>%
  filter(nombre == "Viajes") %>%
  select(notas)

mapping_table <- list(
  "Viaje Vitoria 2021" = tolower(c("Viaje Vitoria")),
  "Viaje Navaluenga 2021" = tolower(c("Viaje Navaluenga 2021")),
  "Visita Madrid 2021" = tolower(c("Visita madrid 2021", "Viaje madrid 2021")),
  "Camping 2021" = tolower(c("Viaje camping 2021")),
  "Viaje Canarias 2021" = tolower(c("Viaje canarias 2021")),
  "Barcelona 2021" = tolower(c("Viaje barcelona 2021"))
)

# TODO: Modif directa en transaciones ?
data <- transacciones %>%
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

  fig <- plotly::plot_ly(
    showlegend = FALSE,
    orientation = "h"
  )

  purrr::walk(unique(data$notas), ~ {
    fig <<- fig %>%
      plotly::add_bars(
        data = data %>%
          filter(notas == .x),
        x = ~notas,
        y = ~valor,
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
      xaxis = list(
        title = ""
      ),
      yaxis = list(
        title = "Euros"
      )
    )
  fig %>%
    my_plotly_theme()



################################################
## title: Procesado de datos financieros
## author: Victor Vaquero Martinez
## date: 1 may 2021
################################################


rm(list = ls())

## ------------ Librerias ------------

# Manejo librerías
library(conflicted)

# Manejo paths
library(here)


# General
library(tidyverse)
library(lubridate)
# library(stringi)

# Lectura
library(xml2)

# Gráficos
library(plotly)
library(htmltidy)

# Conflictos
conflict_prefer("filter", "dplyr")
conflict_prefer("here", "here")
conflict_prefer("layout", "graphics")
conflict_prefer("lag", "dplyr")

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
ns <- xml2::xml_ns(f)

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
cuentas <- get_book_accounts(book)
cuentas

# Datos de transacciones
transacciones <- get_book_transactions(book, cuentas)


## ----------------------------------
## ------------ Análisis ------------
## ----------------------------------

my_font <- list(family = "Myriad Pro")

# Gastos - Ingresos por mes (lineas)
pal <- c("#e34a33", "#a1d99b")
plot_ly(
  data = transacciones %>%
    filter(cuenta_base %in% c("Ingresos", "Gastos")) %>%
    group_by(mes, cuenta_base) %>%
    summarise(valor = sum(valor)),
  x = ~mes,
  y = ~ abs(valor),
  color = ~cuenta_base,
  type = "scatter",
  mode = "line+dots",
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
  )

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

plot_ly(
  data = data %>% group_by(trace),
  x = ~fecha_transacion,
  y = ~valor,
  color = ~sign,
  colors = pal
) %>%
  add_lines() %>%
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
plot_ly(
  data = transacciones %>%
    filter(cuenta_base == "Gastos") %>%
    mutate(cuenta_segundo_nivel = map_chr(lista_cuentas, ~ .x[length(.x) -
      2])) %>%
    group_by(mes, cuenta_segundo_nivel) %>%
    summarise(valor = sum(valor)),
  x = ~mes,
  y = ~valor,
  color = ~cuenta_segundo_nivel,
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

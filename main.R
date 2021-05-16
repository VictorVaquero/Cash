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
## ------------ Variables ------------

files_directory <- here("data")
# file_name <- here("data", "20210425_204621_gnucash_export_Yo_transactions.csv")

## ---------------------------------
## ------------ Lectura ------------
## ---------------------------------

# Listado y metadatos
file_list <- tibble(path = list.files(files_directory, full.names = TRUE)) %>% 
  mutate(name = basename(path),
         parts = map(name, ~ {
           capture <- str_match(.x, "([0-9]{8})_([0-9]{6})_[^_]+_[^_]+_([^_]+)(?:_[^_]+)?\\.(\\w+)")
           tibble(last_date = capture[1, 2],
                  other = capture[1, 3],
                  account = capture[1, 4],
                  type = capture[1, 5])
         })) %>% 
  unnest(parts) %>% 
  mutate(last_date = ymd(last_date),
         account = factor(account))


# Lectura de datos (csv)
data <- file_list %>% 
  filter(type == "csv") %>% 
  mutate(data = map(path, ~ {
    read_delim(.x, delim = ",", skip = 1, locale = locale(decimal_mark = ",", grouping_mark = "."),
                   col_names = c("Date", "Transaction ID", "Number", "Description", "Notes", "Commodity/Currency", 
                                 "Void Reason", "Action", "Memo", "Full Account Name", "Account Name", "Amount With Sym.", 
                                 "Amount Num", "Reconcile", "Reconcile Date", "Rate/Price"),
                   col_types = cols(
                              Date = col_date(format = ""),
                              `Transaction ID` = col_character(),
                              Number = col_logical(),
                              Description = col_character(),
                              Notes = col_character(),
                              `Commodity/Currency` = col_character(),
                              `Void Reason` = col_logical(),
                              Action = col_logical(),
                              Memo = col_logical(),
                              `Full Account Name` = col_character(),
                              `Account Name` = col_character(),
                              `Amount With Sym.` = col_character(),
                              `Amount Num` = col_double(),
                              Reconcile = col_character(),
                              `Reconcile Date` = col_logical(),
                              `Rate/Price` = col_double()
    )) })) %>% 
  unnest(cols = c(data))

# Lectura de datos (xml)
fil <- file_list$path[4]
f <- read_xml(fil, encoding = "utf-8")
ns <- xml_ns(f)

books <- xml_find_all(f, ".//gnc:book")
book <- books[1]
xml_find_all(book, ".//book:id", ns) # ID
xml_find_all(book, ".//gnc:count-data", ns) # Basic numbers


         
## ------------------------------------------------------
## ------------ Limpieza y procesado inicial ------------
## ------------------------------------------------------

# Metadatos del libro
tribble(~ Dato, ~ Valor, ~ Descripcion,
        "Dinero", xml_find_all(book, './gnc:count-data[@cd:type="commodity"]') %>% xml_text(), "Numero de monedas usadas",
        "Cuentas", xml_find_all(book, './gnc:count-data[@cd:type="account"]') %>% xml_text(), "Numero de cuentas creadas",
        "Transacciones", xml_find_all(book, './gnc:count-data[@cd:type="transaction"]') %>% xml_text(), "Numero de movimientos"
)
# Metadastos de las divisas
xml_find_all(book, ".//gnc:commodity", ns) %>% 
  imap_dfr(~tibble(Id = .y,
                   ISO = xml_find_first(.x, "./cmdty:space", ns) %>% xml_text(),
                   Divisa = xml_find_first(.x, "./cmdty:id", ns) %>% xml_text()
  ))

# Metadatos de cuentas
# TODO: Es posible que la commodity no sea una divisa (caso de que tengas stock)
cuentas <- xml_find_all(book, ".//gnc:account", ns)  %>% 
  imap_dfr(~tibble(id = xml_find_first(.x, "./act:id", ns) %>% xml_text(),
                   nombre = xml_find_first(.x, "./act:name", ns) %>% xml_text(),
                   tipo = xml_find_first(.x, "./act:type", ns) %>% xml_text(),
                   descripcion = xml_find_first(.x, "./act:description", ns) %>% xml_text(),
                   divisa = xml_find_first(.x, "./act:commodity/cmdty:id", ns) %>% xml_text(),
                   minimo_valor_invertible = 1/(xml_find_first(.x, "./act:commodity-scu", ns) %>% xml_text() %>% as.numeric()),
                   padre = xml_find_first(.x, "./act:parent", ns) %>% xml_text(),
  ))

find_parents <- function(my_id) {
  c <- cuentas %>% 
    filter(id == my_id) 
  if(nrow(c) == 0) return(NA)
  if (c$nombre == "Root Account") return(c)
  return(c %>% bind_rows(find_parents(c$padre)))
}

# Datos de transacciones
# TODO: Que haces si hay más de 2 cuentas que intervengan en la transacción?
transacciones <- xml_find_all(book, "./gnc:transaction", ns) %>% 
  imap_dfr(~ {
    transacccion <- tibble(
      id_transaccion = xml_find_first(.x, "./trn:id", ns) %>% xml_text(),
      fecha_transacion = xml_find_first(.x, "./trn:date-posted/ts:date", ns) %>% xml_text() %>% ymd_hms(),
      fecha_creada = xml_find_first(.x, "./trn:date-entered/ts:date", ns) %>% xml_text() %>% ymd_hms(),
      descripcion = xml_find_first(.x, "./trn:description", ns) %>% xml_text(),
      notas = xml_find_first(.x, './trn:slots/slot/slot:key[text()="notes"]/following-sibling::slot:value') %>% xml_text(),
      num_splits = xml_find_all(.x, "./trn:splits", ns) %>% xml_children() %>% length()
    )
    splits <- xml_find_all(.x, "./trn:splits/trn:split", ns)
    cuentas_afectadas <- map_dfr(splits, ~
                                   tibble(
                                     id_split = xml_find_first(.x, "./split:id", ns) %>% xml_text(),
                                     reconciliada = xml_find_first(.x, "./split:reconciled-state", ns) %>% xml_text(),
                                     valor = xml_find_first(.x, "./split:value", ns) %>% xml_text(),
                                     cantidad = xml_find_first(.x, "./split:quantity", ns) %>% xml_text(),
                                     id_cuenta = xml_find_first(.x, "./split:account", ns) %>% xml_text()
                                   ))
    transacccion %>% 
      full_join(cuentas_afectadas, by = character())
  }) 

# Sacar cuentas basicas
transacciones <- transacciones %>% 
  mutate(lista_cuentas = map(id_cuenta, ~ {
    p <- find_parents(.x)
    p$nombre
  }),
  cuenta_base = map_chr(lista_cuentas, ~.x[length(.x)-1])
  ) 

# Calcular valores reales
transacciones <- transacciones %>% 
  mutate(valor = (Vectorize(function(.x) eval(parse(text=.x))))(valor),
         cantidad = (Vectorize(function(.x) eval(parse(text=.x))))(cantidad)) %>% 
  mutate(mes = month(fecha_transacion, label = TRUE),
         año = year(fecha_transacion))


## ----------------------------------
## ------------ Análisis ------------
## ----------------------------------

my_font <- list(
  family = "Myriad Pro"
)

# Gastos - Ingresos por mes (lineas)
pal <- c("#e34a33", "#a1d99b")
plot_ly(data = transacciones %>% 
  filter(cuenta_base %in% c("Ingresos", "Gastos")) %>% 
  group_by(mes, cuenta_base) %>% 
  summarise(valor = sum(valor)),
  x = ~mes, y = ~abs(valor), color = ~cuenta_base,
  type = "scatter", mode = "line+dots", colors = pal
) %>% 
  plotly::layout(title = 'Perdidas y ganancias',
                 font = my_font,
                 xaxis = list(title = "Mes"),
                 yaxis = list(title = "Euros",
                              rangemode = "tozero"))

# Movimiento total diario
plot_ly(data = transacciones %>% 
          filter(cuenta_base %in% c("Activo", "Pasivo")) %>% 
          arrange(fecha_transacion) %>% 
          mutate(valor = cumsum(valor)),
        x = ~fecha_transacion, y = ~valor, 
        type = "scatter", mode = "line+dots", colors = pal
) %>% 
  plotly::layout(title = 'Pasta',
                 font = my_font,
                 xaxis = list(title = ""),
                 yaxis = list(title = "Euros",
                              rangemode = "tozero"))


# Movimiento ingresos gastos diario (tendencia total)
plot_ly(data = transacciones %>% 
          filter(cuenta_base %in% c("Ingresos", "Gastos")) %>% 
          arrange(fecha_transacion) %>% 
          mutate(valor = cumsum(valor)),
        x = ~fecha_transacion, y = ~valor, 
        type = "scatter", mode = "line+dots", colors = pal
) %>% 
  plotly::layout(title = 'Pasta',
                 font = my_font,
                 xaxis = list(title = ""),
                 yaxis = list(title = "Euros",
                              rangemode = "tozero"))


# Desglose de gastos (barras)
plot_ly(data = transacciones %>% 
  filter(cuenta_base == "Gastos") %>% 
  mutate(cuenta_segundo_nivel = map_chr(lista_cuentas, ~.x[length(.x)-2])) %>% 
  group_by(mes, cuenta_segundo_nivel) %>% 
  summarise(valor = sum(valor)),
  x = ~mes, y = ~valor, color = ~cuenta_segundo_nivel,
  type = "bar"
  ) %>% 
  plotly::layout(barmode = "stack")



  

# Desglose comida ultimo més (tarta) o ultimos meses

ini_mes <- "abr"
meses <- levels(transacciones$mes)
steps <- imap(meses, ~
      list(args = list(visible = c(rep(FALSE, .y-1), TRUE, rep(FALSE, length(meses)-.y))),
           method = 'restyle',
           name = .x,
           value = .x
           ))
data <- transacciones %>% 
  filter(cuenta_base %in% c("Gastos")) %>% 
  mutate(cuenta = map_chr(lista_cuentas, ~ifelse(length(.x)>3, .x[length(.x)-3], .x[length(.x)-2]))) %>% 
  group_by(mes, cuenta) %>% 
  summarise(valor = sum(valor))

fig <- plot_ly(data, labels = ~cuenta, values = ~valor, 
               showlegend = FALSE)

walk(meses, ~ {
  fig <<- fig %>% 
    add_pie(data = data %>% 
                   filter(mes == .x), 
            labels = ~cuenta, values = ~valor,
            # name = .x,
            visible = ini_mes == .x,
            textposition = 'inside',
            textinfo = 'valor+label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1),
                          text = ~paste(cuenta, '\n', '$', valor, ' euros')),
            hoverinfo = 'text')
})

fig <- fig %>% 
  plotly::layout(title = paste0('Gastos disgregados para el mes de ', ini_mes, collapse = ""),
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 sliders = list(
                   list(
                     active = ini_mes, 
                     currentvalue = list(prefix = "Mes: "), 
                     steps = steps))
                 )
fig

# Balance
transacciones %>% 
  filter(cuenta_base %in% c("Activo", "Pasivo")) %>% 
  mutate(nombre_completo = map(unlist(lista_cuentas), ~paste0(.x, collapse = ":"))) %>% .$nombre_completo
  group_by(nombre_completo) %>% 
    summarise(valor = sum(valor))


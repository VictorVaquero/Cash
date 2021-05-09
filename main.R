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

# Gráficos
library(plotly)

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
           capture <- str_match(.x, "([0-9]{8})_([0-9]{6})_\\w+_\\w+_(\\w+)_\\w+\\.\\w+")
           tibble(last_date = capture[1, 2],
                  other = capture[1, 3],
                  account = capture[1, 4])
         })) %>% 
  unnest(parts) %>% 
  mutate(last_date = ymd(last_date),
         account = factor(account))


# Lectura de datos
data <- file_list %>% 
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



## ------------ Limpieza ------------

# TODO: ERROR si hay más de dos iguales -> slice(74,75)
# TODO: Automatizar checkeo de 4 iguales, son entradas repetidas
# Pares de movimientos
data <- data %>% 
  mutate(eq = if_else(`Amount Num` == -lead(`Amount Num`), 
                                           rep(TRUE, length(`Amount Num`)), 
                                           rep(FALSE, length(`Amount Num`))),
         gr = cumsum(eq),
         movement_tipe = c("TRUE" = "Primary", "FALSE" = "Secondary")[as.character(eq)])

# Aunar datos
data <- data %>% 
  select(-Date, -Description, -Notes, -`Commodity/Currency`, -`Transaction ID`) %>% 
  left_join(data %>% 
    group_by(gr) %>% 
    summarise(
      `Transaction ID` = `Transaction ID`[!is.na(`Transaction ID`)][1],
      Date = Date[!is.na(Date)][1],
      Description = Description[!is.na(Description)][1],
      Notes = Notes[!is.na(Notes)][1],
      `Commodity/Currency` = `Commodity/Currency`[!is.na(`Commodity/Currency`)][1]),
  by = "gr") %>% 
  mutate(transaction_id_augmented = paste(`Transaction ID`, movement_tipe, sep = "_"))

# Eliminar errores
# TODO: Faltan a veces datos (en prestamos falta uno de -50), otras veces sobran, en otros faltan cosas...
data <- data %>% 
  filter(`Transaction ID` != "53ad435926464f4ab04faeaedb968341")

# Buscamos y quitamos repetidos (en varios archivos diferentes)
data <- data %>% 
  group_by(transaction_id_augmented) %>% 
  summarise(across(everything(), ~ nth(.x, which(last_date == min(last_date)))))

# Hierarquia de cuentas
data <- data %>% 
  mutate(account_list = str_split(`Full Account Name`, ":"),
         base_account = map_chr(account_list, ~ .x[1]))

# Varibles útiles
data <- data %>% 
  mutate(year = year(Date),
         month = month(Date, label = TRUE))



## ----------------------------------
## ------------ Análisis ------------
## ----------------------------------

# Gastos - Ingresos por mes (lineas)
gg <- ggplot(data %>% 
  filter(base_account %in% c("Ingresos", "Gastos")) %>% 
  group_by(month, base_account) %>% 
  summarise(value = sum(`Amount Num`, na.rm = TRUE)),
  aes(month, abs(value), col = base_account, group = base_account)) +
  geom_line() +
  geom_point()
ggplotly(gg)


# Desglose de gastos (barras)
gg <- ggplot(data %>% 
  filter(base_account =="Gastos") %>% 
  mutate(second_level_account = map_chr(account_list, ~ .x[2])) %>% 
  group_by(month, second_level_account) %>% 
  summarise(value = sum(`Amount Num`)),
  aes(month, value, fill = second_level_account)) +
  geom_bar(stat = "identity")
ggplotly(gg)  

# Desglose comida ultimo més (tarta) o ultimos meses
# Interactivo...
fig <- plot_ly(data %>% 
                 filter(base_account =="Gastos", 
                        # month == max(month)
                        month > "mar"
                        ) %>% 
                 mutate(whatever_account = map_chr(account_list, ~ ifelse(!is.na(.x[3]), .x[3], .x[2]))) %>% 
                 group_by(month, whatever_account) %>% 
                 summarise(value = sum(`Amount Num`)), 
               labels = ~whatever_account, values = ~value, type = 'pie',
               textposition = 'inside',
               textinfo = 'value+label+percent',
               insidetextfont = list(color = '#FFFFFF'),
               hoverinfo = 'text',
               text = ~paste('$', value, ' euros'),
               marker = list(colors = colors,
                             line = list(color = '#FFFFFF', width = 1)),
               #The 'pull' attribute can also be used to create space between the sectors
               showlegend = FALSE)
fig <- fig %>% plotly::layout(title = paste0('Gastos disgregados para el mes de ', max(data$month), collapse = ""),
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
fig

# Balance
data %>% 
  filter(base_account %in% c("Activo", "Pasivo")) %>% 
  group_by(`Full Account Name`) %>% 
  summarise(`Account Name` = first(`Account Name`), 
            value = sum(`Amount Num`, na.rm = TRUE))

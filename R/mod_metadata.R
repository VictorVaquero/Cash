#' Metadata UI Function
#'
#' @description The metadata module, show common book info.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_metadata_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(class = "row",
             tags$div(class = "col",
                      customCard("Metadata",
                           DT::dataTableOutput(ns("my_book_metadata")))
             ),
             tags$div(class = "col mt-4 mt-sm-0",
                      customCard("Monedas",
                           DT::dataTableOutput(ns("my_currencies_metadata")))
             )
    ),

    tags$div(class = "row mt-4",
             tags$div(class = "col-md-8 offset-2 position-relative",
                      customCard("Cuentas",
                           DT::dataTableOutput(ns("my_book_accounts")))
             )
    )
  )
}

#' metadata Server Functions
#'
#' @param file_list tibble with all cash related data files available
#'
#' @return List with selected book (xml node) and accounts data (tibble)
#' @noRd
mod_metadata_server <- function(id, file_list){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    book <- reactive({
      message(".....get last book")
      get_last_book(file_list, "gnca")
    })
    accounts <- reactive({
      message(".....get accounts")
      get_book_accounts(book()) %>%
        get_parents_list()
    })

    output$my_book_metadata <- DT::renderDataTable({
      message(".....render book metadata")
      get_book_metadata(book()) %>%
        DT::datatable(data = ., options = list(dom = ""),
                      rownames= FALSE)
    })

    output$my_currencies_metadata <- DT::renderDataTable({
      message(".....render currencies metadata")
      get_book_currencies(book()) %>%
        DT::datatable(data = ., options = list(dom = ""),
                      rownames= FALSE)
    })

    output$my_book_accounts <- DT::renderDataTable({
      message(".....render accounts metadata")
      accounts() %>%
        ready_account_metadata(.) %>% # TODO: Hacer antes?
        DT::datatable(data = ., options = list(
          dom = "fp",
          columnDefs = list(list(className = 'dt-center', targets = 1:7)),
          pageLength = 5
          # deferRender = TRUE,
          # scrollY = 200,
          # scroller = TRUE),
        ),
        extensions = 'Responsive')
      # DT::formatStyle(columns = colnames(accounts()), fontSize = '50%')
    })

    return(list(
      book = book,
      accounts = accounts
    ))
  })
}

# Lectura de datos (xml)
get_last_book <- function(file_list, file_type) {
  latest_file <- get_latest_xml(file_list, file_type)
  f <- read_cash_xml(latest_file)

  books <- get_books(f)
  book <- books[1]
  book
}

# Preparar metadatos para visualizar
ready_account_metadata <- function(accounts) {
  accounts %>%
    dplyr::select(-cuenta_base) %>%
    dplyr::mutate(
      lista_cuentas = (Vectorize(function(.v) paste0(.v, collapse = " - "))(lista_cuentas))) %>% # TODO: Extra para DT)
    dplyr::rename(ID = id, Nombre = nombre, `Tipo de Cuenta` = tipo, `Descripción` = descripcion_cuenta,
                  Divisa = divisa, `Minimo valor inverible` = minimo_valor_invertible, `Cuenta padre` = padre,
                  `Listado de cuentas` = lista_cuentas)

}



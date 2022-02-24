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
             tags$div(class = "col-3",
                      customKPI("Fecha inicial",
                                shiny::textOutput(ns("initial_date_kpi"),
                                                     inline = TRUE),
                                color = "text-white"
                                )
                      ),
             tags$div(class = "col-3",
                      customKPI("Fecha final",
                                shiny::textOutput(ns("last_date_kpi"),
                                                  inline = TRUE),
                                color = "text-white"
                      )
             ),
             tags$div(class = "col-2",
                      customKPI("Monedas",
                                shiny::textOutput(ns("currencies_kpi"),
                                                  inline = TRUE),
                                color = "text-white"
                      )
             ),
             tags$div(class = "col-2",
                      customKPI("Cuentas",
                                shiny::textOutput(ns("accounts_kpi"),
                                                  inline = TRUE),
                                color = "text-white"
                      )
             ),
             tags$div(class = "col-2",
                      customKPI("Transacciones",
                                shiny::textOutput(ns("transactions_kpi"),
                                                  inline = TRUE),
                                color = "text-white"
                      )
             )
    ),
    tags$div(class = "row mt-4",
             tags$div(class = "col-md-6 position-relative",
                      customCard("Cuentas",
                           DT::dataTableOutput(ns("my_book_accounts")))
             ),
             tags$div(class = "col-md-6 position-relative",
                      customCard("Transacciones",
                                 DT::dataTableOutput(ns("my_book_transactions")))
             )
    ),
    tags$div(class = "row mt-5",
             tags$div(class = "col",

                                 shinyAce::aceEditor(
                                   outputId = ns("queryTransactions"),
                                   value = "book",
                                   mode = "r",
                                   theme = "tomorrow_night",
                                   placeholder = "Escriba la query deseada..."
                                 ),
                      shiny::actionButton(ns("evalQueryTransactions"), "Eval")
                      )
    ),
    tags$div(class = "row mt-2",
             tags$div(class = "col",
                      customCard("Output",
                                 shiny::htmlOutput(ns("my_book_transactions_code_output"))
                                 )
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

    ## ------ Reactive
    book <- reactive({
      message(".....get last book")
      get_last_book(file_list, "gnca")
    })
    accounts <- reactive({
      message(".....get accounts")
      get_book_accounts(book()) %>%
        get_parents_list()
    })

    transactions <- reactive({
      message(".....load transactions")
      get_book_transactions(book(), accounts())
    })

    ## ----- Logic

    ## ----- Visualizations

    output$initial_date_kpi <- shiny::renderText({
      message(".....render book metadata - ini date kpi")
      get_book_metadata(book()) %>%
        dplyr::filter(Dato == "Fecha ini") %>%
        dplyr::pull(Valor)
    })
    output$last_date_kpi <- shiny::renderText({
      message(".....render book metadata - last date kpi")
      get_book_metadata(book()) %>%
        dplyr::filter(Dato == "Fecha fin") %>%
        dplyr::pull(Valor)
    })
    output$currencies_kpi <- shiny::renderText({
      message(".....render book metadata - currencies kpi")
      get_book_currencies(book()) %>%
        dplyr::filter(Divisa != "template") %>%
        dplyr::pull(Divisa) %>%
        stringr::str_flatten(collapse = ", ")
    })
    output$accounts_kpi <- shiny::renderText({
      message(".....render book metadata - accounts kpi")
      get_book_metadata(book()) %>%
        dplyr::filter(Dato == "Cuentas") %>%
        dplyr::pull(Valor)
    })
    output$transactions_kpi <- shiny::renderText({
      message(".....render book metadata - transactions kpi")
      get_book_metadata(book()) %>%
        dplyr::filter(Dato == "Transacciones") %>%
        dplyr::pull(Valor)
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
        ready_account_metadata(.) %>%# TODO: Hacer antes?
        DT::datatable(data = ., options = list(
          dom = "fp",
          columnDefs = list(list(className = 'dt-center', targets = 1:7)),
          pageLength = 5
        ),
        extensions = 'Responsive')
      # DT::formatStyle(columns = colnames(accounts()), fontSize = '50%')
    })

    output$my_book_transactions <- DT::renderDataTable({
      message(".....render transactions list")
      transactions() %>%
        # ready_transactions_metadata(.) %>% # TODO: Hacer antes?
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

    # TODO: CLEAN
    output$my_book_transactions_code_output <- shiny::renderUI({
      # message(".....render code output")
      input$evalQueryTransactions
      result <- try(eval(parse(text = isolate(input$queryTransactions))))
      ansi2html <- function(ansi){
        HTML(sprintf(
          "<pre>%s</pre>",
          glue::glue_collapse(gsub("\n", "<br/>", as.character(fansi::sgr_to_html(ansi))), sep = "<br/>")
        ))
      }
      r<-ansi2html(capture.output(print(result)))
      message(r)
      r
    })

    observe({
      message(input$queryTransactions)
    })

    return(list(
      book = book,
      accounts = accounts,
      transactions = transactions
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



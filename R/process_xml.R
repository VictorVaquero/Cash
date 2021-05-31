#' Read xml cash file
#'
#' @param file
#'
#' @return tibble
#' @export
#'
#' @examples read_cash_xml(file)
read_cash_xml <- function(file) {
  f <- xml2::read_xml(file$path, encoding = "utf-8")
  # ns <- xml_ns(f)
  return(f)
}


#' Return books from file
#'
#' @param xml_root_node
#'
#' @return tibble
#' @export
#'
#' @examples get_books(xml)
get_books <- function(xml_root_node) {
  xml2::xml_find_all(xml_root_node, ".//gnc:book")
}

#' Get an specific book metadata
#'
#' @param xml_book
#'
#' @return
#' @export tibble
#'
#' @examples get_book_metadata(book)
get_book_metadata <- function(xml_book) {
  tribble(
    ~Dato,
    ~Valor,
    ~Descripcion,
    "Dinero",
    xml2::xml_find_all(xml_book, './gnc:count-data[@cd:type="commodity"]') %>% xml2::xml_text(.),
    "Numero de monedas usadas",
    "Cuentas",
    xml2::xml_find_all(xml_book, './gnc:count-data[@cd:type="account"]') %>% xml2::xml_text(.),
    "Numero de cuentas creadas",
    "Transacciones",
    xml2::xml_find_all(xml_book, './gnc:count-data[@cd:type="transaction"]') %>% xml2::xml_text(.),
    "Numero de movimientos"
  )
}

#' Get a list of the books currencies
#'
#' @param xml_book
#'
#' @return tibble
#' @export
#'
#' @import xml2
#'
#' @examples
get_book_currencies <- function(xml_book) {
  ns <- xml2::xml_ns(xml_book)

  xml_find_all(xml_book, ".//gnc:commodity", ns) %>%
    purrr::imap_dfr(
      ~ tibble(
        Id = .y,
        ISO = xml_find_first(.x, "./cmdty:space", ns) %>% xml_text(),
        Divisa = xml_find_first(.x, "./cmdty:id", ns) %>% xml_text()
      )
    )
}

#' Get a list of the book accounts
#'
#' @param xml_book
#'
#' @return tibble
#' @export
#'
#' @import xml2
#'
#' @examples
get_book_accounts <- function(xml_book) {
  ns <- xml2::xml_ns(xml_book)

  # TODO: Es posible que la commodity no sea una divisa (caso de que tengas stock)
  xml_find_all(xml_book, ".//gnc:account", ns) %>%
    purrr::imap_dfr(
      ~ tibble(
        id = xml_find_first(.x, "./act:id", ns) %>% xml_text(),
        nombre = xml_find_first(.x, "./act:name", ns) %>% xml_text(),
        tipo = xml_find_first(.x, "./act:type", ns) %>% xml_text(),
        descripcion = xml_find_first(.x, "./act:description", ns) %>% xml_text(),
        divisa = xml_find_first(.x, "./act:commodity/cmdty:id", ns) %>% xml_text(),
        minimo_valor_invertible = 1 / (
          xml_find_first(.x, "./act:commodity-scu", ns) %>% xml_text() %>% as.numeric()
        ),
        padre = xml_find_first(.x, "./act:parent", ns) %>% xml_text(),
      )
    )
}


#' Obtain accounts hierarchy
#'
#' @param my_id
#'
#' @return tibble
#'
#'
#' @examples
find_parents <- function(my_id, accounts) {
  c <- accounts %>%
    filter(id == my_id)
  if (nrow(c) == 0) {
    return(NA)
  }
  if (c$nombre == "Root Account") {
    return(c)
  }
  return(c %>% bind_rows(find_parents(c$padre, accounts)))
}


#' Get all of the account transactions
#'
#' @param xml_book
#'
#' @return tibble
#' @export
#'
#' @import xml2
#'
#' @examples
get_book_transactions <- function(xml_book, accounts) {
  ns <- xml2::xml_ns(xml_book)

  # TODO: Que haces si hay más de 2 cuentas que intervengan en la transacción?
  transacciones <- xml_find_all(xml_book, "./gnc:transaction", ns) %>%
    purrr::imap_dfr(~ {
      transacccion <- tibble(
        id_transaccion = xml_find_first(.x, "./trn:id", ns) %>% xml_text(),
        fecha_transacion = xml_find_first(.x, "./trn:date-posted/ts:date", ns) %>% xml_text() %>% lubridate::ymd_hms(),
        fecha_creada = xml_find_first(.x, "./trn:date-entered/ts:date", ns) %>% xml_text() %>% lubridate::ymd_hms(),
        descripcion = xml_find_first(.x, "./trn:description", ns) %>% xml_text(),
        notas = xml_find_first(
          .x,
          './trn:slots/slot/slot:key[text()="notes"]/following-sibling::slot:value'
        ) %>% xml_text(),
        num_splits = xml_find_all(.x, "./trn:splits", ns) %>% xml_children() %>% length()
      )
      splits <- xml_find_all(.x, "./trn:splits/trn:split", ns)
      cuentas_afectadas <- map_dfr(
        splits,
        ~
          tibble(
            id_split = xml_find_first(.x, "./split:id", ns) %>% xml_text(),
            reconciliada = xml_find_first(.x, "./split:reconciled-state", ns) %>% xml_text(),
            valor = xml_find_first(.x, "./split:value", ns) %>% xml_text(),
            cantidad = xml_find_first(.x, "./split:quantity", ns) %>% xml_text(),
            id_cuenta = xml_find_first(.x, "./split:account", ns) %>% xml_text()
          )
      )
      transacccion %>%
        full_join(cuentas_afectadas, by = character())
    })

  # Sacar cuentas basicas
  transacciones <- transacciones %>%
    mutate(
      lista_cuentas = purrr::map(id_cuenta, ~ {
        p <- find_parents(.x, accounts)
        p$nombre
      }),
      cuenta_base = purrr::map_chr(lista_cuentas, ~ .x[length(.x) - 1])
    )

  # Calcular valores reales
  transacciones <- transacciones %>%
    mutate(
      valor = (Vectorize(function(.x) {
        eval(parse(
          text = .x
        ))
      }))(valor),
      cantidad = (Vectorize(function(.x) {
        eval(parse(
          text = .x
        ))
      }))(cantidad)
    ) %>%
    mutate(
      mes = lubridate::month(fecha_transacion, label = TRUE),
      año = lubridate::year(fecha_transacion)
    )
}

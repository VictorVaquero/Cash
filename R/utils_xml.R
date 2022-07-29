#' @include utils_general.R

# Preparar cache
# TODO: Realmente es saltarme el uso de un bbdd
cache_memory <- cachem::cache_disk(logfile = stdout())


#' Get latest xml file
#'
#' @return tibble
#' @export
#'
#' @examples get_latest_xml()
get_latest_xml <- function(file_list, file_type) {
  # TODO: Ahora solo el último, checkear consistencia
  latest_file <- file_list %>%
    filter(type == file_type,
           is_valid) %>%
    filter(
      last_date == max(last_date)
    )
  return(latest_file)
}


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

# TODO: This should be in a normalized data format, not directly for visualization
#' Get an specific book metadata
#'
#' @param xml_book
#'
#' @return
#' @export tibble
#'
#' @examples get_book_metadata(book)
get_book_metadata <- log("get_book_metadata") %decor% function(xml_book) {
  tribble(
    ~Dato,    ~Valor,    ~Descripcion,

    "Fecha ini",
    xml2::xml_find_all(xml_book, ".//ts:date") %>%
      xml2::xml_text() %>%
      lubridate::ymd_hms() %>%
      min() %>% list(f=.) %>%
      glue::glue_data("{lubridate::day(f)} de {lubridate::month(f, label = TRUE, abbr = FALSE)} de {lubridate::year(f)}") %>%
      as.character(),
    "Primera fecha disponible",

    "Fecha fin",
    xml2::xml_find_all(xml_book, ".//ts:date") %>%
      xml2::xml_text() %>%
      lubridate::ymd_hms() %>%
      max() %>% list(f=.) %>%
      glue::glue_data("{lubridate::day(f)} de {lubridate::month(f, label = TRUE, abbr = FALSE)} de {lubridate::year(f)}") %>%
      as.character(),
    "Última fecha disponible",

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
get_book_accounts <- log("get_book_accounts") %decor% function(xml_book) {
  ns <- xml2::xml_ns(xml_book)

  # TODO: Es posible que la commodity no sea una divisa (caso de que tengas stock)
  # TODO: Estaria bien hacerlo de tal forma que no sean todas extricatamente necesarias
  accounts <- xml_find_all(xml_book, ".//gnc:account", ns) %>%
    purrr::imap_dfr(
      ~ dplyr::tibble(
        id = xml_find_first(.x, "./act:id", ns) %>% xml_text(),
        nombre = xml_find_first(.x, "./act:name", ns) %>% xml_text(),
        tipo = xml_find_first(.x, "./act:type", ns) %>% xml_text(),
        descripcion_cuenta = xml_find_first(.x, "./act:description", ns) %>% xml_text(),
        divisa = xml_find_first(.x, "./act:commodity/cmdty:id", ns) %>% xml_text(),
        minimo_valor_invertible = 1 / (
          xml_find_first(.x, "./act:commodity-scu", ns) %>% xml_text() %>% as.numeric()
        ),
        padre = xml_find_first(.x, "./act:parent", ns) %>% xml_text(),
      )
    )

  return(accounts)
}
get_book_accounts <- get_book_accounts %>%
  memoise::memoise(., cache = cache_memory,
                   hash = function(.x)
                     rlang::hash(xml2::xml_find_all(.x[[2]], ".//*[local-name() = 'id']") %>%
                                                     xml2::xml_text()))



#' Get parents and add their names
#'
#' @param accounts
#'
#' @return
#' @export
#'
#' @examples
get_parents_list <- function(accounts) {
  accounts %>%
    dplyr::mutate(
      lista_cuentas_ids = purrr::map(id, ~ {
        p <- find_parents(.x, accounts)
        if(any(!is.na(p))) return(p$id)
        else NA_character_
      }),
      # TODO: Mejorar esto
      lista_cuentas = purrr::map(id, ~ {
        p <- find_parents(.x, accounts)
        if(any(!is.na(p))) return(p$nombre)
        else NA_character_
      }),
      cuenta_base = purrr::map_chr(lista_cuentas, ~ .x[max(1,length(.x) - 1)])
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
  stopifnot(is.character(my_id))
  stopifnot("tbl" %in% class(accounts))

  c <- accounts %>%
    filter(id == my_id)
  if (nrow(c) == 0) {
    return(NA)
  }
  if (nrow(c) > 1) {
    c <- c %>% head(n = 1)
    message("...Warning, repeated accounts IDs, using first one")
  }
  if (c$tipo == "ROOT" | is.na(c$padre)) {
    return(c)
  }

  p <- find_parents(c$padre, accounts)

  if (identical(p, NA)) {
    message("...Warning, unknown parent ID")
    return(c)
  }

  return(c %>% bind_rows(p))
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
get_book_transactions <- log("get_book_transactions") %decor%  function(xml_book, accounts) {
  ns <- xml2::xml_ns(xml_book)

  # TODO: Que haces si hay más de 2 cuentas que intervengan en la transacción?
  transacciones <- xml_find_all(xml_book, "./gnc:transaction", ns) %>%
    purrr::imap_dfr(~ {
      transacccion <- tibble(
        id_transaccion = xml_find_first(.x, "./trn:id", ns) %>% xml_text(),
        fecha_transacion = xml_find_first(.x, "./trn:date-posted/ts:date", ns) %>% xml_text() %>% lubridate::ymd_hms(),
        fecha_creada = xml_find_first(.x, "./trn:date-entered/ts:date", ns) %>% xml_text() %>% lubridate::ymd_hms(),
        descripcion_transaccion = xml_find_first(.x, "./trn:description", ns) %>% xml_text(),
        notas = xml_find_first(
          .x,
          './trn:slots/slot/slot:key[text()="notes"]/following-sibling::slot:value'
        ) %>% xml_text(),
        num_splits = xml_find_all(.x, "./trn:splits", ns) %>% xml_children() %>% length()
      )
      splits <- xml_find_all(.x, "./trn:splits/trn:split", ns)
      cuentas_afectadas <- purrr::map_dfr(
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
  # TODO: Arregla calculando padres previo cuentas, es estupido sino
  # transacciones <- transacciones %>%
  #   mutate(
  #     lista_cuentas = purrr::map(id_cuenta, ~ {
  #       p <- find_parents(.x, accounts)
  #       p$nombre
  #     }),
  #     cuenta_base = purrr::map_chr(lista_cuentas, ~ .x[length(.x) - 1])
  #   )
  transacciones <- transacciones %>%
    left_join(accounts, by = c("id_cuenta" = "id"))

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
      mes = lubridate::month(fecha_transacion, label = TRUE, abbr = FALSE),
      ano = lubridate::year(fecha_transacion)
    )

  return(transacciones)
}
# get_book_transactions <- get_book_transactions %>%
#   memoise::memoise(., cache = cache_memory,
#                    hash = function(.x)
#                      rlang::hash(
#                      list(
#                       xml2::xml_find_all(.x$xml_book, ".//*[local-name() = 'id']") %>%
#                                      xml2::xml_text(),
#                        .x$accounts
#                      )))




#' Lectura de datos (csv)
#'
#' @param file_list
#'
#' @return tibble
#' @export
#' @import readr
#' @examples read_cash_csv(file_list)
read_cash_csv <- function(file_list) {
  file_list %>%
    mutate(data = purrr::map(path, ~ {
      readr::read_delim(
        .x,
        delim = ",",
        skip = 1,
        locale = readr::locale(decimal_mark = ",", grouping_mark = "."),
        col_names = c(
          "Date",
          "Transaction ID",
          "Number",
          "Description",
          "Notes",
          "Commodity/Currency",
          "Void Reason",
          "Action",
          "Memo",
          "Full Account Name",
          "Account Name",
          "Amount With Sym.",
          "Amount Num",
          "Reconcile",
          "Reconcile Date",
          "Rate/Price"
        ),
        col_types = readr::cols(
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
        )
      )
    })) %>%
    unnest(cols = c(data))
}

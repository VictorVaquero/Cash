
#' Saca días hasta el final del mes
#'
#' @param .d
#'
#' @return
#' @export
#'
#' @examples
days_remainder <- function(.d) {
  as.numeric(lubridate::days_in_month(.d) - lubridate::mday(.d))
}


#' Descarga de telegram el ultimo archivo de pasta
#'
#' @return
#' @export
#'
#' @examples
download_latest_cash_file <- function() {
  system("python3 ./python/download_gnu_files.py")
}

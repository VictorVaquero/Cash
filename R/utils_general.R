VISUALIZATION_HEIGHT <- 325 # TODO: MOVE THIS
LOGGING_DIR <- "./log"
LOGGING_FILE <- "cash_app.log"

get_lhs <- function(){
  calls <- sys.calls()

  call_firsts <- lapply(calls,`[[`,1)
  pipe_calls <- vapply(call_firsts,identical,logical(1),quote(`%>%`))
  if(all(!pipe_calls)){
    NULL
  } else {
    pipe_calls <- which(pipe_calls)
    pipe_calls <- pipe_calls[length(pipe_calls)]
    calls[[c(pipe_calls,2)]]
  }
}

#' Logging decorator
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
log <- function(.text) {
  name_func <- .text
  function(.f) {
    decorated_func <- function(...) {
      logger::log_info("INIT {name_func}")
      return_value <- .f(...)
      logger::log_info("END {name_func}")
      return(return_value)
    }
  }
}

`%decor%` <- function(decorator, func) decorator(func)

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
